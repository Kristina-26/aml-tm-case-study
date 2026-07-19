library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)

data_dir   <- "../data"
output_dir <- "../output"

## 1. Loading raw data

# Both blank cells and text "NULL" are treated as missing (NA) on import so
# that the logic is consistent.
alerts_raw <- read_excel(file.path(data_dir, "alerts.xlsx"), sheet = "Sheet1",
                          na = c("", "NULL", "NA", "N/A", "#N/A"))

metadata   <- read_excel(file.path(data_dir, "additional_info.xlsx"),
  sheet = "Metadata")
industry   <- read_excel(file.path(data_dir, "additional_info.xlsx"),
  sheet = "Industry Segment")

# Quantify how many cells contained the literal text "NULL" in the true raw,
# just for reporting purposes.
alerts_asis <- suppressMessages(read_excel(file.path(data_dir, "alerts.xlsx"),
  sheet = "Sheet1"))
n_null_literal <- sum(sapply(alerts_asis, function(x) sum(x == "NULL",
  na.rm = TRUE)))
cat(sprintf("Literal 'NULL' text strings found across all fields (raw file):
  %d cells\n", n_null_literal))
sapply(alerts_asis, function(x) sum(x == "NULL", na.rm = TRUE))
rm(alerts_asis)

cat("Raw alerts dimensions:", dim(alerts_raw), "\n")
cat("Metadata fields:\n"); print(metadata)
cat("Industry segment table:", nrow(industry), "rows,",
    n_distinct(industry$`Industry Code`), "unique industry codes\n")

## 2. Initial data quality diagnostics

# 2a. The first column ("...1"/Unnamed: 0) duplicates intID but is not identical
#     to it -> it looks like an internal row/export index, not a business key.
names(alerts_raw)[1] <- "row_index"

# 2b. intID uniqueness
n_rows      <- nrow(alerts_raw)
n_unique_id <- n_distinct(alerts_raw$intID)
cat(sprintf("\nRows: %d | Unique intID values: %d (%.1f%% are duplicated IDs)\n",
            n_rows, n_unique_id, 100 * (1 - n_unique_id / n_rows)))

# Check whether intID collisions occur *within* the same customer Type (pb/lcfi)
# or *across* types (which would indicate intID is only unique per source system)
dup_id_tbl <- alerts_raw %>%
  group_by(intID) %>%
  filter(n() > 1) %>%
  summarise(n_types = n_distinct(Type), n = n(), .groups = "drop")

cat(sprintf("intID collisions across DIFFERENT Type (pb vs lcfi): %d groups\n",
            sum(dup_id_tbl$n_types > 1)))
cat(sprintf("intID collisions WITHIN the same Type: %d groups\n",
            sum(dup_id_tbl$n_types == 1)))

# 2c. Exact duplicate records (same content, ignoring the row_index/export
# column)
exact_dupes <- alerts_raw %>% select(-row_index)

# Group by every field to find how many times each unique record occurs.
dup_group_sizes <- exact_dupes %>%
  group_by(across(everything())) %>%
  summarise(n_occurrences = n(), .groups = "drop") %>%
  filter(n_occurrences > 1)

n_exact_dupe_rows   <- sum(dup_group_sizes$n_occurrences)
n_exact_dupe_groups <- nrow(dup_group_sizes)
max_group_size      <- max(dup_group_sizes$n_occurrences, default = 0)

cat(sprintf("Fully duplicated alert records (all fields identical, excl. row
  index): %d rows across %d distinct records\n",
            n_exact_dupe_rows, n_exact_dupe_groups))
cat("Distribution of duplicate group sizes (how many times each record
  repeats):\n")
print(table(dup_group_sizes$n_occurrences))
if (max_group_size > 2) {
  cat(sprintf("NOTE: at least one record appears %d times, not just 2 -
    dedup will remove all but one copy regardless.\n",
              max_group_size))
}

## 3. Deduplication

## We remove rows that are complete duplicates of another row on every business
## field (AlertType, AlertState, all dates, PEP, risk category, Type, industry).
## intID is NOT used as the dedup key since it is not a reliable unique
## identifier (see diagnostics above) - it appears to be assigned independently
## within each source system (Private Banking vs LC&FI) and collides when the
## two extracts are combined.

alerts_dedup <- alerts_raw %>%
  distinct(across(-row_index), .keep_all = TRUE)

cat(sprintf("\nRows before dedup: %d | after dedup: %d | removed: %d\n",
            nrow(alerts_raw), nrow(alerts_dedup), nrow(alerts_raw) -
    nrow(alerts_dedup)))

## 4. Data type conversion and derived fields

alerts <- alerts_dedup %>%
  mutate(
    DateCreated  = suppressWarnings(as_date(DateCreated)),
    DateClosed   = suppressWarnings(as_date(DateClosed)),
    CaseOpen     = suppressWarnings(as_datetime(CaseOpen)),
    CaseClosed   = suppressWarnings(as_datetime(CaseClosed)),
    CaseReported = suppressWarnings(as_datetime(CaseReported)),


    Type = recode(Type, "pb" = "Private Banking", "lcfi" = "LC&FI"),

    # Structural NAs vs. genuine missingness:
    #  - PEP is only captured for Private Banking clients (individuals)
    #  - IndustryCode is only captured for LC&FI clients (legal entities)
    PEP_clean = case_when(
      Type == "LC&FI" ~ "Not applicable (LC&FI)",
      is.na(PEP)       ~ "Unknown / not recorded",
      PEP == "Y"        ~ "PEP",
      PEP == "N"        ~ "Not PEP",
      TRUE               ~ PEP
    ),

    CusRiskCategory_clean = case_when(
      is.na(CusRiskCategory) ~ "Unknown / not recorded",
      TRUE ~ CusRiskCategory
    ),

    # Alert-lifecycle flags -----------------------------------------------
    escalated_to_case = !is.na(CaseOpen),                # 1st line -> 2nd line
    sar_filed          = !is.na(CaseReported),            # SAR sent to FIU
    case_state_clean   = ifelse(is.na(CaseState), "No case opened", CaseState),

    # Cycle times (days) ----------------------------------------------------
    days_to_alert_close   = as.numeric(difftime(DateClosed, DateCreated,
      units = "days")),
    days_case_investig    = as.numeric(difftime(CaseClosed, CaseOpen,
      units = "days")),
    days_close_to_report  = as.numeric(difftime(CaseReported, CaseClosed,
      units = "days")),

    year_created  = year(DateCreated),
    yearmon_created = floor_date(DateCreated, "month")
  )

## 5. Join industry risk segment

industry <- industry %>%
  rename(IndustryCode = `Industry Code`, IndustryRiskScore = `Risk Score`,
         IndustrySegment = Segment)

alerts <- alerts %>%
  left_join(industry, by = "IndustryCode") %>%
  mutate(
    IndustrySegment_clean = case_when(
      Type == "Private Banking" ~ "Not applicable (PB)",
      !is.na(IndustrySegment)    ~ IndustrySegment,
      TRUE                        ~ "Other / not in high-risk industry list"
    )
  )

## 6. Sanity checks

stopifnot(all(alerts$days_to_alert_close   >= 0 |
    is.na(alerts$days_to_alert_close)))
stopifnot(nrow(alerts) == nrow(alerts_dedup))

cat("\nFinal analysis-ready dataset:", nrow(alerts), "rows,", ncol(alerts),
  "columns\n")

## 7. Export
saveRDS(alerts, file.path(output_dir, "alerts_clean.rds"))
write.csv(alerts, file.path(output_dir, "alerts_clean.csv"), row.names = FALSE)

cat("\nSaved cleaned dataset to output/alerts_clean.[rds|csv]\n")

