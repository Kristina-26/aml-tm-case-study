library(dplyr)
library(tidyr)
library(readr)

output_dir <- "../output"
alerts <- readRDS(file.path(output_dir, "alerts_clean.rds"))

fmt_pct <- function(x) sprintf("%.2f%%", 100 * x)

## T1. Data completeness by field

fields <- c("intID","AlertType","AlertState","DateCreated","DateClosed","CaseOpen",
            "CaseClosed","CaseReported","CaseState","PEP","CusRiskCategory","Type","IndustryCode")

t1 <- tibble(
  Field = fields,
  N_missing = sapply(fields, function(f) sum(is.na(alerts[[f]]))),
  Pct_missing = sapply(fields, function(f) round(100 * mean(is.na(alerts[[f]])), 1))
)
write_csv(t1, file.path(output_dir, "T1_completeness.csv"))
print(t1)

## T2. Volume by customer type and AlertType

t2 <- alerts %>%
  count(Type, AlertType, name = "n_alerts") %>%
  arrange(Type, desc(n_alerts))
write_csv(t2, file.path(output_dir, "T2_volume_by_type_alerttype.csv"))
print(t2)

## T3. Alert funnel overview: Alert -> Case -> SAR

t3 <- alerts %>%
  group_by(Type) %>%
  summarise(
    n_alerts = n(),
    n_escalated = sum(escalated_to_case),
    escalation_rate = mean(escalated_to_case),
    n_sar = sum(sar_filed),
    sar_rate_of_alerts = mean(sar_filed),
    sar_rate_of_escalated = ifelse(n_escalated > 0, n_sar / n_escalated, NA),
    .groups = "drop"
  ) %>%
  bind_rows(
    alerts %>% summarise(
      Type = "Overall",
      n_alerts = n(),
      n_escalated = sum(escalated_to_case),
      escalation_rate = mean(escalated_to_case),
      n_sar = sum(sar_filed),
      sar_rate_of_alerts = mean(sar_filed),
      sar_rate_of_escalated = n_sar / n_escalated
    )
  )
write_csv(t3, file.path(output_dir, "T3_funnel_by_type.csv"))
print(t3)

## T4. Funnel performance ("yield") by AlertType/scenario

t4 <- alerts %>%
  group_by(Type, AlertType) %>%
  summarise(
    n_alerts = n(),
    n_escalated = sum(escalated_to_case),
    escalation_rate = mean(escalated_to_case),
    n_sar = sum(sar_filed),
    sar_rate_of_alerts = mean(sar_filed),
    .groups = "drop"
  ) %>%
  arrange(Type, desc(n_alerts))
write_csv(t4, file.path(output_dir, "T4_scenario_yield.csv"))
print(t4, n = 30)

## T5. AlertState (1st line disposition) distribution

t5 <- alerts %>%
  count(Type, AlertState, name = "n") %>%
  group_by(Type) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  arrange(Type, desc(n)) %>%
  ungroup()
write_csv(t5, file.path(output_dir, "T5_alertstate_distribution.csv"))
print(t5)

## T6. Escalation / SAR rate by Customer Risk Category

t6 <- alerts %>%
  group_by(CusRiskCategory_clean) %>%
  summarise(
    n_alerts = n(),
    escalation_rate = mean(escalated_to_case),
    sar_rate = mean(sar_filed),
    .groups = "drop"
  ) %>%
  arrange(desc(n_alerts))
write_csv(t6, file.path(output_dir, "T6_by_risk_category.csv"))
print(t6)

## T7. Escalation / SAR rate by PEP status (Private Banking only)

t7 <- alerts %>%
  filter(Type == "Private Banking") %>%
  group_by(PEP_clean) %>%
  summarise(
    n_alerts = n(),
    escalation_rate = mean(escalated_to_case),
    sar_rate = mean(sar_filed),
    .groups = "drop"
  )
write_csv(t7, file.path(output_dir, "T7_by_pep.csv"))
print(t7)

## T8. Escalation / SAR rate by Industry Segment (LC&FI only)

t8 <- alerts %>%
  filter(Type == "LC&FI") %>%
  group_by(IndustrySegment_clean) %>%
  summarise(
    n_alerts = n(),
    escalation_rate = mean(escalated_to_case),
    sar_rate = mean(sar_filed),
    .groups = "drop"
  ) %>%
  arrange(desc(n_alerts))
write_csv(t8, file.path(output_dir, "T8_by_industry_segment.csv"))
print(t8)

## T9. Cycle-time summary statistics

t9 <- alerts %>%
  summarise(
    median_days_to_alert_close = median(days_to_alert_close, na.rm = TRUE),
    mean_days_to_alert_close   = round(mean(days_to_alert_close, na.rm = TRUE), 1),
    p90_days_to_alert_close    = quantile(days_to_alert_close, 0.9, na.rm = TRUE),
    median_days_case_investig  = median(days_case_investig, na.rm = TRUE),
    mean_days_case_investig    = round(mean(days_case_investig, na.rm = TRUE), 1),
    median_days_close_to_report = median(days_close_to_report, na.rm = TRUE),
    mean_days_close_to_report  = round(mean(days_close_to_report, na.rm = TRUE), 1),
    n_negative_cycle_times = sum(days_to_alert_close < 0, na.rm = TRUE) +
                              sum(days_case_investig < 0, na.rm = TRUE)
  )
write_csv(t9, file.path(output_dir, "T9_cycle_times.csv"))
print(t9)

## T10. Yearly alert volume & yield trend

t10 <- alerts %>%
  group_by(year_created) %>%
  summarise(
    n_alerts = n(),
    escalation_rate = mean(escalated_to_case),
    sar_rate = mean(sar_filed),
    .groups = "drop"
  )
write_csv(t10, file.path(output_dir, "T10_yearly_trend.csv"))
print(t10, n = 20)

cat("\nAll summary tables written to output/ as CSV.\n")
