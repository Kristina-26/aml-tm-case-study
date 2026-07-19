library(dplyr)
library(ggplot2)
library(scales)
library(tidyr)

output_dir <- "../output"
fig_dir    <- "../figures"
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

alerts <- readRDS(file.path(output_dir, "alerts_clean.rds"))

theme_set(
  theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 13),
      plot.subtitle = element_text(color = "grey35", size = 10.5),
      panel.grid.minor = element_blank(),
      legend.position = "bottom"
    )
)

pal_type <- c("Private Banking" = "#2E5C8A", "LC&FI" = "#C77B2C")

save_plot <- function(p, name, w = 8, h = 5) {
  ggsave(file.path(fig_dir, paste0(name, ".png")), p, width = w, height = h, dpi = 200, bg = "white")
}

## F1. Alert volume by scenario (AlertType), split by Type

f1_data <- alerts %>% count(Type, AlertType)
alert_order <- f1_data %>% group_by(AlertType) %>% summarise(n = sum(n)) %>% arrange(n) %>% pull(AlertType)
f1_data$AlertType <- factor(f1_data$AlertType, levels = alert_order)

p1 <- ggplot(f1_data, aes(x = AlertType, y = n, fill = Type)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = pal_type) +
  scale_y_continuous(labels = comma) +
  labs(title = "Alert volume by TM scenario",
       subtitle = "Scenario mix differs materially between Private Banking and LC&FI",
       x = NULL, y = "Number of alerts", fill = NULL)
save_plot(p1, "F1_volume_by_scenario", h = 6)

## F2. Monthly alert volume over time

f2_data <- alerts %>%
  count(yearmon_created, Type)

p2 <- ggplot(f2_data, aes(x = yearmon_created, y = n, color = Type)) +
  geom_line(linewidth = 0.6) +
  scale_color_manual(values = pal_type) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(title = "Monthly alert volume over time (2008-2021)",
       subtitle = "Volumes are volatile; several step-changes suggest scenario/threshold or system changes",
       x = NULL, y = "Alerts generated per month", color = NULL)
save_plot(p2, "F2_monthly_trend")


## F3. TM funnel: Alert -> Escalated to case -> SAR filed ------------------

funnel <- alerts %>%
  group_by(Type) %>%
  summarise(Alerts = n(), `Escalated to case` = sum(escalated_to_case), `SAR filed` = sum(sar_filed)) %>%
  pivot_longer(-Type, names_to = "stage", values_to = "n") %>%
  mutate(stage = factor(stage, levels = c("Alerts", "Escalated to case", "SAR filed")))

p3 <- ggplot(funnel, aes(x = stage, y = n, fill = Type)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = comma(n)), position = position_dodge(width = 0.9), vjust = -0.4, size = 3.2) +
  scale_y_log10(labels = comma) +
  scale_fill_manual(values = pal_type) +
  labs(title = "TM alert funnel: Alert -> Case -> SAR (log scale)",
       subtitle = "The vast majority of alerts are closed at 1st line without escalation",
       x = NULL, y = "Count (log scale)", fill = NULL)
save_plot(p3, "F3_funnel")

## F4. Scenario "yield": escalation rate vs SAR rate, by AlertType

library(ggrepel)

f4_data <- alerts %>%
  group_by(Type, AlertType) %>%
  summarise(n_alerts = n(), escalation_rate = mean(escalated_to_case),
            sar_rate = mean(sar_filed), .groups = "drop") %>%
  filter(n_alerts >= 20)

p4 <- ggplot(f4_data, aes(x = escalation_rate, y = sar_rate, size = n_alerts, color = Type)) +
  geom_point(alpha = 0.75) +
  geom_text_repel(aes(label = AlertType), size = 3, color = "grey20",
                   max.overlaps = Inf, seed = 1, show.legend = FALSE,
                   min.segment.length = 0, segment.size = 0.3,
                   box.padding = 0.4, point.padding = 0.3) +
  scale_x_continuous(labels = percent) +
  scale_y_continuous(labels = percent) +
  scale_color_manual(values = pal_type) +
  scale_size_continuous(range = c(2, 12), labels = comma) +
  labs(title = "Scenario productivity: escalation rate vs SAR (yield) rate",
       subtitle = "Point size = alert volume. Scenarios in the top-right are the most productive",
       x = "Escalation rate (Alert -> Case)", y = "SAR rate (Alert -> SAR)",
       color = NULL, size = "Alert volume")
save_plot(p4, "F4_scenario_yield_scatter", w = 11, h = 8)

## F5. SAR rate by Customer Risk Category (risk-based discrimination)

f5_data <- alerts %>%
  filter(CusRiskCategory_clean != "Unknown / not recorded") %>%
  group_by(CusRiskCategory_clean) %>%
  summarise(n_alerts = n(), sar_rate = mean(sar_filed), escalation_rate = mean(escalated_to_case)) %>%
  mutate(CusRiskCategory_clean = factor(CusRiskCategory_clean,
         levels = c("Lower Risk", "Medium Risk", "Higher Risk", "Not Specified")))

p5 <- ggplot(f5_data, aes(x = CusRiskCategory_clean, y = sar_rate)) +
  geom_col(fill = "#2E5C8A") +
  geom_text(aes(label = percent(sar_rate, accuracy = 0.1)), vjust = -0.4, size = 3.3) +
  scale_y_continuous(labels = percent, expand = expansion(mult = c(0, 0.15))) +
  labs(title = "SAR rate by customer risk category",
       subtitle = "Higher-risk customers show a materially higher SAR conversion rate,\nsupporting the risk-based calibration of the model",
       x = NULL, y = "SAR rate (share of alerts leading to a SAR)")
save_plot(p5, "F5_sar_rate_by_risk_category")

## F6. Alert disposition (AlertState) composition

f6_data <- alerts %>%
  mutate(AlertState_grp = case_when(
    grepl("^Closed - Not", AlertState) ~ AlertState,
    grepl("^Closed", AlertState) ~ AlertState,
    TRUE ~ "Open / in progress"
  )) %>%
  count(Type, AlertState_grp) %>%
  group_by(Type) %>% mutate(pct = n / sum(n)) %>% ungroup()

p6 <- ggplot(f6_data, aes(x = Type, y = pct, fill = AlertState_grp)) +
  geom_col() +
  geom_text(aes(label = ifelse(pct > 0.03, percent(pct, accuracy = 1), "")),
            position = position_stack(vjust = 0.5), size = 3, color = "white") +
  scale_y_continuous(labels = percent) +
  labs(title = "1st-line alert disposition (AlertState) by customer type",
       x = NULL, y = "Share of alerts", fill = NULL) +
  guides(fill = guide_legend(ncol = 2))
save_plot(p6, "F6_alertstate_composition")

## F7. Cycle time distribution: days to close an alert

f7_data <- alerts %>% filter(!is.na(days_to_alert_close), days_to_alert_close >= 0, days_to_alert_close < 200)

p7 <- ggplot(f7_data, aes(x = days_to_alert_close, fill = Type)) +
  geom_histogram(binwidth = 5, alpha = 0.75, position = "identity") +
  facet_wrap(~Type, ncol = 1, scales = "free_y") +
  scale_fill_manual(values = pal_type) +
  labs(title = "Time to close an alert at 1st line (days)",
       subtitle = "Truncated at 200 days for readability",
       x = "Days from DateCreated to DateClosed", y = "Number of alerts", fill = NULL) +
  theme(legend.position = "none")
save_plot(p7, "F7_cycle_time_alert_close")

## F8. Escalation and SAR rate trend over years

f8_data <- alerts %>%
  filter(year_created >= 2013) %>%   # early years too sparse to be meaningful
  group_by(year_created) %>%
  summarise(n_alerts = n(), escalation_rate = mean(escalated_to_case), sar_rate = mean(sar_filed)) %>%
  pivot_longer(c(escalation_rate, sar_rate), names_to = "metric", values_to = "rate") %>%
  mutate(metric = recode(metric, escalation_rate = "Escalation rate (Alert->Case)",
                          sar_rate = "SAR rate (Alert->SAR)"))

p8 <- ggplot(f8_data, aes(x = year_created, y = rate, color = metric)) +
  geom_line(linewidth = 0.8) + geom_point(size = 1.8) +
  scale_y_continuous(labels = percent) +
  scale_x_continuous(breaks = 2013:2021) +
  labs(title = "TM model productivity over time (2013-2021)",
       subtitle = "Escalation and SAR rates fluctuate year to year with no clear stable trend",
       x = NULL, y = "Rate", color = NULL)
save_plot(p8, "F8_yield_trend")

cat("Saved", length(list.files(fig_dir, pattern = "png$")), "figures to", fig_dir, "\n")

