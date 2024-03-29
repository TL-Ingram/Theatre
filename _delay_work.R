#exploring the delay

df_datetime_only <- process_one %>%
  select(c(1,6:12,14,16:20,40:41,53:54,57:59,67:71)) %>%
  # mutate(check = if_else(sent_for == porter_time, TRUE, FALSE)) %>%
  # mutate(check = if_else(into_theatre == anaesthetic_handover, TRUE, FALSE))
  # mutate(check = if_else(closure == operation_end_time, TRUE, FALSE)) %>%
  mutate(across(c(2:13), hm)) %>%
  mutate_at(vars(c(2:13)), function(x) hour(x)*60 + minute(x)) %>%
  mutate(sent_porter = pmin(sent_for, porter_time, na.rm = T)) %>%
  select(c(1,27,4:6,8:9,11:26,-sent_for,-porter_time,-closure,-anaesthetic_handover))

time_difference <- df_datetime_only %>%
  rowid_to_column(., "id") %>%
  select(c(1:10,20:22)) %>%
  drop_na(.) %>%
  pivot_longer(!c(1:2,10:13), names_to = "time_diff", values_to = "mins") %>%
  group_by(id) %>%
  mutate(diff = mins - lag(mins)) %>%
  select(-mins) %>%
  mutate(time_diff = replace(time_diff, time_diff == "in_time", "FROM patient sent for TO patient into pre op"),
         time_diff = replace(time_diff, time_diff == "anaesthetic_start_time", "FROM patient entering pre-op TO anaesthetic start"),
         time_diff = replace(time_diff, time_diff == "into_theatre", "FROM anaesthetic start TO patient into theatre"),
         time_diff = replace(time_diff, time_diff == "operation_end_time", "FROM patient into theatre TO operation end"),
         time_diff = replace(time_diff, time_diff == "recovery_time", "FROM operation end TO patient into recovery"),
         time_diff = replace(time_diff, time_diff == "out_of_recovery", "out_recovery - recovery_start")) %>%
  filter(!(time_diff == "sent_porter" | time_diff == "FROM patient into theatre TO operation end" | time_diff == "out_recovery - recovery_start")) %>%
  mutate(time_diff = as_factor(time_diff),
         diff = ifelse(time_diff == "FROM patient sent for TO patient into pre op" & diff < 0, NA, diff),
         diff = ifelse(time_diff == "FROM anaesthetic start TO patient into theatre" & diff < 0, NA, diff),
         diff = ifelse(time_diff == "FROM operation end TO patient into recovery" & diff < 0, NA, diff),
         time_loss_logic = as_factor(time_loss_logic)) %>%
  pivot_wider(names_from = "time_diff", values_from = "diff") %>%
  drop_na(.) %>%
  pivot_longer(!c(1:6), names_to = "time_diff", values_to = "diff") %>%
  mutate(time_diff = as_factor(time_diff)) #%>%
time_difference <- time_difference %>%
  left_join(outlier_summary, by = c("time_diff", "time_loss_logic")) %>%
  mutate(diff = ifelse(diff > eIQR, NA, diff)) %>%
  select(-c(9:14)) %>%
  pivot_wider(names_from = "time_diff", values_from = "diff") %>%
  drop_na(.) %>%
  pivot_longer(!c(1:6), names_to = "time_diff", values_to = "diff") %>%
  mutate(time_diff = as_factor(time_diff)) #%>%

#need to do more filtering of outliers. Can't trust all data - could simply remove all outliers that are > or < 3x IQR.
# outlier_summary <- time_difference %>%
#   group_by(time_diff, time_loss_logic) %>%
#   summarise(median = median(diff, na.rm = TRUE),
#             q25 = quantile(diff, 0.25, na.rm = TRUE),
#             q75 = quantile(diff, 0.75, na.rm = TRUE),
#             max = max(diff),
#             IQR = q75 - q25,
#             eIQR = 3*IQR) #%>%
# write_csv(here("tables", "outlier_summary.csv"))

#anything above eIQR in diff column should be made NA. Then pivot wider, drop_na, pivot longer so we're back where we were and then plot the graph below again


yearly_loss_by_stage <- time_difference %>%
  ggplot(aes(x = year, y = diff, group = time_loss_logic, colour = time_loss_logic)) +
  geom_point(stat = "summary", fun = median, alpha = 0.7) +
  stat_summary(fun = median, geom = "line", size = 2.25, alpha = 0.7) +
  facet_grid(. ~ time_diff, labeller = label_wrap_gen(width=28)) +
  scale_colour_manual(values = c("steelblue", "red")) +
  expand_limits(y = 0:30) +
  scale_y_continuous(breaks = seq(0, 30, 5)) +
  scale_x_continuous(breaks = seq(2010, 2022, 2)) +
  theme_ipsum(
    axis_title_just = "cc",
    axis_title_face = "bold",
    axis_text_size = 24,
    axis_title_size = 27
  ) +
  theme(
    panel.grid.minor = element_line("grey30"),
    panel.grid.major = element_line("grey30"),
    axis.line.x = element_line("grey50"),
    axis.ticks = element_line(colour = "grey50", size = 0.2),
    axis.ticks.x = element_line(colour = "grey50", size = 0.2),
    axis.text.x = element_text(
      angle = 35,
      vjust = 1.0, hjust = 1.0,
    ),
    plot.title = element_text(size = 34),
    plot.caption = element_text(
      size = 21,
      face = "italic", color = "black"
    ),
    legend.title = element_text(size = 24, face = "bold"),
    legend.text = element_text(size = 24),
    strip.text.x = element_text(
      size = 21, color = "black"
    ),
  ) +
  labs(
    x = "Year",
    y = "Time (minutes)",
    colour = "Allowed time exceeded",
    title = "Yearly median time taken between theatre stages"
  )
ggsave(here("plots", "yearly_time_loss.jpg"), width = 25, height = 20) 
yearly_loss_by_stage

distribution <- time_difference %>%
  ggplot(aes(diff, fill = time_diff)) +
  geom_density(alpha = 0.25, size = 1) +
  #facet_wrap(.~year) +
  xlim(0,100) +
  xlab("Time (minutes)") +
  theme_ipsum(
    axis_title_just = "cc",
    axis_title_face = "bold",
    axis_text_size = 16,
    axis_title_size = 18)
ggsave(here("plots", "dist_time_differences_outliers_removed.jpg"), width = 15, height = 15)
distribution


# DONE table of differences
# DONE graph should have median and quartiles shaded
# DONE more outlier removal required
# table of minute differences between delayed and not delayed
# facet by theatre
# facet by type of surgery - how many types?
# make it so it's like, per surgery median time taken is ??. For those that are delayed on average the only diff is [insert stage]
# do something for last financial year

# na_count <- time_diff %>% #18% sent_porter == NA. Going to remove all rows containing NAs.
#   group_by(time_diff) %>%
#   summarise(n = n(),
#             sum = sum(is.na(mins)),
#             prop = sum/n)


# count of delays, or proportion? Delay minutes total? How to do that to make it representative for each year. Maybe range?
# forecast next year delays
# facet by theatre
# do something about the reasons

# DONE column with delayed = 1, not delayed = 2
# do two things: 1 = delayed mins can be used for forecasting per year
# DONE 2 = only look at time differences between stages for delayed = 1
# DONE then I suppose can compare times on stages between delayed = 1 and delayed = 0





# DONE - check if columns are actually duplicated
# DONE - removed either sent_for or porter_time. Too many NAs. Therefore saved only whichever time was earlier.
# time difference in between each stage
# actual operation time may not matter, although could group by type?
# does the after surgery recovery time even matter for this work?
# look at proportion of late in 2022, then group by reason category? Language processing part.
# schedule times wrong? Standard
# Difference between planned and actual procedure codes. Why so many procedure codes
# grouping by type of surgery, ordering the time of day for particular surgery types?