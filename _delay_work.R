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
  mutate(time_diff = replace(time_diff, time_diff == "in_time", "intime - sentfor"),
         time_diff = replace(time_diff, time_diff == "anaesthetic_start_time", "ana_st - intime"),
         time_diff = replace(time_diff, time_diff == "into_theatre", "in_theatre - ana_st"),
         time_diff = replace(time_diff, time_diff == "operation_end_time", "op_end - in_theatre"),
         time_diff = replace(time_diff, time_diff == "recovery_time", "into_recovery - op_end"),
         time_diff = replace(time_diff, time_diff == "out_of_recovery", "out_recov - recovery")) %>%
  filter(!(time_diff == "sent_porter" | time_diff == "op_end - in_theatre" | time_diff == "out_recov - recovery")) %>%
  mutate(time_diff = as_factor(time_diff),
         diff = ifelse(time_diff == "intime - sentfor" & diff < 0, NA, diff),
         diff = ifelse(time_diff == "in_theatre - ana_st" & diff < 0, NA, diff),
         diff = ifelse(time_diff == "into_recovery - op_end" & diff < 0, NA, diff),
         time_loss_logic = as_factor(time_loss_logic))

more <- time_difference %>%
  pivot_wider(names_from = "time_diff", values_from = "diff") %>%
  drop_na(.) %>%
  pivot_longer(!c(1:6), names_to = "time_diff", values_to = "diff")

#need to do more filtering of outliers. Can't trust all data - could simply remove all outliers that are > or < 3x IQR.

test3 <- more %>%
  ggplot(aes(x = year, y = diff, group = time_loss_logic, colour = time_loss_logic)) +
  geom_point(stat = "summary", fun = median) +
  stat_summary(fun = median, geom = "line") +
  facet_grid(. ~ time_diff)
  ggsave(here("plots", "yearly_time_loss.png"), width = 10, height = 10)
test3
?stat_summary


  
  

# table of differences
# graph should have median and quartiles shaded
# appropriate dt naming


# na_count <- time_diff %>% #18% sent_porter == NA. Going to remove all rows containing NAs.
#   group_by(time_diff) %>%
#   summarise(n = n(),
#             sum = sum(is.na(mins)),
#             prop = sum/n)


# create df of time differences
# create representative random sample of each year
# graph
# forecast next year delays
# facet by theatre

#column with delayed = 1, not delayed = 2
#do two things: 1 = delayed mins can be used for forecasting per year
#2 = only look at time differences between stages for delayed = 1
#then I suppose can compare times on stages between delayed = 1 and delayed = 0


#next minus subsequent columns from each other
#plot
#group by only those that have reasons for delay in start or finish then look at reasons. Do individually for start then finish



# DONE - check if columns are actually duplicated
# DONE - removed either sent_for or porter_time. Too many NAs. Therefore saved only whichever time was earlier.
# time difference in between each stage
# actual operation time may not matter, although could group by type?
# does the after surgery recovery time even matter for this work?
# look at proportion of late in 2022, then group by reason category? Language processing part.
# schedule times wrong? Standard
# Difference between planned and actual procedure codes. Why so many procedure codes
# grouping by type of surgery, ordering the time of day for particular surgery types?