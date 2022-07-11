# load libraries
shelf(odbc, DBI, tidyverse, lubridate)

con <- DBI::dbConnect(odbc::odbc(),
                      Driver   = "SQL Server",
                      Server   = "wwldwsql1",
                      Database = "nhs_reporting",
                      UID      = "",
                      PWD      = "",
                      Port     = 1433)
system.time(data_theatre <- dbReadTable(con, "reporting_theatres"))

# doParallel::registerDoParallel(cores = 3)

process_one <- data_theatre %>%
  mutate(surgery_date_dt = date(surgery_date_dt)) %>%
  filter(surgery_date_dt > "2022-01-01" & surgery_date_dt < "2022-06-17") %>%
  filter(site_description == "Wrightington Hospital") %>%
  filter(operation_usage == "Operation Completed") %>%
  select(c(1,2,5,15,18,27,28,30,32,34,35,36,37,38,39,40,42,43,44,51,52,53,55,
           56,57,58,59,83,84,85,86,88,89,90,91,92,93,108,109,128,129,130,131,
           132,133,137,138,139,140,141,142,143,150:164))
exammple <- process_one %>%
  head(1000) %>%
  select(c(1,6:12,14)) %>%
  write_rds("figure_1.rds")
df_datetime_only <- process_one %>%
  select(c(1,6:12,14,16:19,53:54,57:59,66:67)) %>%
  # mutate(check = if_else(sent_for == porter_time, TRUE, FALSE)) %>%
  # mutate(check = if_else(into_theatre == anaesthetic_handover, TRUE, FALSE))
  # mutate(check = if_else(closure == operation_end_time, TRUE, FALSE)) %>%
  mutate(across(c(2:13), hm)) %>%
  mutate_at(vars(c(2:13)), function(x) hour(x)*60 + minute(x)) %>%
  mutate(sent_porter = pmin(sent_for, porter_time, na.rm = T)) %>%
  select(c(1,21,4:6,8:9,11:20,-sent_for,-porter_time,-closure,-anaesthetic_handover))

time_difference <- rowid_to_column(df_datetime_only, "group_id") %>%
  select(1:9) %>%
  drop_na(.) %>%
  pivot_longer(!c(1:2), names_to = "time_diff", values_to = "mins") %>%
  group_by(group_id) %>%
  mutate(diff = mins - lag(mins)) %>%
  select(-mins) %>%
  mutate(time_diff = replace(time_diff, time_diff == "in_time", "intime - sentfor"),
         time_diff = replace(time_diff, time_diff == "anaesthetic_start_time", "ana_st - intime"),
         time_diff = replace(time_diff, time_diff == "into_theatre", "in_theatre - ana_st"),
         time_diff = replace(time_diff, time_diff == "operation_end_time", "op_end - in_theatre"),
         time_diff = replace(time_diff, time_diff == "recovery_time", "into_recovery - op_end"),
         time_diff = replace(time_diff, time_diff == "out_of_recovery", "out_recov - recovery")) %>%
  filter(!(time_diff == "sent_porter" | time_diff == "op_end - in_theatre" | time_diff == "out_recov - recovery")) %>%
  mutate(time_diff = as_factor(time_diff))
time_difference %>% 
  filter(time_difference, diff > 0) %>%
  ggplot(time_difference, aes(time_diff, diff)) +
  geom_boxplot()


stats <- time_difference %>%
  group_by(time_diff) %>%
  summarise(x = quantile(diff, c(0.25, 0.5, 0.75)), q = c(0.25, 0.5, 0.75))
  
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