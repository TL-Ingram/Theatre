library(odbc)
library(DBI)
library(tidyverse)
library(lubridate)

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

df_dt_only <- process_one %>%
  select(c(1,6:12,14,16:19,53:54,57:59,66:67)) %>%
  # mutate(check = if_else(sent_for == porter_time, TRUE, FALSE)) %>%
  # mutate(check = if_else(into_theatre == anaesthetic_handover, TRUE, FALSE))
  # mutate(check = if_else(closure == operation_end_time, TRUE, FALSE)) %>%
  mutate(across(c(2:13), hm)) %>%
  mutate_at(vars(c(2:13)), function(x) hour(x)*60 + minute(x)) %>%
  mutate(sent_porter = pmin(sent_for, porter_time, na.rm = T)) %>%
  select(c(1,21,4:6,8:9,11:20,-sent_for,-porter_time,-closure,-anaesthetic_handover))
#next minus subsequent columns from each other
#plot

#group by only those that have reasons for delay in start or finish then look at reasons. Do individually for start then finish
  


# DONE - check if columns are actually duplicated
# DONE - removed either sent_for or porter_time. Too many NAs. Therefore saved only whichever time was earlier.
# time difference in between each stage
# actual operation time may not matter, although could group by type?
# does the after surgery recovery time even matter for this work?
# look at proportion of late in 2022, then group by reason category? Language processing part.