# load libraries
shelf(odbc, DBI, tidyverse, lubridate, here)

con <- DBI::dbConnect(odbc::odbc(),
                      Driver   = "SQL Server",
                      Server   = "wwldwsql1",
                      Database = "nhs_reporting",
                      UID      = "",
                      PWD      = "",
                      Port     = 1433)
system.time(data_theatre <- dbReadTable(con, "reporting_theatres"))

# doParallel::registerDoParallel(cores = 3)

# random_sample_of_representative_prop_each_year
# work out total count of each year then work out proportion reasonable to take
# also count and prop of time_loss_delay each year

process_one <- data_theatre %>%
  mutate(surgery_date_dt = date(surgery_date_dt)) %>%
  # filter(surgery_date_dt > "2022-01-01" & surgery_date_dt < "2022-06-17") %>%
  filter(site_description == "Wrightington Hospital") %>%
  filter(operation_usage == "Operation Completed") %>%
  select(c(1,2,5,15,18,27,28,30,32,34,35,36,37,38,39,40,42,43,44,47,51,52,53,55,
           56,57,58,59,83,84,85,86,88,89,90,91,92,93,108,109,128,129,130,131,
           132,133,137,138,139,140,141,142,143,150:164)) %>%
  mutate(year = year(surgery_date_dt),
         time_loss_mins = overrun_mins - delayed_start_mins,
         time_loss_logic = ifelse(time_loss_mins > 0, 1,0))

year_count <- process_one %>%
  group_by(year, time_loss_logic) %>%
  summarise(n = n()) %>%
  mutate(rel.freq = paste0(round(100 * n/sum(n), 0), "%"))


