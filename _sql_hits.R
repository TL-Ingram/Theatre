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

doParallel::registerDoParallel(cores = 3)

process_one <- data_theatre %>%
  mutate(surgery_date_dt = date(surgery_date_dt)) %>%
  filter(surgery_date_dt > "2022-01-01" & surgery_date_dt < "2022-06-17") %>%
  filter(site_description == "Wrightington Hospital") %>%
  filter(operation_usage == "Operation Completed") %>%
  select(c(1,2,5,15,18,27,28,30,32,34,35,36,37,38,39,40,42,43,44,51,52,53,55,56,57,
           58,59,83,84,85,86,88,89,90,91,92,93,108,109,128,129,130,131,132,
           133,137,138,139,140,141,142,143,150:164))
df_dt_only <- process_one %>%
  select(c(1,6:19,46:51,53,64,66:67)) %>%
  select(c(1:15,22,24,25)) %>%
  select(c(1,11,9,2:8,10,12:18)) %>%
  select(c(1:8,14,9,12,10:11,13,15:18))
# check if columns are actually duplicated 
# time difference in between each stage
# actual operation time may not matter, although could group by type?