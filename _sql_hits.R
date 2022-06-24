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

process_one <- data %>%
  mutate(surgery_date_dt = date(surgery_date_dt)) %>%
  filter(surgery_date_dt > "2022-01-01") %>%
  select(c(1,2,5,15,18,27,28,30,32,34,35,36,38,40,42,43,44,51,52,53,55,56,57,58,59,83,84,85,86,88,89,90,91,92,93,108,109,128,129,130,131,132,133,137,138,139,140,141,142,143,150:164))
