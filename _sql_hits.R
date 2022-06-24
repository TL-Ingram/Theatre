library(odbc)
library(DBI)

con <- DBI::dbConnect(odbc::odbc(),
                      Driver   = "SQL Server",
                      Server   = "wwldwsql1",
                      Database = "nhs_reporting",
                      UID      = "",
                      PWD      = "",
                      Port     = 1433)
system.time(data_theatre <- dbReadTable(con, "reporting_theatres"))

doParallel::registerDoParallel(cores = 3)

