
# library(RPostgreSQL)
library(RMySQL)
library(dplyr)
library(threadr)

setwd("C:/UK_PRTR")
# create a connection
# save the password that we can "hide" it as best as we can by collapsing it

pw <- {
  "viewonly"
}

# loads the MSQL driver
drv <- dbDriver("MySQL")

# creates a connection to the postgres database
# note that "con" will be used later in each connection to the database
con = dbConnect(MySQL(), user='webuser', password='viewonly', dbname='prtr', host='hornbeam')

dbListTables(con)

time <- Sys.time()
year <- str_sub(as.character(time), start = 1, end = -16)

dbExistsTable(con, "facility")
dbExistsTable(con, "release_transfer_type")
# TRUE

# get data from the Data Base
# rs <- dbSendQuery(con, "SELECT * FROM facility") # all available data
rs <- dbSendQuery(con, "SELECT * FROM facility limit 100") 
# rs <- dbSendQuery(con, "SELECT * FROM facility WHERE year >= '2008' AND year < '2010'")
# rs <- dbSendQuery(con, "SELECT * FROM facility WHERE year BETWEEN '2008' AND '2010'")  

AAA <- fetch(rs,n=-1)  # convert to a data.frame
str(AAA)
AAA

write.csv(AAA, file= "Facility_data.csv")

dbDisconnect(con)
