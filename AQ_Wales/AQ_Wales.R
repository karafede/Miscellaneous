

library(openair)
library(importr)
library(dplyr)
library(plyr)
library(tidyr)
library(lubridate)
library(ggplot2)

setwd("C:/AQ_Wales")

# Helpers
print_database_names()
print_statistic_types()


# Cardiff Centre = "CARD"
# Cwmbran = "CWMB"
# Newport Malpas Depot  = "NPT2"
# Newport = "Newport St Julians Comp School"
# Port Talbot = "PT"
# Port Talbot Margam = "PT4"
# Swansea Morriston Roadside = "SWA5" 
# Wrexham = "WREX"
# Aston Hill = "AH"
# Marchlyn Mawr = "MAWR"
# Narberth = "PEMB"
# Rhondda-Cynon-Taf Nantgarw = "RHD2"

info_sites <- search_database("wales", extra = TRUE)    ### metadata
info_filter <- info_sites %>%
  filter(site_name == "Swansea Morriston Roadside") 


codes <- c("CARD", "CWMB", "NPT2", "NPT4", "NPT1", "PT", "PT4", "SWA5", "WREX", "AH","MAWR", "PEMB", "RHD2" )
pollutant <- c("no2","o3", "ge10") 

# make sure they are in order
codes <- sort(codes)

data <- import_measures("wales", site = codes, 
                        variable = c("no2","o3", "ge10", "gr10"), 
                        start = 2015, end = 2015)



data <- import_measures("wales", site = "SWA5", 
                        variable = c("ge10"), 
                        start = 2015, end = 2015)



# data_PM10_CWMB <- na.omit(data_PM10_CWMB)  # remove row with NA values
# mean_PM10_CWMB <- mean(data_PM10_CWMB$pm10)


stats_wales <- import_stats("wales", site = codes, 
                                    variable = c("no2","o3", "ge10", "gr10"),
                                    start = 2015, 
                                    end = 2015,
                                    statistic = "annual_mean")

####--------------- update the spreadsheets for NO2, O3, and PM10------------------------------------##########

###############################################################################################################
###############################################################################################################
################################## PLOTS ######################################################################

data <- read.csv("welshNO2_PM10_Ozone_Trends.csv")

data <- data %>%
  mutate(date = dmy(date, tz = "UTC"),
         year = year(date))

# select data only from year > 1992
data <- data[ data$year > 1992, ]

NO2_data <- data %>%
  select(year,
         Cardiff_Centre_NO2,
         Mean_4_long_running_urban_background_sites_NO2,
         Swansea_Morriston_Roadside_NO2,
         Mean_3_long_running_traffic_sites_NO2)

library(reshape2)
# Specify id.vars: the variables to keep but not split apart on
NO2_data <- melt(NO2_data, id.vars=c("year"))


NO2_data %>% 
  ggplot(aes(year, value, color = variable)) +
   theme_bw() +
  geom_vline(xintercept = 1993:2015, linetype = "longdash", colour="gray") +
  theme(strip.background = element_rect(colour="white", fill="white"),
         panel.border = element_rect(fill = "NA", color = "black")) +
  geom_point(size = 4) + 
  ylim(c(0,45)) + 
  ylab(expression(paste(NO[2], " (µg/",m^3, ")"))) + 
  theme(axis.title.y = element_text(face="bold", colour="black", size=12),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=13)) +
  
  geom_line() + geom_smooth() +
  theme(axis.title.x = element_blank()) +  # Remove x-axis label
  theme(legend.position="bottom", legend.direction="vertical",
        legend.background = element_rect(color = "NA", 
                                         fill = "white", size = 1, linetype = "solid"))+
  theme(legend.title=element_blank()) 




