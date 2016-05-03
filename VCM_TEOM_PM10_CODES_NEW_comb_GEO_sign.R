 
################ VCM TEOM DATA PROCESSING by SITE CODE ######################### 

########### Change path for new diectory !!!!!! ##############################
##############################################################################

setwd("C:/TEOM_VCM")

library(openair)
library(plyr)
library(reshape2)
library(zoo)
library(lattice)
library(latticeExtra)
library(hexbin)
library(ggplot2)
library(gridExtra)
library(sqldf)
library(rworldmap)
library(ggmap)



#############                                  ####################################
##############  Load HERE NEW TOTAL INPUT DATA ####################################
#############                                  ####################################

UK_AIR_FDMS <- read.csv("C:/TEOM_VCM/UK_AIR/TEOM_FDMS/UK_AIR_INPUT_FDMS.csv",
                        header = TRUE)    #### PM10 from UK AIR
UK_AIR_INPUT_V10 <- read.csv("C:/TEOM_VCM/UK_AIR/Volatile_PM10/UK_AIR_INPUT_V10.csv",
                        header = TRUE)    #### Volatile PM10 from UK AIR (V10)
AQ_ENG_INPUT_STD <- read.csv("C:/TEOM_VCM/AQENGLAND/TEOM_STD/AQ_ENG_INPUT_STD.csv",
                      header = TRUE)   #### PM10 from AQ ENGLAND
AQ_ENG_INPUT_V10 <- read.csv("C:/TEOM_VCM/AQENGLAND/Volatile_PM10/AQ_ENG_INPUT_V10.csv",
                      header = TRUE)    #### Volatile PM10 from AQ ENGLAND (V10)


###### Combine UK AIR and AQ ENGLAND datasets to look for PM10 ########################

setwd("C:/TEOM_VCM")

DATASET_10Dec2014_PM10 <- cbind(UK_AIR_FDMS,AQ_ENG_INPUT_STD)

for (i in seq(1, by = 2, len = length(DATASET_10Dec2014_PM10)/2)) {
  colnames(DATASET_10Dec2014_PM10)[i+1] = "type_TEOM"
}

type_TEOM_PM10 <-DATASET_10Dec2014_PM10[2:3,]
type_TEOM_PM10 <- as.data.frame(t(type_TEOM_PM10))
colnames(type_TEOM_PM10) <- c("instr", "instr_a") 
rows.to.keep_PM10 <-which(rownames(type_TEOM_PM10) %in% "type_TEOM") 
type_TEOM_PM10 <- type_TEOM_PM10[rows.to.keep_PM10,] 


date_10Dec2014 <- as.data.frame(seq(from=as.POSIXct("2014-12-10 01:00:00", tz="GMT"), 
                                    to=as.POSIXct("2014-12-11 24:00:00", tz="GMT"), by="hour"))

colnames(date_10Dec2014) <- "date"
DATASET_10Dec2014_PM10 <- cbind(date_10Dec2014,
                                DATASET_10Dec2014_PM10[2:nrow(DATASET_10Dec2014_PM10),])

write.csv(DATASET_10Dec2014_PM10, file = "DATASET_10Dec2014_PM10.csv", row.names=FALSE)


###### Combine UK AIR and AQ ENGLAND datasets for V10 ##########################

DATASET_10Dec2014_V10 <- cbind(UK_AIR_INPUT_V10, AQ_ENG_INPUT_V10)

for (i in seq(1, by = 2, len = length(DATASET_10Dec2014_V10)/2)) {
  colnames(DATASET_10Dec2014_V10)[i+1] = "type_TEOM"
}

type_TEOM_V10 <-DATASET_10Dec2014_V10[2:3,]
type_TEOM_V10 <- as.data.frame(t(type_TEOM_V10))
colnames(type_TEOM_V10) <- c("instr", "instr_a") 
rows.to.keep_V10 <-which(rownames(type_TEOM_V10) %in% "type_TEOM") 
type_TEOM_V10 <- type_TEOM_V10[rows.to.keep_V10,] 

DATASET_10Dec2014_V10 <- cbind(date_10Dec2014,
                               DATASET_10Dec2014_V10[2:nrow(DATASET_10Dec2014_V10),])

write.csv(DATASET_10Dec2014_V10, file = "DATASET_10Dec2014_V10.csv", row.names=FALSE)



###################################################################################
###################################################################################
############### Site List for DATASET #############################################

####### data set for PM10 (UK-AIR and AQ ENG) ######################################

data_PM10 <-  DATASET_10Dec2014_PM10[,!(names(DATASET_10Dec2014_PM10) %in% "type_TEOM")] ##"type TEOM" columns

SITES_PM10 <- as.data.frame(colnames(data_PM10[-1]))
SITES_PM10 <- as.data.frame(gsub(".", " ", SITES_PM10[,1], fixed = TRUE))
colnames(SITES_PM10) <- "Site.Name"
names_data_PM10 <- t(SITES_PM10)
names_data_PM10 <- cbind("date",names_data_PM10)
names_data_PM10 <- as.character(names_data_PM10)
colnames(data_PM10) <- names_data_PM10

write.csv(data_PM10, file = "DATA_ONLY_PM10_10Dec2014.csv", row.names=FALSE)

###### data set for V10 (UK-AIR and AQ ENG) ######################################

data_V10 <-  DATASET_10Dec2014_V10[,!(names(DATASET_10Dec2014_V10) %in% "type_TEOM")] ##"type TEOM" columns

SITES_V10 <- as.data.frame(colnames(data_V10[-1]))
SITES_V10 <- as.data.frame(gsub(".", " ", SITES_V10[,1], fixed = TRUE))
colnames(SITES_V10) <- "Site.Name"
names_data_V10 <- t(SITES_V10)
names_data_V10 <- cbind("date",names_data_V10)
names_data_V10 <- as.character(names_data_V10)
colnames(data_V10) <- names_data_V10

write.csv(data_V10, file = "DATA_ONLY_V10_10Dec2014.csv", row.names=FALSE)



########### Import UK AIR METADATA ###################################################

UK_SITES_META <- read.csv("AURN_sites_metadata_20140327_15-28.csv",
                          header = TRUE)

########### Import AQ ENGLAND METADATA ###############################################

AQ_ENG_SITES_META <- read.csv("aqengland_AQE_md_metadata_20140709_09-48.csv",
                              header = TRUE)


########## Merge meta data for UK AIR and AQ ENGLAND SITES ############################

UK_SITES_META <- UK_SITES_META[,1:(length(UK_SITES_META)-3)]
AQ_ENG_SITES_META <- AQ_ENG_SITES_META[,1:(length(AQ_ENG_SITES_META)-3)]

UK_SITES_META <- cbind(UK_SITES_META[,2:7],UK_SITES_META[11],UK_SITES_META[14])
AQ_ENG_SITES_META <- AQ_ENG_SITES_META[,1:8]

META <- rbind(UK_SITES_META,AQ_ENG_SITES_META)

write.csv(META, file = "META_ALL.csv", row.names=FALSE)


#######################  PM10 from UK AIR and AQ ENG ################################
###########################         PM10        ####################################

lookup_PM10 <- unique(META)  ### sort out PM10 names only once

META_10Dec2014_PM10 <- join(SITES_PM10, lookup_PM10, by = "Site.Name")
META_10Dec2014_PM10 <- cbind(META_10Dec2014_PM10,type_TEOM_PM10[2])

write.csv(META_10Dec2014_PM10, file = "META_10Dec2014_PM10.csv", row.names=FALSE)



#######################  PM10 from UK AIR and AQ ENG ################################
###########################         V10        ####################################

lookup_V10 <- unique(META)  ### sort out V10 names only once

META_10Dec2014_V10 <- join(SITES_V10, lookup_V10, by = "Site.Name")
META_10Dec2014_V10 <- cbind(META_10Dec2014_V10,type_TEOM_V10[2])

write.csv(META_10Dec2014_V10, file = "META_10Dec2014_V10.csv", row.names=FALSE)



######## Replace NA codes in META data PM10 and V10 #######################

dat3 <- as.matrix(META_10Dec2014_PM10[2])
Q <- which(is.na(META_10Dec2014_PM10[2])==TRUE)  
dat3[Q] <- paste("MISSING",Q, sep="")  
META_10Dec2014_PM10[2] <-  as.data.frame(dat3)

write.csv(META_10Dec2014_PM10, file = "META_10Dec2014_PM10.csv", row.names=FALSE)

dat4 <- as.matrix(META_10Dec2014_V10[2])
P <- which(is.na(META_10Dec2014_V10[2])==TRUE)  
dat4[P] <- paste("MISSING",P, sep="")  
META_10Dec2014_V10[2] <-  as.data.frame(dat4)

write.csv(META_10Dec2014_V10, file = "META_10Dec2014_V10.csv", row.names=FALSE)


##### NEW columns names for data....with site CODES!!!! #######
#################  PM10  #####################################

CODES_PM10 <- as.data.frame(META_10Dec2014_PM10[,2])
colnames(CODES_PM10) <- "Site.Code"
code_names_data_PM10 <- t(CODES_PM10)
code_names_data_PM10 <- cbind("date",code_names_data_PM10)
code_names_data_PM10 <- as.character(code_names_data_PM10)
colnames(data_PM10) <- code_names_data_PM10

##### NEW columns names for data....with site CODES!!!! #######
#################  V10  #####################################

CODES_V10 <- as.data.frame(META_10Dec2014_V10[,2])
colnames(CODES_V10) <- "Site.Code"
code_names_data_V10 <- t(CODES_V10)
code_names_data_V10 <- cbind("date",code_names_data_V10)
code_names_data_V10 <- as.character(code_names_data_V10)
colnames(data_V10) <- code_names_data_V10


#################################################################################
######### Merge Sites within 130 Km distance  ###################################

##### use V10 data from UK-AIR and AQ ENG ####################################
TEOM_FDMS_a <- META_10Dec2014_V10[ which(META_10Dec2014_V10$instr_a =='N ugm-3 (TEOM FDMS)'), ]
TEOM_FDMS_b <- META_10Dec2014_V10[ which(META_10Dec2014_V10$instr_a =='P ugm-3 (TEOM FDMS)'), ]
TEOM_FDMS <- rbind(TEOM_FDMS_a, TEOM_FDMS_b)

##### use PM10 data from UK-AIR and AQ ENG #####################################

TEOM_STD_a <- META_10Dec2014_PM10[ which(META_10Dec2014_PM10$instr_a =='N ugm-3 (Ref.eq)'), ]
TEOM_STD_b <- META_10Dec2014_PM10[ which(META_10Dec2014_PM10$instr_a =='P ugm-3 (Ref.eq)'), ]

TEOM_STD_C <- META_10Dec2014_PM10[ which(META_10Dec2014_PM10$instr_a =='P ugm-3 (INDIC.GRAV)'), ]
TEOM_STD_A <- rbind(TEOM_STD_a, TEOM_STD_b)


################ Merge Sites within 130 Km distance for  ###########################
######################### P ugm-3 (INDIC.GRAV) #####################################

EASTING_STD_C <- TEOM_STD_C[5] ## i
NORTHING_STD_C <- TEOM_STD_C[6] ##i
EASTING_FDMS <- TEOM_FDMS[5] ##j
NORTHING_FDMS <- TEOM_FDMS[6] ##j

LAT_STD_C <- TEOM_STD_C[3] ## i
LON_STD_C <- TEOM_STD_C[4] ##i
LAT_FDMS <- TEOM_FDMS[3] ##j
LON_FDMS <- TEOM_FDMS[4] ##j

LAT_STD_C_geo <- TEOM_STD_C[2:3] ## i  ## site codes + LAT
rownames(LAT_STD_C_geo) = LAT_STD_C_geo [,1]
LAT_STD_C_geo <- LAT_STD_C_geo[-1]
LAT_STD_C_geo <- t(LAT_STD_C_geo)


LON_STD_C_geo <- TEOM_STD_C[2:4] ## i  ## site codes + LAT
rownames(LON_STD_C_geo) = LON_STD_C_geo [,1]
LON_STD_C_geo <- LON_STD_C_geo[-c(1,2)]
LON_STD_C_geo <- t(LON_STD_C_geo)


LAT_FDMS_geo<- TEOM_FDMS[2:3] ##j   ## site codes + LAT
rownames(LAT_FDMS_geo) = LAT_FDMS_geo [,1]
LAT_FDMS_geo <- LAT_FDMS_geo[-1]
LAT_FDMS_geo <- t(LAT_FDMS_geo)

LON_FDMS_geo <- TEOM_FDMS[2:4] ##j  ## site codes + LON
rownames(LON_FDMS_geo) = LON_FDMS_geo [,1]
LON_FDMS_geo <- LON_FDMS_geo[-c(1,2)]
LON_FDMS_geo <- t(LON_FDMS_geo)

CODES_FDMS <- TEOM_FDMS[2]    ##### Site CODES where there are TEOM_FDMS
CODES_STD_C <- TEOM_STD_C[2]     ##### Site CODES where there are TEOM_STD
SITES_FDMS <- TEOM_FDMS[1]   ##### Site NAMES where there are TEOM_FDMS
SITES_STD_C <- TEOM_STD_C[1]   ##### Site NAMES where there are TEOM_STD

############## LOOOOOPPPPPPPPYYYYYY  (EASTING, NORTHING)  ########################

d <- data.frame()

for (j in 1:nrow(TEOM_FDMS)) {
  for(i in 1:nrow(TEOM_STD_C)) {
  distance_STD_C = sqrt((EASTING_FDMS[j,]- EASTING_STD_C[i,])^2 + (NORTHING_FDMS[j,]- NORTHING_STD_C[i,])^2) < 130000
}
}

for (j in 1:nrow(TEOM_FDMS)) {
  for(i in 1:nrow(TEOM_STD_C)) {
  distance_STD_C[j] = sqrt((EASTING_FDMS[j,]- EASTING_STD_C[i,])^2 + (NORTHING_FDMS[j,]- NORTHING_STD_C[i,])^2) < 130000
  d <- rbind(d,distance_STD_C[j])
 }
}


AAA_C <- split(d, 1:nrow(TEOM_STD_C))
AAA_C <- as.data.frame(AAA_C)
colnames(AAA_C) <- CODES_STD_C[,1]   #### Site CODES
rownames(AAA_C) <- CODES_FDMS[,1]  #### Site CODE


SITES_STD_C[,1]                       ###### Site NAMES
names_STD_C <- as.character(SITES_STD_C[,1]) 
names_STD_C <- as.list(names_STD_C)  ###### List of Site NAMES
prova_C <- as.list(names_STD_C)   ###### List of Site NAMES


CODES_STD_C [,1]                     ###### Site CODES
codes_STD_C <- as.character(CODES_STD_C[,1]) 
codes_STD_C <- as.list(codes_STD_C)  ###### List of Site CODES
prova_codes_C <- as.list(codes_STD_C)  ###### List of Site CODES


AAA_C <- split(d, 1:nrow(TEOM_STD_C))
AAA_C <- as.data.frame(AAA_C)
colnames(AAA_C) <- CODES_STD_C[,1]   #### Site CODES
rownames(AAA_C) <- CODES_FDMS[,1]  #### Site CODE


SITES_STD_C[,1]                       ###### Site NAMES
names_STD_C <- as.character(SITES_STD_C[,1]) 
names_STD_C <- as.list(names_STD_C)  ###### List of Site NAMES
prova_C <- as.list(names_STD_C)   ###### List of Site NAMES


CODES_STD_C [,1]                     ###### Site CODES
codes_STD_C <- as.character(CODES_STD_C[,1]) 
codes_STD_C <- as.list(codes_STD_C)  ###### List of Site CODES
prova_codes_C <- as.list(codes_STD_C)  ###### List of Site CODES



###### For each site with TEOM_STD_C within 130km of any TEOM-FDMS merge data, LAT, LON ###
############################## USE Site NAMES  ######################################
##############################    V10 (data, LAT,LON)  #################################


for (i in 1: ncol(AAA_C)){
  
  FDMS_C <- assign(paste("FDMS",prova_codes_C[i],sep="_"),as.data.frame(data_V10[ , which(names(data_V10) %in% row.names(assign(paste(names_STD_C[i],sep="_"),
                      as.data.frame(subset(AAA_C[i],AAA_C[i]=="TRUE")))))]))
  
  
  STD_C <- assign(paste("STD",prova_codes_C[i],sep="_"),as.data.frame(data_PM10[ , which(names(data_PM10) %in% colnames(assign(paste(names_STD_C[i],sep="_"),
                     as.data.frame(subset(AAA_C[i],AAA_C[i]=="TRUE")))))]))
  
  colnames(STD_C) <- paste(CODES_STD_C[i,1],"TEOM_STD_C",sep ="_")
  
  
  LAT_FDMS_C <- assign(paste("LAT_FDMS",prova_codes_C[i],sep="_"),LAT_FDMS_geo[ ,which(colnames(LAT_FDMS_geo) %in% row.names(assign(paste(names_STD_C[i],sep="_"),
                        as.data.frame(subset(AAA_C[i],AAA_C[i]=="TRUE")))))])
 

  #LAT_STD_C <- assign(paste("LAT_STD",prova_codes_C[i],sep="_"),as.data.frame(LAT_STD_C_geo[ , which(colnames(LAT_STD_C_geo) %in% colnames(assign(paste(names_STD_C[i],sep="_"),
   #                             as.data.frame(subset(AAA_C[i],AAA_C[i]=="TRUE")))))]))
  
  LAT_STD_C <- assign(paste("LAT_STD",prova_codes_C[i],sep="_"),LAT_STD_C_geo[ , which(colnames(LAT_STD_C_geo) %in% colnames(assign(paste(names_STD_C[i],sep="_"),
                        as.data.frame(subset(AAA_C[i],AAA_C[i]=="TRUE")))))])

  
  LON_FDMS_C <- assign(paste("LON_FDMS",prova_codes_C[i],sep="_"),LON_FDMS_geo[ ,which(colnames(LON_FDMS_geo) %in% row.names(assign(paste(names_STD_C[i],sep="_"),
                              as.data.frame(subset(AAA_C[i],AAA_C[i]=="TRUE")))))])
  
  
  #LON_STD_C <- assign(paste("LON_STD",prova_codes_C[i],sep="_"),as.data.frame(LON_STD_C_geo[ , which(colnames(LON_STD_C_geo) %in% colnames(assign(paste(names_STD_C[i],sep="_"),
   #                           as.data.frame(subset(AAA_C[i],AAA_C[i]=="TRUE")))))]))
  
  LON_STD_C <- assign(paste("LON_STD",prova_codes_C[i],sep="_"),LON_STD_C_geo[ , which(colnames(LON_STD_C_geo) %in% colnames(assign(paste(names_STD_C[i],sep="_"),
                              as.data.frame(subset(AAA_C[i],AAA_C[i]=="TRUE")))))])
  
  #colnames(LAT_STD_C) <- paste(CODES_STD_C[i,1],"LAT_STD_C",sep ="_")
  #colnames(LON_STD_C) <- paste(CODES_STD_C[i,1],"LON_STD_C",sep ="_")
  colnames(LAT_FDMS) <- paste(CODES_FDMS[i,1],"LAT_FDMS",sep ="_")
  colnames(LON_FDMS) <- paste(CODES_FDMS[i,1],"LON_FDMS",sep ="_")
  ALL_C <- assign(paste("ALL",prova_codes_C[i],sep="_"),cbind(date_10Dec2014,FDMS_C,STD_C))
  
  MEAN_STD_C <- as.numeric(as.character(unlist(STD_C[1])))
  MEAN_STD_C <- mean(MEAN_STD_C, na.rm = TRUE)  #### calculate the mean for TEOM_STD
  LAT_LON_STD_C <- assign(paste("LAT_LON_STD_C",prova_codes_C[i],sep="_"),cbind(LAT_STD_C,LON_STD_C,MEAN_STD_C))
  
  MEAN_FDMS_C <- as.matrix(FDMS_C[])  
  MEAN_FDMS_C <- matrix(as.numeric(unlist(MEAN_FDMS_C)),nrow=nrow(MEAN_FDMS_C))
  MEAN_FDMS_C <- colMeans(MEAN_FDMS_C, na.rm = TRUE)
  LAT_LON_FDMS_C <-assign(paste("LAT_LON_FDMS_C",prova_codes_C[i],sep="_"),cbind(LAT_FDMS_C,LON_FDMS_C,MEAN_FDMS_C))
  
  outputname_C <- paste("130Km_Data_Sites_CODES/",gsub("[.]csv$","",prova_codes_C[i]),"_TEOM_STD_C.csv",sep ="")
  print(outputname_C)
  write.csv(ALL_C, file = outputname_C,row.names=FALSE)
  
  outputname_D <- paste("130Km_Data_Sites_CODES/",gsub("[.]csv$","",prova_codes_C[i]),"_LAT_LON_STD_C.csv",sep ="")
  print(outputname_D)
  write.csv(LAT_LON_STD_C, file = outputname_D,row.names=FALSE)
  
  outputname_E <- paste("130Km_Data_Sites_CODES/",gsub("[.]csv$","",prova_codes_C[i]),"_LAT_LON_FDMS_C.csv",sep ="")
  print(outputname_E)
  write.csv(LAT_LON_FDMS_C, file = outputname_E,row.names=FALSE)
  

  ######## MAPS and SAVE ####################

  mypath <- file.path("C:","TEOM_VCM","130Km_Data_Sites_CODES", "MAPS_FDMS_STD_C_130km"
                      ,paste("MAP_STD_C_", prova_codes_C[i], ".jpg", sep = ""))
  
  #jpeg(file=mypath)
    
  #newmap <- getMap(resolution = "low")
  #map <- plot(newmap, xlim = c(-13, 6), ylim = c(49, 61), asp = 1)  ### UK map
  #MAP_STD_C <- points(LON_STD_C, LAT_STD_C, col = "red", cex = 1, pch = 21, bg = "grey")
  #MAP_FDMS_C <- points(LON_FDMS_C, LAT_FDMS_C, col = "blue", cex = 1, pch = 23, bg = "blue")


  lat <- c(49, 55)
  lon <- c(-13, 10)
  map <- get_map(location = c(lon = mean(lon), lat = mean(lat)), zoom = 7)
  #,maptype = "satellite", source = "google")
  #MAP_FDMS_C <- ggmap(map) + geom_point(data=as.data.frame(LAT_LON_FDMS_C), alpha = .8,
   #                     aes(x = LON_FDMS_C, y = LAT_FDMS_C,
    #                        size = MEAN_FDMS_C), color="blue")

  MAP_FDMS_STD_C <- ggmap(map) + geom_point(data=as.data.frame(LAT_LON_STD_C), alpha = .8,
                       aes(x = LON_STD_C, y = LAT_STD_C,size = MEAN_STD_C),
                       color="red") +
    geom_point(data=as.data.frame(LAT_LON_FDMS_C), alpha = .8,
                            aes(x = LON_FDMS_C, y = LAT_FDMS_C), color="blue")
    

  MAP_FDMS_STD_C <- MAP_FDMS_STD_C + ggtitle(paste("TEOM STD AQ ENGLAND_",
                                                   prova_codes_C[i])) +
    xlab(" ") + ylab(" ") + theme(text=element_text(size=9, family="Comic Sans MS"))
  
                                                  
  ggsave(mypath, MAP_FDMS_STD_C)
  
  #title(main = paste("TEOM STD AQ ENGLAND_",prova_codes_C[i]), font.main= 1.5) 
  #dev.off()

}


################ Merge Sites within 130 Km distance for  ###########################
######################### P-N ugm-3 (Ref.eq) #####################################

EASTING_STD_A <- TEOM_STD_A[5] ## i
NORTHING_STD_A <- TEOM_STD_A[6] ##i
EASTING_FDMS <- TEOM_FDMS[5] ##j
NORTHING_FDMS <- TEOM_FDMS[6] ##j

LAT_STD_A <- TEOM_STD_A[3] ## i
LON_STD_A <- TEOM_STD_A[4] ##i
LAT_FDMS <- TEOM_FDMS[3] ##j
LON_FDMS <- TEOM_FDMS[4] ##j

LAT_STD_A_geo <- TEOM_STD_A[2:3] ## i  ## site codes + LAT
rownames(LAT_STD_A_geo) = LAT_STD_A_geo [,1]
LAT_STD_A_geo <- LAT_STD_A_geo[-1]
LAT_STD_A_geo <- t(LAT_STD_A_geo)


LON_STD_A_geo <- TEOM_STD_A[2:4] ## i  ## site codes + LAT
rownames(LON_STD_A_geo) = LON_STD_A_geo [,1]
LON_STD_A_geo <- LON_STD_A_geo[-c(1,2)]
LON_STD_A_geo <- t(LON_STD_A_geo)


LAT_FDMS_geo<- TEOM_FDMS[2:3] ##j   ## site codes + LAT
rownames(LAT_FDMS_geo) = LAT_FDMS_geo [,1]
LAT_FDMS_geo <- LAT_FDMS_geo[-1]
LAT_FDMS_geo <- t(LAT_FDMS_geo)

LON_FDMS_geo <- TEOM_FDMS[2:4] ##j  ## site codes + LON
rownames(LON_FDMS_geo) = LON_FDMS_geo [,1]
LON_FDMS_geo <- LON_FDMS_geo[-c(1,2)]
LON_FDMS_geo <- t(LON_FDMS_geo)

CODES_FDMS <- TEOM_FDMS[2]    ##### Site CODES where there are TEOM_FDMS
CODES_STD_A <- TEOM_STD_A[2]     ##### Site CODES where there are TEOM_STD
SITES_FDMS <- TEOM_FDMS[1]   ##### Site NAMES where there are TEOM_FDMS
SITES_STD_A <- TEOM_STD_A[1]   ##### Site NAMES where there are TEOM_STD


############## LOOOOOPPPPPPPPYYYYYY  ####################################

e <- data.frame()

for (j in 1:nrow(TEOM_FDMS)) {
  for(i in 1:nrow(TEOM_STD_A)) {
    distance_STD_A = sqrt((EASTING_FDMS[j,]- EASTING_STD_A[i,])^2 + (NORTHING_FDMS[j,]- NORTHING_STD_A[i,])^2) < 130000
  }
}

for (j in 1:nrow(TEOM_FDMS)) {
  for(i in 1:nrow(TEOM_STD_A)) {
    distance_STD_A[j] = sqrt((EASTING_FDMS[j,]- EASTING_STD_A[i,])^2 + (NORTHING_FDMS[j,]- NORTHING_STD_A[i,])^2) < 130000
    e <- rbind(e,distance_STD_A[j])
  }
}


AAA_A <- split(e, 1:nrow(TEOM_STD_A))
AAA_A <- as.data.frame(AAA_A)
colnames(AAA_A) <- CODES_STD_A[,1]   #### Site CODES
rownames(AAA_A) <- CODES_FDMS[,1]  #### Site CODE


SITES_STD_A[,1]                       ###### Site NAMES
names_STD_A <- as.character(SITES_STD_A[,1]) 
names_STD_A <- as.list(names_STD_A)  ###### List of Site NAMES
prova_A <- as.list(names_STD_A)   ###### List of Site NAMES


CODES_STD_A [,1]                     ###### Site CODES
codes_STD_A <- as.character(CODES_STD_A[,1]) 
codes_STD_A <- as.list(codes_STD_A)  ###### List of Site CODES
prova_codes_A <- as.list(codes_STD_A)  ###### List of Site CODES



###### For each site with TEOM_STD_A within 130km of any TEOM-FDMS merge data, LAT, LON ###
############################## USE Site NAMES  ######################################
##############################    V10 (data, LAT,LON)  #################################


for (i in 1: ncol(AAA_A)){
  
  FDMS_A <- assign(paste("FDMS",prova_codes_A[i],sep="_"),as.data.frame(data_V10[ , which(names(data_V10) %in% row.names(assign(paste(names_STD_A[i],sep="_"),
                            as.data.frame(subset(AAA_A[i],AAA_A[i]=="TRUE")))))]))
  
  STD_A <- assign(paste("STD",prova_codes_A[i],sep="_"),as.data.frame(data_PM10[ , which(names(data_PM10) %in% colnames(assign(paste(names_STD_A[i],sep="_"),
                        as.data.frame(subset(AAA_A[i],AAA_A[i]=="TRUE")))))]))
  
  colnames(STD_A) <- paste(CODES_STD_A[i,1],"TEOM_STD_A",sep ="_")
  
  
  LAT_FDMS_A <- assign(paste("LAT_FDMS",prova_codes_A[i],sep="_"),LAT_FDMS_geo[ ,which(colnames(LAT_FDMS_geo) %in% row.names(assign(paste(names_STD_A[i],sep="_"),
                         as.data.frame(subset(AAA_A[i],AAA_A[i]=="TRUE")))))])
  
  
  LAT_STD_A <- assign(paste("LAT_STD",prova_codes_A[i],sep="_"),LAT_STD_A_geo[ , which(colnames(LAT_STD_A_geo) %in% colnames(assign(paste(names_STD_A[i],sep="_"),
                       as.data.frame(subset(AAA_A[i],AAA_A[i]=="TRUE")))))])
  
  
  LON_FDMS_A <- assign(paste("LON_FDMS",prova_codes_A[i],sep="_"),LON_FDMS_geo[ ,which(colnames(LON_FDMS_geo) %in% row.names(assign(paste(names_STD_A[i],sep="_"),
                      as.data.frame(subset(AAA_A[i],AAA_A[i]=="TRUE")))))])

  

  LON_STD_A <- assign(paste("LON_STD",prova_codes_A[i],sep="_"),LON_STD_A_geo[ , which(colnames(LON_STD_A_geo) %in% colnames(assign(paste(names_STD_A[i],sep="_"),
                                         as.data.frame(subset(AAA_A[i],AAA_A[i]=="TRUE")))))]) 
  

  colnames(LAT_FDMS) <- paste(CODES_FDMS[i,1],"LAT_FDMS",sep ="_")
  colnames(LON_FDMS) <- paste(CODES_FDMS[i,1],"LON_FDMS",sep ="_")
  ALL_A <- assign(paste("ALL",prova_codes_A[i],sep="_"),cbind(date_10Dec2014,FDMS_A,STD_A))
  

  
  MEAN_STD_A <- as.numeric(as.character(unlist(STD_A[1])))
  MEAN_STD_A <- mean(MEAN_STD_A, na.rm = TRUE)  #### calculate the mean for TEOM_STD
  LAT_LON_STD_A <- assign(paste("LAT_LON_STD_A",prova_codes_A[i],sep="_"),cbind(LAT_STD_A,LON_STD_A,MEAN_STD_A))
  
  
  MEAN_FDMS_A <- as.matrix(FDMS_A[])  
  MEAN_FDMS_A <- matrix(as.numeric(unlist(MEAN_FDMS_A)),nrow=nrow(MEAN_FDMS_A))
  MEAN_FDMS_A <- colMeans(MEAN_FDMS_A, na.rm = TRUE)
  LAT_LON_FDMS_A <-assign(paste("LAT_LON_FDMS_A",prova_codes_A[i],sep="_"),cbind(LAT_FDMS_A,LON_FDMS_A,MEAN_FDMS_A))
  
  
  outputname_A <- paste("130Km_Data_Sites_CODES/",gsub("[.]csv$","",prova_codes_A[i]),"_TEOM_STD_A.csv",sep ="")
  print(outputname_A)
  write.csv(ALL_A, file = outputname_A,row.names=FALSE)
  
  outputname_F <- paste("130Km_Data_Sites_CODES/",gsub("[.]csv$","",prova_codes_A[i]),"_LAT_LON_STD_A.csv",sep ="")
  print(outputname_F)
  write.csv(LAT_LON_STD_A, file = outputname_F,row.names=FALSE)
  
  outputname_G <- paste("130Km_Data_Sites_CODES/",gsub("[.]csv$","",prova_codes_A[i]),"_LAT_LON_FDMS_A.csv",sep ="")
  print(outputname_G)
  write.csv(LAT_LON_FDMS_A, file = outputname_G,row.names=FALSE)
  
  
  ######## MAPS and SAVE ####################
  
  mypath <- file.path("C:","TEOM_VCM","130Km_Data_Sites_CODES", "MAPS_FDMS_STD_A_130km"
                      ,paste("MAP_STD_A_", prova_codes_A[i], ".jpg", sep = ""))
  
  lat <- c(49, 55)
  lon <- c(-13, 10)
  map <- get_map(location = c(lon = mean(lon), lat = mean(lat)), zoom = 7)

  
  MAP_FDMS_STD_A <- ggmap(map) + geom_point(data=as.data.frame(LAT_LON_STD_A), alpha = .8,
                         aes(x = LON_STD_A, y = LAT_STD_A,size = MEAN_STD_A),
                                            color="red") +
    geom_point(data=as.data.frame(LAT_LON_FDMS_A), alpha = .8,
               aes(x = LON_FDMS_A, y = LAT_FDMS_A), color="blue")
  
  
  MAP_FDMS_STD_A <- MAP_FDMS_STD_A + ggtitle(paste("TEOM STD AQ ENGLAND_",
                                                   prova_codes_A[i])) +
    xlab(" ") + ylab(" ") + theme(text=element_text(size=9, family="Comic Sans MS"))
  
  
  ggsave(mypath, MAP_FDMS_STD_A)
  
}



####################################################################################
#####################################################################################

setwd("C:/TEOM_VCM/130Km_Data_Sites_CODES")

TEOM_STD_130km_C <- list.files(path ="C:/TEOM_VCM/130Km_Data_Sites_CODES",
                             pattern = "TEOM_STD_C") #this includes only files with "TEOM_STD"

names_STD_NEW_C <- as.list(TEOM_STD_130km_C)


####################################################################################
#####################################################################################

setwd("C:/TEOM_VCM/130Km_Data_Sites_CODES")

TEOM_STD_130km_A <- list.files(path ="C:/TEOM_VCM/130Km_Data_Sites_CODES",
                               pattern = "TEOM_STD_A") #this includes only files with "TEOM_STD"

names_STD_NEW_A <- as.list(TEOM_STD_130km_A)


################### MEANS ##########################################################
################## ugm-3 (INDIC.GRAV) #############################################


for (i in c(1:length(TEOM_STD_130km_C))) {
  JJJ_C <- assign(gsub("[.]csv$","",TEOM_STD_130km_C[i]), read.csv(TEOM_STD_130km_C[i], header=TRUE, 
                                                               as.is = 1)) 

  NCOL_C = ncol(as.matrix(read.csv(TEOM_STD_130km_C[i], header=TRUE,as.is = 1)))
  LLL_C <- as.matrix(JJJ_C[,c(-1,-NCOL_C)])   #### omit fist column, colum of date, and last TEOM STANDARD column
   
  LLL_C <- -1*LLL_C
  # CHANGE_SIGN <- -1*(LLL_C)  ### change sign to each v10 value
  # ABS_VAL <- abs(LLL_C)      ### set each v10 value to its absolute value
  
  
  ##### means of all columns EXCEPT date ### ugm-3 (INDIC.GRAV) ######

  MEAN_V10_C <- assign(paste("MEAN_V10_C",TEOM_STD_130km_C[i],sep="_"),rowMeans((LLL_C), na.rm = TRUE))
  JJJ_C$TEOM_V10_MEAN_C <- MEAN_V10_C
  TEOM_STD_C <- as.matrix(JJJ_C[NCOL_C])
  JJJ_C$TEOM_STD_Original_C <- TEOM_STD_C/1.3   ### ugm-3 (INDIC.GRAV) + ESEPA factor
  JJJ_C$TEOM_STD_NEW_C <- (((TEOM_STD_C/1.3)-3)/1.03)-(1.87*(MEAN_V10_C))
  
  outputname_MEAN_C <- paste("TEOM_MEANS_CODES/",gsub("[.]csv$","",names_STD_NEW_C[i]),"_V10_MEAN_C.csv",sep ="")
  print(outputname_MEAN_C)
  write.csv(JJJ_C, file = outputname_MEAN_C,row.names=FALSE)

}



################### MEANS ##########################################################
################## ### ugm-3 (Ref.eq.) #############################################

  for (i in c(1:length(TEOM_STD_130km_A))) {
    JJJ_A <- assign(gsub("[.]csv$","",TEOM_STD_130km_A[i]), read.csv(TEOM_STD_130km_A[i], header=TRUE, 
                                              as.is = 1)) 
    
    NCOL_A = ncol(as.matrix(read.csv(TEOM_STD_130km_A[i], header=TRUE,as.is = 1)))
    LLL_A <- as.matrix(JJJ_A[,c(-1,-NCOL_A)])   #### omit fist column, colum of date, and last TEOM STANDARD column
    
    LLL_A <- -1*LLL_A
    # CHANGE_SIGN <- -1*(LLL_A)  ### change sign to each v10 value
    # ABS_VAL <- abs(LLL_A)      ### set each v10 value to its absolute value
 
    ##### means of all columns EXCEPT date ### ugm-3 (Ref.eq.)
    
    MEAN_V10_A <- assign(paste("MEAN_V10_A",TEOM_STD_130km_A[i],sep="_"),rowMeans(LLL_A, na.rm = TRUE))
    JJJ_A$TEOM_V10_MEAN_A <- MEAN_V10_A
    TEOM_STD_A <- as.matrix(JJJ_A[NCOL_A])
    JJJ_A$TEOM_STD_Original_A <- TEOM_STD_A/0.8333   ### ugm-3 (Ref.eq.)+USEPA correction
    JJJ_A$TEOM_STD_NEW_A <- (((TEOM_STD_A/0.8333)-3)/1.03)-(1.87*(MEAN_V10_A))
    
    outputname_MEAN_A <- paste("TEOM_MEANS_CODES/",gsub("[.]csv$","",names_STD_NEW_A[i]),"_V10_MEAN_A.csv",sep ="")
    print(outputname_MEAN_A)
    write.csv(JJJ_A, file = outputname_MEAN_A,row.names=FALSE)
  }


#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################


