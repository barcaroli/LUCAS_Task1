#----------------------------------------------------------
# Script to prepare the LUCAS dataset:
# read GOPA corrected dataset, assign
# * STRATUM_LUCAS + WGT_LUCAS
# * NUTS24
# * FAO forest + settlement + LUE + LUD
#----------------------------------------------------------
# Input: LUCAS22_corrected_v2.csv
#        
#        master
# Output: LUCAS22_corrected_complete.csv
#----------------------------------------------------------
setwd("D:/Google Drive/LUCAS 2025/1.Prepare data/")
path_input <- "D:/Google Drive/LUCAS 2025/2.DATA/"

library(data.table)
s <- fread(paste0(path_input,"LUCAS22_corrected_v3.csv"))
#remove rows with NA in LC, LU
s$SURVEY_LC1 <- toupper(s$SURVEY_LC1)
s$SURVEY_LU1 <- toupper(s$SURVEY_LU1)
s=s[!(is.na(s$SURVEY_LC1) | is.na(s$SURVEY_LU1)), ]
a <- s[s$SURVEY_LC1 == "8", ]
a <- s[s$SURVEY_LU1 == "8", ]
# s=s[s$SURVEY_LC1 != "8", ]
# s=s[s$SURVEY_LU1 != "8", ]

# Assign WGT_LUCAS + STRATUM_LUCAS
s2 <- fread(paste0(path_input,"sample_LUCAS_2022.csv"))
s <- merge(s,s2[,c("POINT_ID","WGT_LUCAS","STRATUM_LUCAS")])

# Calculate fpc
s$ones <- 1
num<-aggregate(s$ones,by=list(s$STRATUM_LUCAS),FUN=sum)
num$n=num$x
num$x<-NULL
den<-aggregate(s$WGT_LUCAS,by=list(s$STRATUM_LUCAS),FUN=sum)
den$N=den$x
den$x<-NULL
fpc<-merge(num,den,by="Group.1")
fpc$STRATUM_LUCAS<-fpc$Group.1
fpc$fpc=fpc$n/fpc$N
s <- merge(s,fpc[,c("STRATUM_LUCAS","fpc")],by="STRATUM_LUCAS")


# Assign NUTS24
load("D:/Google Drive/LUCAS 2025/2.Master/master_complete.RData")
# master$ELEV2 <- ifelse(master$ELEV_DEM < 100, 1, 
#                        ifelse(master$ELEV_DEM < 200, 2,
#                               ifelse(master$ELEV < 500, 3,
#                                      ifelse(master$ELEV_DEM < 1000, 4,
#                                             ifelse(master$ELEV_DEM < 1500, 5, 6)))))
# master$ELEV2 <- as.factor(master$ELEV2)
# master$CLC18_1d<-factor(substr(master$CLC18_vett,1,1))


s <- merge(s,master[,c("POINT_ID","point_area","NUTS0_24","NUTS1_24","NUTS2_24","NUTS3_24")],by="POINT_ID")


# Assign population
pop <- read.csv(paste0(path_input,"EU_population_2009_2023.csv"))
colnames(pop) <- c("NUTS","Pop2009","Pop2012","Pop2015","Pop2018","Pop2023")
s <- merge(s,pop[,c("NUTS","Pop2023")],by.x="NUTS0_24",by.y="NUTS")

# Assign Fao classes, settlement, LUE and LUD
source("2.assign_FAO_settlement_LUE_LUD.R")

# Calculate settl_pc
table(as.numeric(s$settlement))
s$settl_pc <- (as.numeric(s$settlement)) * 1000 / s$Pop2023
summary(s$settl_pc)


filename <- paste0(path_input,"LUCAS22_corrected_complete.csv")

write.table(s,filename,sep=",",quote=F,row.names=F)
