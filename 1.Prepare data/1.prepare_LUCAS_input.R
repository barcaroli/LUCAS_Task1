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
s2022 <- fread(paste0(path_input,"LUCAS22_corrected_v3.csv"))
#####################################################################
# MODIFIED:
s2022$SURVEY_LC1 <- toupper(s2022$SURVEY_LC1)
s2022$SURVEY_LC1_SPEC <- toupper(s2022$SURVEY_LC1_SPEC)
s2022$SURVEY_LU1 <- toupper(s2022$SURVEY_LU1)
s2022$SURVEY_LU1_SPEC <- toupper(s2022$SURVEY_LU1_SPEC)
s2022$SURVEY_FEATURE_WIDTH <- ifelse(is.na(s2022$SURVEY_FEATURE_WIDTH),8,s2022$SURVEY_FEATURE_WIDTH)
s2022$SURVEY_TREE_HEIGHT_MATURITY <- ifelse(is.na(s2022$SURVEY_TREE_HEIGHT_MATURITY),8,s2022$SURVEY_TREE_HEIGHT_MATURITY)
######################################################################

# Assign WGT_LUCAS + STRATUM_LUCAS
s2 <- fread(paste0(path_input,"sample_LUCAS_2022.csv"))
s2022 <- merge(s2022,s2[,c("POINT_ID","WGT_LUCAS","STRATUM_LUCAS")])
sum(s2022$WGT_LUCAS)

# Calculate fpc
s2022$ones <- 1
num<-aggregate(s2022$ones,by=list(s2022$STRATUM_LUCAS),FUN=sum)
num$n=num$x
num$x<-NULL
den<-aggregate(s2022$WGT_LUCAS,by=list(s2022$STRATUM_LUCAS),FUN=sum)
den$N=den$x
den$x<-NULL
fpc<-merge(num,den,by="Group.1")
fpc$STRATUM_LUCAS<-fpc$Group.1
fpc$fpc=fpc$n/fpc$N
s2022 <- merge(s2022,fpc[,c("STRATUM_LUCAS","fpc")],by="STRATUM_LUCAS")


# Assign NUTS24
load("D:/Google Drive/LUCAS 2025/2.Master/master_complete.RData")
# master$ELEV2 <- ifelse(master$ELEV_DEM < 100, 1,
#                        ifelse(master$ELEV_DEM < 200, 2,
#                               ifelse(master$ELEV < 500, 3,
#                                      ifelse(master$ELEV_DEM < 1000, 4,
#                                             ifelse(master$ELEV_DEM < 1500, 5, 6)))))
# save(master,file="D:/Google Drive/LUCAS 2025/2.Master/master_complete.RData")
# master$ELEV2 <- as.factor(master$ELEV2)
# master$CLC18_1d<-factor(substr(master$CLC18_vett,1,1))


s2022 <- merge(s2022,master[,c("POINT_ID","point_area","NUTS0_24","NUTS1_24","NUTS2_24","NUTS3_24","ELEV2")],by="POINT_ID")


# Assign population
pop <- read.csv(paste0(path_input,"EU_population_2009_2023.csv"))
colnames(pop) <- c("NUTS","Pop2009","Pop2012","Pop2015","Pop2018","Pop2023")
s2022 <- merge(s2022,pop[,c("NUTS","Pop2023")],by.x="NUTS0_24",by.y="NUTS")

# Assign Fao classes, settlement, LUE and LUD
source("2.assign_FAO_settlement_LUE_LUD.R")
table(s2022$fao_class_name)/nrow(s2022)

# Calculate settl_pc
table(as.numeric(s2022$settlement))
s2022$settl_pc <- (as.numeric(s2022$settlement)) * 1000 / s2022$Pop2023
summary(s2022$settl_pc)


filename <- paste0(path_input,"LUCAS22_corrected_complete.csv")

write.table(s2022,filename,sep=",",quote=F,row.names=F)
