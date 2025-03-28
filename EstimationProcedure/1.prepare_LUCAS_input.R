#----------------------------------------------------------
# Script to prepare the LUCAS dataset:
# read GOPA corrected dataset, assign
# * STRATUM_LUCAS + WGT_LUCAS
# * NUTS24
# * FAO forest + settlement + LUE + LUD
#----------------------------------------------------------
# Input: LUCAS22_corrected_v4.csv
#        sample_LUCAS_2022.csv
#        master_complete.RData
# Output: LUCAS22_corrected_complete.csv
#----------------------------------------------------------
library(data.table)
s2022 <- fread("LUCAS22_corrected_v4.csv")
table(s2022$SURVEY_LC1)
#####################################################################
# MODIFIED:
s2022$SURVEY_LC1 <- toupper(s2022$SURVEY_LC1)
s2022$SURVEY_LC1_SPEC <- toupper(s2022$SURVEY_LC1_SPEC)
s2022$SURVEY_LU1 <- toupper(s2022$SURVEY_LU1)
s2022$SURVEY_LU1_SPEC <- toupper(s2022$SURVEY_LU1_SPEC)
s2022$SURVEY_FEATURE_WIDTH <- ifelse(is.na(s2022$SURVEY_FEATURE_WIDTH),8,s2022$SURVEY_FEATURE_WIDTH)
s2022$SURVEY_TREE_HEIGHT_MATURITY <- ifelse(is.na(s2022$SURVEY_TREE_HEIGHT_MATURITY),8,s2022$SURVEY_TREE_HEIGHT_MATURITY)
######################################################################
# MODIFIED (change LU2):
xtabs(~SURVEY_LU2,data=s2022,addNA=TRUE)
s2018 <- fread("Input_Data_EU_LUCAS_2018.CSV")
s2018$POINT_ID <- as.numeric(s2018$POINT_ID)
s2022 <- merge(s2022,s2018[,c("POINT_ID","LU2")],by="POINT_ID",all.x=TRUE)
s2022$SURVEY_LU2 <- ifelse(!is.na(s2022$LU2) & s2022$LU2 == "8","8",s2022$SURVEY_LU2)
s2022$LU2 <- NULL
xtabs(~SURVEY_LU2,data=s2022,addNA=TRUE)
######################################################################
# MODIFIED:
# a <- s2022[s2022$SURVEY_LC1=="G30" | s2022$SURVEY_LC1=="G40",]
s2022 <- s2022[!(s2022$SURVEY_LC1=="G30" | s2022$SURVEY_LC1=="G40"),]
b <- s2022[s2022$SURVEY_LC1=="C30",]
######################################################################
#####################################################################
# MODIFIED:
s2018 <- fread("Survey_2018_cal_wgt.txt")
table(s2018$SURVEY_OBS_TYPE)
s2018$obs_type2018 <- ifelse(s2018$SURVEY_OBS_TYPE == 1,1,2)
s2022$obs_type2022 <- ifelse(s2022$SURVEY_OBS_TYPE == 1,1,2)
panel <- s2022[s2022$POINT_ID %in% s2018$POINT_ID,c("POINT_ID","SURVEY_LC1","SURVEY_LU1","obs_type2022")]
panel <- merge(panel,s2018[,c("POINT_ID","land_cover","land_use","obs_type2018")])
# load("master_complete.RData")
# panel <- merge(panel,master[,c("POINT_ID","BCK21_R")])
panel$ones <- 1
panel_A <- panel[substr(panel$land_cover,1,2)=="A1" & substr(panel$SURVEY_LC1,1,2)!="A1",]
# xtabs(ones~BCK21_R+SURVEY_LC1,data=panel_A)
panel_B <- panel_A[panel_A$obs_type2018==1 & panel_A$obs_type2022==2,]
# xtabs(ones~BCK21_R+SURVEY_LC1,data=panel_B)
xtabs(ones~land_cover+SURVEY_LC1,data=panel_B)
panel_C <- panel_B[panel_B$land_cover=="A11" & panel_B$SURVEY_LC1 %in% c("A21","A22","A23"),]
panel_D <- panel_B[panel_B$land_cover=="A12",]

xtabs(ones~land_cover+SURVEY_LC1,data=panel_C)
# xtabs(ones~land_cover+SURVEY_LC1+BCK21_R,data=panel_C)
s2022$SURVEY_LC1 <- ifelse(s2022$POINT_ID %in% panel_C$POINT_ID,"A11",s2022$SURVEY_LC1)
s2022$SURVEY_LC1 <- ifelse(s2022$POINT_ID %in% panel_D$POINT_ID,"A12",s2022$SURVEY_LC1)
cat("\n Number of modified observations for A11: ",nrow(panel_C),"\n")
cat("\n Number of modified observations for A12: ",nrow(panel_D),"\n")
table(s2022$SURVEY_LC1)

######################################################################

# Assign WGT_LUCAS + STRATUM_LUCAS
s2 <- fread("sample_LUCAS_2022.csv")
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
load("master_complete.RData")
s2022 <- merge(s2022,master[,c("POINT_ID","point_area","NUTS0_24","NUTS1_24","NUTS2_24","NUTS3_24","ELEV2")],by="POINT_ID")


# Assign population
pop <- read.csv("EU_population_2009_2023.csv")
colnames(pop) <- c("NUTS","Pop2009","Pop2012","Pop2015","Pop2018","Pop2023")
s2022 <- merge(s2022,pop[,c("NUTS","Pop2023")],by.x="NUTS0_24",by.y="NUTS")

# Assign Fao classes, settlement, LUE and LUD
source("1a.assign_FAO_settlement_LUE_LUD.R")
table(s2022$fao_class_name)/nrow(s2022)

# Calculate settl_pc
table(as.numeric(s2022$settlement))
s2022$settl_pc <- (as.numeric(s2022$settlement)) * 1000 / s2022$Pop2023
summary(s2022$settl_pc)

write.table(s2022,"LUCAS22_corrected_complete.csv",sep=",",quote=F,row.names=F)
