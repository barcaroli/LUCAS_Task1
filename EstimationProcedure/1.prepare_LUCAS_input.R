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
library(openxlsx)
s2022 <- fread("LUCAS22_corrected_v8.csv")
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
# panel_A1 <- panel[substr(panel$land_cover,1,1)=="A" & substr(panel$SURVEY_LC1,1,1)!="A",]
# panel_A2 <- panel[substr(panel$land_cover,1,2)=="A1" & substr(panel$SURVEY_LC1,1,2)!="A1",]
# addmargins(xtabs(~substr(land_cover,1,1)+substr(SURVEY_LC1,1,1),data=panel_A1,addNA=TRUE))
# addmargins(xtabs(~substr(land_cover,1,2)+substr(SURVEY_LC1,1,2),data=panel_A1,addNA=TRUE))
# addmargins(xtabs(~substr(land_cover,1,1)+substr(SURVEY_LC1,1,1),data=panel_A2,addNA=TRUE))
# addmargins(xtabs(~substr(land_cover,1,2)+substr(SURVEY_LC1,1,2),data=panel_A2,addNA=TRUE))

# panel$set1 <- ifelse(panel$land_cover == "A" | panel$land_use %in% c("U210", "U220", "U221", "U222", "U223", "U224", "U225", "U226", "U227", 
#                                                                      "U228", "U310", "U311", "U312", "U314", "U315", "U316", "U317", "U318", 
#                                                                      "U319", "U320", "U321", "U322", "U330", "U340", "U341", "U342", "U350", 
#                                                                      "U360", "U362", "U370"),1,0)
# panel$set2 <- ifelse(panel$SURVEY_LC1 == "A" | panel$SURVEY_LU1 %in% c("U210", "U220", "U221", "U222", "U223", "U224", "U225", "U226", "U227", 
#                                                                      "U228", "U310", "U311", "U312", "U314", "U315", "U316", "U317", "U318", 
#                                                                      "U319", "U320", "U321", "U322", "U330", "U340", "U341", "U342", "U350", 
#                                                                      "U360", "U362", "U370"),1,0)
# 
# xtabs(~set1+set2,data=panel)

panel_A <- panel[substr(panel$land_cover,1,2)=="A1" & substr(panel$SURVEY_LC1,1,2)!="A1",]

# xtabs(ones~BCK21_R+SURVEY_LC1,data=panel_A)
panel_B <- panel_A[panel_A$obs_type2018==1 & panel_A$obs_type2022==2,]
# xtabs(ones~BCK21_R+SURVEY_LC1,data=panel_B)
xtabs(ones~land_cover+SURVEY_LC1,data=panel_B)
# panel_C <- panel_B[panel_B$land_cover=="A11" & panel_B$SURVEY_LC1 %in% c("A21","A22","A23"),]
# panel_D <- panel_B[panel_B$land_cover=="A12" & panel_B$SURVEY_LC1 %in% c("A21","A22","A23"),]
panel_C <- panel_B[panel_B$land_cover=="A11" & panel_B$SURVEY_LC1 !="A11",]

panel_D <- panel_B[panel_B$land_cover=="A12" & panel_B$SURVEY_LC1 !="A12",]

panel_E <- panel[panel$land_use %in% c("U210", "U220", "U221", "U222", "U223", "U224", "U225", "U226", "U227", 
                                       "U228", "U310", "U311", "U312", "U314", "U315", "U316", "U317", "U318", 
                                       "U319", "U320", "U321", "U322", "U330", "U340", "U341", "U342", "U350", 
                                       "U360", "U362", "U370")
                 & !panel$SURVEY_LU1 %in% c("U210", "U220", "U221", "U222", "U223", "U224", "U225", "U226", "U227", 
                                          "U228", "U310", "U311", "U312", "U314", "U315", "U316", "U317", "U318", 
                                          "U319", "U320", "U321", "U322", "U330", "U340", "U341", "U342", "U350", 
                                          "U360", "U362", "U370"),]
panel_F <- panel_E[panel_E$obs_type2018==1 & panel_E$obs_type2022==2,]

# xtabs(~set1+set2,data=panel_C)
# xtabs(~set1+set2,data=panel_D)
# xtabs(~set1+set2,data=panel_F)

# panel_Z <- rbind(panel_C,panel_D,panel_F)
# panel_Z <- panel_Z[!duplicated(panel_Z$POINT_ID),]
# xtabs(~set1+set2,data=panel_Z)
# 
# xtabs(ones~land_cover+SURVEY_LC1,data=panel_C)
# xtabs(ones~land_cover+SURVEY_LC1+BCK21_R,data=panel_C)
s2022$SURVEY_LC1 <- ifelse(s2022$POINT_ID %in% panel_C$POINT_ID,panel_C$land_cover,s2022$SURVEY_LC1)
s2022$SURVEY_LC1 <- ifelse(s2022$POINT_ID %in% panel_D$POINT_ID,panel_D$land_cover,s2022$SURVEY_LC1)
s2022$SURVEY_LU1 <- ifelse(s2022$POINT_ID %in% panel_F$POINT_ID,panel_F$land_use,s2022$SURVEY_LU1)
cat("\n Number of modified observations for A11: ",nrow(panel_C),"\n")
cat("\n Number of modified observations for A12: ",nrow(panel_D),"\n")
cat("\n Number of modified observations for land use: ",nrow(panel_F),"\n")
table(s2022$SURVEY_LC1)

######################################################################
# NEW IMPUTATION
xtabs(~SURVEY_LC1,data=s2022)
xtabs(~SURVEY_LU1,data=s2022)
panel$flag_LC <- ifelse(nchar(as.character(panel$SURVEY_LC1)) < 3
                        | grepl("X",panel$SURVEY_LC1)
                        | grepl("x",panel$SURVEY_LC1),1,0)
table(panel$flag_LC)
to_be_imputed <- panel[panel$flag_LC==1 & panel$obs_type2018==1 & panel$obs_type2022!=1,]
cat("\n Number of imputations (last modification):",nrow(to_be_imputed))
table(s2022$SURVEY_LC1)
s2022$SURVEY_LC1 <- ifelse(s2022$POINT_ID %in% to_be_imputed$POINT_ID,to_be_imputed$land_cover,s2022$SURVEY_LC1)
xtabs(~SURVEY_LC1,data=s2022)
xtabs(~SURVEY_LU1,data=s2022)


######################################################################
# New imputation of A12
# a11_12 <- read.xlsx("A11_A12_cj.xlsx",sheet = 1)
# table(a11_12$land_cover)
# s2022$SURVEY_LC1[s2022$POINT_ID %in% a11_12$POINT_ID] <- a11_12$land_cover
# xtabs(~SURVEY_LC1,data=s2022)
a11_12 <-read.xlsx("A11_A12_final.xlsx")
a11_12 <- a11_12[order(a11_12$POINT_ID),]
addmargins(xtabs(~land_cover+NUTS0_24,data=a11_12))
# land_cover  BE  BG  CY  CZ  DE  DK  EE  EL  FR  HR  IE  IT  NL  PL  PT  RO Sum
#        A11  24   6   2  11  49   6   1  10  29   2   2  29   1  26   5  23 226
#        A12   0   0   1   4  11   0   0   2   3   1   1   5  24   1   3   2  58
#        Sum  24   6   3  15  60   6   1  12  32   3   3  34  25  27   8  25 284
a <- s2022[s2022$POINT_ID %in% a11_12$POINT_ID,]
addmargins(xtabs(~SURVEY_LC1+POINT_NUTS0,data=a))
# SURVEY_LC1  BE  BG  CY  CZ  DE  DK  EE  EL  FR  HR  IE  IT  NL  PL  PT  RO Sum
#        A11  24   6   3  15  60   5   1  12  32   3   3  34   0  27   8  25 258
#        A12   0   0   0   0   0   1   0   0   0   0   0   0  24   0   0   0  25
#        A30   0   0   0   0   0   0   0   0   0   0   0   0   1   0   0   0   1
#        Sum  24   6   3  15  60   6   1  12  32   3   3  34  25  27   8  25 284
a$SURVEY_LC1[a$POINT_ID %in% a$POINT_ID] <- a11_12$land_cover 
addmargins(xtabs(~SURVEY_LC1+POINT_NUTS0,data=a))
# SURVEY_LC1  BE  BG  CY  CZ  DE  DK  EE  EL  FR  HR  IE  IT  NL  PL  PT  RO Sum
#        A11  24   6   2  11  49   6   1  10  29   2   2  29   1  26   5  23 226
#        A12   0   0   1   4  11   0   0   2   3   1   1   5  24   1   3   2  58
#        Sum  24   6   3  15  60   6   1  12  32   3   3  34  25  27   8  25 284
s2022$SURVEY_LC1[s2022$POINT_ID %in% a11_12$POINT_ID] <- a11_12$land_cover
addmargins(xtabs(~SURVEY_LC1+POINT_NUTS0,data=s2022[s2022$POINT_ID %in% a$POINT_ID,]))
# SURVEY_LC1  BE  BG  CY  CZ  DE  DK  EE  EL  FR  HR  IE  IT  NL  PL  PT  RO Sum
#        A11  24   6   2  11  49   6   1  10  29   2   2  29   1  26   5  23 226
#        A12   0   0   1   4  11   0   0   2   3   1   1   5  24   1   3   2  58
#        Sum  24   6   3  15  60   6   1  12  32   3   3  34  25  27   8  25 284
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
table(s2022$settlement)

# Calculate settl_pc
table(as.numeric(s2022$settlement))
# s2022$settl_pc <- (as.numeric(s2022$settlement)) * 1000 / s2022$Pop2023
s2022$settl_pc <- (as.numeric(s2022$settlement)) / s2022$Pop2023
summary(s2022$settl_pc)

write.table(s2022,"LUCAS22_corrected_complete.csv",sep=",",quote=F,row.names=F)
