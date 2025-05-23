#-----------------------------------------------------------
# Script to compare estimate trends at EU level (22, 26, 26) 
#-----------------------------------------------------------
#----------------------------------------------------------
# Input: ./estimates2009/
#        ./estimates2012/
#        ./estimates2015/
#        ./estimates2018/
#        ./4.TwoPhaseEstimates2022
#        EU_structure.csv
# Output: ./7.EU_estimates/Europe_estimates.xlsx
#----------------------------------------------------------
library(ReGenesees)
library(data.table)
options(stringsAsFactors = TRUE)
options(scipen=100)
if ("xlsx" %in% loadedNamespaces()){
  detach("package:xlsx", unload = TRUE)
}
library(openxlsx)
library(data.table)
EU_structure <- read.csv("EU_structure.csv")
EU_structure <- EU_structure[-nrow(EU_structure),]

dire <- getwd()
direnew1 <- paste(dire, "./7.EU_estimates/", sep = "")
if (dir.exists(direnew1))
  unlink(direnew1,recursive=TRUE)
if (!dir.exists(direnew1))
  dir.create(direnew1)

# Create a new workbook
wb <- createWorkbook()
# Add two worksheets
addWorksheet(wb, "Europe22")
addWorksheet(wb, "Europe26")
addWorksheet(wb, "Europe27")


#-------------------------------------
# Europe 22 countries
#-------------------------------------
EU22 <- EU_structure$EU23_2009[!is.na(EU_structure$EU23_2009)]
EU22
length(EU22)
#-------------------------------------
# LUCAS 2009
#-------------------------------------
path_input <- "./estimates2009/"
est <- NULL
for (k in EU22) {
  country <- read.csv(paste0(path_input,k,"_est_LC1_LU1_2009.csv"))
  country <- country[,c(1:3)]
  est <- rbind(est,country)
}
tot2009 <- aggregate(cbind(Total,SE)~Variable,data=est,FUN=sum)
tot2009$Variable <- gsub("SURVEY_","",tot2009$Variable)
tot2009$CV <- tot2009$SE / tot2009$Total
tot2009$CI.l <- tot2009$Total - tot2009$SE * 1.96
tot2009$CI.u <- tot2009$Total + tot2009$SE * 1.96
colnames(tot2009)[2] <- "Area2009"
colnames(tot2009)[3:6] <- paste0(colnames(tot2009)[3:6],"_2009")
#-------------------------------------
# Compute settl_pc
pop <- read.csv("EU_population_2009_2023.csv")
tot2009$Area2009[tot2009$Variable=="settl_pc"] <- tot2009$Area2009[tot2009$Variable=="settlement1"] / pop$Pop2009[pop$NUTS0=="EU23_2009"]
# V=var(sett / pop ) = 1/(pop)^2 * V(sett)
# V(sett) è l'SE restituito da Regenesees al quadrato. 
# SE=sqrt(V)
# CV = SE/settl_pc
# settl_pc+-1.96*SE
tot2009$SE_2009[tot2009$Variable=="settl_pc"] <- sqrt( (1 / pop$Pop2009[pop$NUTS0=="EU23_2009"]^2 )  * (tot2009$SE[tot2009$Variable=="settlement1"])^2 )
tot2009$CV_2009[tot2009$Variable=="settl_pc"] <- tot2009$SE_2009[tot2009$Variable=="settl_pc"] / tot2009$Area2009[tot2009$Variable=="settl_pc"]
tot2009$CI.l_2009[tot2009$Variable=="settl_pc"] <- tot2009$Area2009[tot2009$Variable=="settl_pc"] - 1.96 * tot2009$SE_2009[tot2009$Variable=="settl_pc"]
tot2009$CI.u_2009[tot2009$Variable=="settl_pc"] <- tot2009$Area2009[tot2009$Variable=="settl_pc"] + 1.96 * tot2009$SE_2009[tot2009$Variable=="settl_pc"]
tot2009[tot2009$Variable=="settl_pc",] 
#-------------------------------------
# Compute settl_proportion
settl_proportion <- tot2009$Area2009[tot2009$Variable=="settlement1"] / (tot2009$Area2009[tot2009$Variable=="settlement0"] + tot2009$Area2009[tot2009$Variable=="settlement1"])
# V=var(sett1 / (sett0+sett1) ) = 1/(sett0+sett1)^2 * V(sett1)
settl_proportion_SE <- sqrt( (1 / (tot2009$Area2009[tot2009$Variable=="settlement0"] + tot2009$Area2009[tot2009$Variable=="settlement1"])^2 )  * (tot2009$SE[tot2009$Variable=="settlement1"])^2 )
settl_proportion_CV <- settl_proportion_SE / settl_proportion
settl_proportion_CI.l <- settl_proportion - 1.96 * settl_proportion_SE
settl_proportion_CI.u <- settl_proportion + 1.96 * settl_proportion_SE
tot2009 <- rbind(tot2009,c("settl_proportion",settl_proportion,settl_proportion_SE,settl_proportion_CV,settl_proportion_CI.l,settl_proportion_CI.u))
tot2009[, 2:6] <- lapply(tot2009[, 2:6], as.numeric)



#-------------------------------------
# LUCAS 2012
#-------------------------------------
path_input <- "./estimates2012/"
est <- NULL
for (k in EU22) {
  country <- read.csv(paste0(path_input,k,"_est_LC1_LU1_2012.csv"))
  country <- country[,c(1:3)]
  est <- rbind(est,country)
}
tot2012 <- aggregate(cbind(Total,SE)~Variable,data=est,FUN=sum)
tot2012$Variable <- gsub("SURVEY_","",tot2012$Variable)
tot2012$CV <- tot2012$SE / tot2012$Total
tot2012$CI.l <- tot2012$Total - tot2012$SE * 1.96
tot2012$CI.u <- tot2012$Total + tot2012$SE * 1.96
colnames(tot2012)[2] <- "Area2012"
colnames(tot2012)[3:6] <- paste0(colnames(tot2012)[3:6],"_2012")
#-------------------------------------
# Compute settl_pc
tot2012$Area2012[tot2012$Variable=="settl_pc"] <- tot2012$Area2012[tot2012$Variable=="settlement1"] / pop$Pop2012[pop$NUTS0=="EU23_2009"]
tot2012$SE_2012[tot2012$Variable=="settl_pc"] <- sqrt( (1 / pop$Pop2012[pop$NUTS0=="EU23_2009"]^2 )  * (tot2012$SE[tot2012$Variable=="settlement1"])^2 )
tot2012$CV_2012[tot2012$Variable=="settl_pc"] <- tot2012$SE_2012[tot2012$Variable=="settl_pc"] / tot2012$Area2012[tot2012$Variable=="settl_pc"]
tot2012$CI.l_2012[tot2012$Variable=="settl_pc"] <- tot2012$Area2012[tot2012$Variable=="settl_pc"] - 1.96 * tot2012$SE_2012[tot2012$Variable=="settl_pc"]
tot2012$CI.u_2012[tot2012$Variable=="settl_pc"] <- tot2012$Area2012[tot2012$Variable=="settl_pc"] + 1.96 * tot2012$SE_2012[tot2012$Variable=="settl_pc"]
tot2012[tot2012$Variable=="settl_pc",] 
#-------------------------------------
# Compute settl_proportion
settl_proportion <- tot2012$Area2012[tot2012$Variable=="settlement1"] / (tot2012$Area2012[tot2012$Variable=="settlement0"] + tot2012$Area2012[tot2012$Variable=="settlement1"])
# V=var(sett1 / (sett0+sett1) ) = 1/(sett0+sett1)^2 * V(sett1)
settl_proportion_SE <- sqrt( (1 / (tot2012$Area2012[tot2012$Variable=="settlement0"] + tot2012$Area2012[tot2012$Variable=="settlement1"])^2 )  * (tot2012$SE[tot2012$Variable=="settlement1"])^2 )
settl_proportion_CV <- settl_proportion_SE / settl_proportion
settl_proportion_CI.l <- settl_proportion - 1.96 * settl_proportion_SE
settl_proportion_CI.u <- settl_proportion + 1.96 * settl_proportion_SE
tot2012 <- rbind(tot2012,c("settl_proportion",settl_proportion,settl_proportion_SE,settl_proportion_CV,settl_proportion_CI.l,settl_proportion_CI.u))
tot2012[, 2:6] <- lapply(tot2012[, 2:6], as.numeric)

#-------------------------------------
# LUCAS 2015
#-------------------------------------
path_input <- "./estimates2015/"
est <- NULL
for (k in EU22) {
  country <- read.csv(paste0(path_input,k,"_est_LC1_LU1_2015.csv"))
  country <- country[,c(1:3)]
  est <- rbind(est,country)
}
tot2015 <- aggregate(cbind(Total,SE)~Variable,data=est,FUN=sum)
tot2015$Variable <- gsub("SURVEY_","",tot2015$Variable)
tot2015$CV <- tot2015$SE / tot2015$Total
tot2015$CI.l <- tot2015$Total - tot2015$SE * 1.96
tot2015$CI.u <- tot2015$Total + tot2015$SE * 1.96
colnames(tot2015)[2] <- "Area2015"
colnames(tot2015)[3:6] <- paste0(colnames(tot2015)[3:6],"_2015")
#-------------------------------------
# Compute settl_pc
tot2015$Area2015[tot2015$Variable=="settl_pc"] <- tot2015$Area2015[tot2015$Variable=="settlement1"] / pop$Pop2015[pop$NUTS0=="EU23_2009"]
tot2015$SE_2015[tot2015$Variable=="settl_pc"] <- sqrt( (1 / pop$Pop2015[pop$NUTS0=="EU23_2009"]^2 )  * (tot2015$SE[tot2015$Variable=="settlement1"])^2 )
tot2015$CV_2015[tot2015$Variable=="settl_pc"] <- tot2015$SE_2015[tot2015$Variable=="settl_pc"] / tot2015$Area2015[tot2015$Variable=="settl_pc"]
tot2015$CI.l_2015[tot2015$Variable=="settl_pc"] <- tot2015$Area2015[tot2015$Variable=="settl_pc"] - 1.96 * tot2015$SE_2015[tot2015$Variable=="settl_pc"]
tot2015$CI.u_2015[tot2015$Variable=="settl_pc"] <- tot2015$Area2015[tot2015$Variable=="settl_pc"] + 1.96 * tot2015$SE_2015[tot2015$Variable=="settl_pc"]
tot2015[tot2015$Variable=="settl_pc",] 
#-------------------------------------
# Compute settl_proportion
settl_proportion <- tot2015$Area2015[tot2015$Variable=="settlement1"] / (tot2015$Area2015[tot2015$Variable=="settlement0"] + tot2015$Area2015[tot2015$Variable=="settlement1"])
# V=var(sett1 / (sett0+sett1) ) = 1/(sett0+sett1)^2 * V(sett1)
settl_proportion_SE <- sqrt( (1 / (tot2015$Area2015[tot2015$Variable=="settlement0"] + tot2015$Area2015[tot2015$Variable=="settlement1"])^2 )  * (tot2015$SE[tot2015$Variable=="settlement1"])^2 )
settl_proportion_CV <- settl_proportion_SE / settl_proportion
settl_proportion_CI.l <- settl_proportion - 1.96 * settl_proportion_SE
settl_proportion_CI.u <- settl_proportion + 1.96 * settl_proportion_SE
tot2015 <- rbind(tot2015,c("settl_proportion",settl_proportion,settl_proportion_SE,settl_proportion_CV,settl_proportion_CI.l,settl_proportion_CI.u))
tot2015[, 2:6] <- lapply(tot2015[, 2:6], as.numeric)


#-------------------------------------
# LUCAS 2018
#-------------------------------------
path_input <- "./estimates2018/"
est <- NULL
for (k in EU22) {
  country <- read.csv(paste0(path_input,k,"_est_LC1_LU1_2018.csv"))
  country <- country[,c(1:3)]
  est <- rbind(est,country)
}
tot2018 <- aggregate(cbind(Total,SE)~Variable,data=est,FUN=sum)
tot2018$Variable <- gsub("SURVEY_","",tot2018$Variable)
tot2018$CV <- tot2018$SE / tot2018$Total
tot2018$CI.l <- tot2018$Total - tot2018$SE * 1.96
tot2018$CI.u <- tot2018$Total + tot2018$SE * 1.96
colnames(tot2018)[2] <- "Area2018"
colnames(tot2018)[3:6] <- paste0(colnames(tot2018)[3:6],"_2018")
#-------------------------------------
# Compute settl_pc
tot2018$Area2018[tot2018$Variable=="settl_pc"] <- tot2018$Area2018[tot2018$Variable=="settlement1"] / pop$Pop2018[pop$NUTS0=="EU23_2009"]
tot2018$SE_2018[tot2018$Variable=="settl_pc"] <- sqrt( (1 / pop$Pop2018[pop$NUTS0=="EU23_2009"]^2 )  * (tot2018$SE[tot2018$Variable=="settlement1"])^2 )
tot2018$CV_2018[tot2018$Variable=="settl_pc"] <- tot2018$SE_2018[tot2018$Variable=="settl_pc"] / tot2018$Area2018[tot2018$Variable=="settl_pc"]
tot2018$CI.l_2018[tot2018$Variable=="settl_pc"] <- tot2018$Area2018[tot2018$Variable=="settl_pc"] - 1.96 * tot2018$SE_2018[tot2018$Variable=="settl_pc"]
tot2018$CI.u_2018[tot2018$Variable=="settl_pc"] <- tot2018$Area2018[tot2018$Variable=="settl_pc"] + 1.96 * tot2018$SE_2018[tot2018$Variable=="settl_pc"]
tot2018[tot2018$Variable=="settl_pc",] 
#-------------------------------------
# Compute settl_proportion
settl_proportion <- tot2018$Area2018[tot2018$Variable=="settlement1"] / (tot2018$Area2018[tot2018$Variable=="settlement0"] + tot2018$Area2018[tot2018$Variable=="settlement1"])
# V=var(sett1 / (sett0+sett1) ) = 1/(sett0+sett1)^2 * V(sett1)
settl_proportion_SE <- sqrt( (1 / (tot2018$Area2018[tot2018$Variable=="settlement0"] + tot2018$Area2018[tot2018$Variable=="settlement1"])^2 )  * (tot2018$SE[tot2018$Variable=="settlement1"])^2 )
settl_proportion_CV <- settl_proportion_SE / settl_proportion
settl_proportion_CI.l <- settl_proportion - 1.96 * settl_proportion_SE
settl_proportion_CI.u <- settl_proportion + 1.96 * settl_proportion_SE
tot2018 <- rbind(tot2018,c("settl_proportion",settl_proportion,settl_proportion_SE,settl_proportion_CV,settl_proportion_CI.l,settl_proportion_CI.u))
tot2018[, 2:6] <- lapply(tot2018[, 2:6], as.numeric)


#-------------------------------------
# LUCAS 2022
#-------------------------------------
est <- NULL
for (k in EU22) {
  country <- read.csv(paste0("./4.TwoPhaseEstimates2022/",k,"_est_LC1_LU1_2022.csv"))
  country <- country[,c(1:3)]
  est <- rbind(est,country)
}
tot2022 <- aggregate(cbind(Total,SE)~Variable,data=est,FUN=sum)
tot2022$Variable <- gsub("SURVEY_","",tot2022$Variable)
tot2022$CV <- tot2022$SE / tot2022$Total
tot2022$CI.l <- tot2022$Total - tot2022$SE * 1.96
tot2022$CI.u <- tot2022$Total + tot2022$SE * 1.96
colnames(tot2022)[2] <- "Area2022"
colnames(tot2022)[3:6] <- paste0(colnames(tot2022)[3:6],"_2022")
#-------------------------------------
# Compute settl_pc
tot2022$Area2022[tot2022$Variable=="settl_pc"] <- tot2022$Area2022[tot2022$Variable=="settlement1"] / pop$Pop2023[pop$NUTS0=="EU23_2009"]
tot2022$SE_2022[tot2022$Variable=="settl_pc"] <- sqrt( (1 / pop$Pop2023[pop$NUTS0=="EU23_2009"]^2 )  * (tot2022$SE[tot2022$Variable=="settlement1"])^2 )
tot2022$CV_2022[tot2022$Variable=="settl_pc"] <- tot2022$SE_2022[tot2022$Variable=="settl_pc"] / tot2022$Area2022[tot2022$Variable=="settl_pc"]
tot2022$CI.l_2022[tot2022$Variable=="settl_pc"] <- tot2022$Area2022[tot2022$Variable=="settl_pc"] - 1.96 * tot2022$SE_2022[tot2022$Variable=="settl_pc"]
tot2022$CI.u_2022[tot2022$Variable=="settl_pc"] <- tot2022$Area2022[tot2022$Variable=="settl_pc"] + 1.96 * tot2022$SE_2022[tot2022$Variable=="settl_pc"]
tot2022[tot2022$Variable=="settl_pc",] 
#-------------------------------------
# Compute settl_proportion
settl_proportion <- tot2022$Area2022[tot2022$Variable=="settlement1"] / (tot2022$Area2022[tot2022$Variable=="settlement0"] + tot2022$Area2022[tot2022$Variable=="settlement1"])
# V=var(sett1 / (sett0+sett1) ) = 1/(sett0+sett1)^2 * V(sett1)
settl_proportion_SE <- sqrt( (1 / (tot2022$Area2022[tot2022$Variable=="settlement0"] + tot2022$Area2022[tot2022$Variable=="settlement1"])^2 )  * (tot2022$SE[tot2022$Variable=="settlement1"])^2 )
settl_proportion_CV <- settl_proportion_SE / settl_proportion
settl_proportion_CI.l <- settl_proportion - 1.96 * settl_proportion_SE
settl_proportion_CI.u <- settl_proportion + 1.96 * settl_proportion_SE
tot2022 <- rbind(tot2022,c("settl_proportion",settl_proportion,settl_proportion_SE,settl_proportion_CV,settl_proportion_CI.l,settl_proportion_CI.u))
tot2022[, 2:6] <- lapply(tot2022[, 2:6], as.numeric)


tot22 <- merge(tot2009,tot2012,by="Variable",all.x=TRUE,all.y=TRUE)
tot22 <- merge(tot22,tot2015,by="Variable",all.x=TRUE,all.y=TRUE)
tot22 <- merge(tot22,tot2018,by="Variable",all.x=TRUE,all.y=TRUE)
tot22 <- merge(tot22,tot2022,by="Variable",all.x=TRUE,all.y=TRUE)


# Write the summary to the first sheet
writeData(wb, sheet = "Europe22", tot22)

##################################################################

#-------------------------------------
# Europe 26 countries
#-------------------------------------
EU26 <- EU_structure$EU27_2012[!is.na(EU_structure$EU27_2012)]
EU26
length(EU26)

#-------------------------------------
# LUCAS 2012
#-------------------------------------
path_input <- "./estimates2012/"
est <- NULL
for (k in EU26) {
  country <- read.csv(paste0(path_input,k,"_est_LC1_LU1_2012.csv"))
  country <- country[,c(1:3)]
  est <- rbind(est,country)
}
tot2012 <- aggregate(cbind(Total,SE)~Variable,data=est,FUN=sum)
tot2012$Variable <- gsub("SURVEY_","",tot2012$Variable)
tot2012$CV <- tot2012$SE / tot2012$Total
tot2012$CI.l <- tot2012$Total - tot2012$SE * 1.96
tot2012$CI.u <- tot2012$Total + tot2012$SE * 1.96
colnames(tot2012)[2] <- "Area2012"
colnames(tot2012)[3:6] <- paste0(colnames(tot2012)[3:6],"_2012")
#-------------------------------------
# Compute settl_pc
tot2012$Area2012[tot2012$Variable=="settl_pc"] <- tot2012$Area2012[tot2012$Variable=="settlement1"] / pop$Pop2012[pop$NUTS0=="EU27_2012"]
tot2012$SE_2012[tot2012$Variable=="settl_pc"] <- sqrt( (1 / pop$Pop2012[pop$NUTS0=="EU27_2012"]^2 )  * (tot2012$SE[tot2012$Variable=="settlement1"])^2 )
tot2012$CV_2012[tot2012$Variable=="settl_pc"] <- tot2012$SE_2012[tot2012$Variable=="settl_pc"] / tot2012$Area2012[tot2012$Variable=="settl_pc"]
tot2012$CI.l_2012[tot2012$Variable=="settl_pc"] <- tot2012$Area2012[tot2012$Variable=="settl_pc"] - 1.96 * tot2012$SE_2012[tot2012$Variable=="settl_pc"]
tot2012$CI.u_2012[tot2012$Variable=="settl_pc"] <- tot2012$Area2012[tot2012$Variable=="settl_pc"] + 1.96 * tot2012$SE_2012[tot2012$Variable=="settl_pc"]
tot2012[tot2012$Variable=="settl_pc",] 
#-------------------------------------
# Compute settl_proportion
settl_proportion <- tot2012$Area2012[tot2012$Variable=="settlement1"] / (tot2012$Area2012[tot2012$Variable=="settlement0"] + tot2012$Area2012[tot2012$Variable=="settlement1"])
settl_proportion_SE <- sqrt( (1 / (tot2012$Area2012[tot2012$Variable=="settlement0"] + tot2012$Area2012[tot2012$Variable=="settlement1"])^2 )  * (tot2012$SE[tot2012$Variable=="settlement1"])^2 )
settl_proportion_CV <- settl_proportion_SE / settl_proportion
settl_proportion_CI.l <- settl_proportion - 1.96 * settl_proportion_SE
settl_proportion_CI.u <- settl_proportion + 1.96 * settl_proportion_SE
tot2012 <- rbind(tot2012,c("settl_proportion",settl_proportion,settl_proportion_SE,settl_proportion_CV,settl_proportion_CI.l,settl_proportion_CI.u))
tot2012[, 2:6] <- lapply(tot2012[, 2:6], as.numeric)


#-------------------------------------
# LUCAS 2015
#-------------------------------------
path_input <- "./estimates2015/"
est <- NULL
for (k in EU26) {
  country <- read.csv(paste0(path_input,k,"_est_LC1_LU1_2015.csv"))
  country <- country[,c(1:3)]
  est <- rbind(est,country)
}
tot2015 <- aggregate(cbind(Total,SE)~Variable,data=est,FUN=sum)
tot2015$Variable <- gsub("SURVEY_","",tot2015$Variable)
tot2015$CV <- tot2015$SE / tot2015$Total
tot2015$CI.l <- tot2015$Total - tot2015$SE * 1.96
tot2015$CI.u <- tot2015$Total + tot2015$SE * 1.96
colnames(tot2015)[2] <- "Area2015"
colnames(tot2015)[3:6] <- paste0(colnames(tot2015)[3:6],"_2015")
#-------------------------------------
# Compute settl_pc
tot2015$Area2015[tot2015$Variable=="settl_pc"] <- tot2015$Area2015[tot2015$Variable=="settlement1"] / pop$Pop2015[pop$NUTS0=="EU27_2012"]
tot2015$SE_2015[tot2015$Variable=="settl_pc"] <- sqrt( (1 / pop$Pop2015[pop$NUTS0=="EU27_2012"]^2 )  * (tot2015$SE[tot2015$Variable=="settlement1"])^2 )
tot2015$CV_2015[tot2015$Variable=="settl_pc"] <- tot2015$SE_2015[tot2015$Variable=="settl_pc"] / tot2015$Area2015[tot2015$Variable=="settl_pc"]
tot2015$CI.l_2015[tot2015$Variable=="settl_pc"] <- tot2015$Area2015[tot2015$Variable=="settl_pc"] - 1.96 * tot2015$SE_2015[tot2015$Variable=="settl_pc"]
tot2015$CI.u_2015[tot2015$Variable=="settl_pc"] <- tot2015$Area2015[tot2015$Variable=="settl_pc"] + 1.96 * tot2015$SE_2015[tot2015$Variable=="settl_pc"]
tot2015[tot2015$Variable=="settl_pc",] 
#-------------------------------------
# Compute settl_proportion
settl_proportion <- tot2015$Area2015[tot2015$Variable=="settlement1"] / (tot2015$Area2015[tot2015$Variable=="settlement0"] + tot2015$Area2015[tot2015$Variable=="settlement1"])
settl_proportion_SE <- sqrt( (1 / (tot2015$Area2015[tot2015$Variable=="settlement0"] + tot2015$Area2015[tot2015$Variable=="settlement1"])^2 )  * (tot2015$SE[tot2015$Variable=="settlement1"])^2 )
settl_proportion_CV <- settl_proportion_SE / settl_proportion
settl_proportion_CI.l <- settl_proportion - 1.96 * settl_proportion_SE
settl_proportion_CI.u <- settl_proportion + 1.96 * settl_proportion_SE
tot2015 <- rbind(tot2015,c("settl_proportion",settl_proportion,settl_proportion_SE,settl_proportion_CV,settl_proportion_CI.l,settl_proportion_CI.u))
tot2015[, 2:6] <- lapply(tot2015[, 2:6], as.numeric)


#-------------------------------------
# LUCAS 2018
#-------------------------------------
path_input <- "./estimates2018/"
est <- NULL
for (k in EU26) {
  country <- read.csv(paste0(path_input,k,"_est_LC1_LU1_2018.csv"))
  country <- country[,c(1:3)]
  est <- rbind(est,country)
}
tot2018 <- aggregate(cbind(Total,SE)~Variable,data=est,FUN=sum)
tot2018$Variable <- gsub("SURVEY_","",tot2018$Variable)
tot2018$CV <- tot2018$SE / tot2018$Total
tot2018$CI.l <- tot2018$Total - tot2018$SE * 1.96
tot2018$CI.u <- tot2018$Total + tot2018$SE * 1.96
colnames(tot2018)[2] <- "Area2018"
colnames(tot2018)[3:6] <- paste0(colnames(tot2018)[3:6],"_2018")
#-------------------------------------
# Compute settl_pc
tot2018$Area2018[tot2018$Variable=="settl_pc"] <- tot2018$Area2018[tot2018$Variable=="settlement1"] / pop$Pop2018[pop$NUTS0=="EU27_2012"]
tot2018$SE_2018[tot2018$Variable=="settl_pc"] <- sqrt( (1 / pop$Pop2018[pop$NUTS0=="EU27_2012"]^2 )  * (tot2018$SE[tot2018$Variable=="settlement1"])^2 )
tot2018$CV_2018[tot2018$Variable=="settl_pc"] <- tot2018$SE_2018[tot2018$Variable=="settl_pc"] / tot2018$Area2018[tot2018$Variable=="settl_pc"]
tot2018$CI.l_2018[tot2018$Variable=="settl_pc"] <- tot2018$Area2018[tot2018$Variable=="settl_pc"] - 1.96 * tot2018$SE_2018[tot2018$Variable=="settl_pc"]
tot2018$CI.u_2018[tot2018$Variable=="settl_pc"] <- tot2018$Area2018[tot2018$Variable=="settl_pc"] + 1.96 * tot2018$SE_2018[tot2018$Variable=="settl_pc"]
tot2018[tot2018$Variable=="settl_pc",] 
#-------------------------------------
# Compute settl_proportion
settl_proportion <- tot2018$Area2018[tot2018$Variable=="settlement1"] / (tot2018$Area2018[tot2018$Variable=="settlement0"] + tot2018$Area2018[tot2018$Variable=="settlement1"])
settl_proportion_SE <- sqrt( (1 / (tot2018$Area2018[tot2018$Variable=="settlement0"] + tot2018$Area2018[tot2018$Variable=="settlement1"])^2 )  * (tot2018$SE[tot2018$Variable=="settlement1"])^2 )
settl_proportion_CV <- settl_proportion_SE / settl_proportion
settl_proportion_CI.l <- settl_proportion - 1.96 * settl_proportion_SE
settl_proportion_CI.u <- settl_proportion + 1.96 * settl_proportion_SE
tot2018 <- rbind(tot2018,c("settl_proportion",settl_proportion,settl_proportion_SE,settl_proportion_CV,settl_proportion_CI.l,settl_proportion_CI.u))
tot2018[, 2:6] <- lapply(tot2018[, 2:6], as.numeric)

#-------------------------------------
# LUCAS 2022
#-------------------------------------
est <- NULL
for (k in EU26) {
  country <- read.csv(paste0("./4.TwoPhaseEstimates2022/",k,"_est_LC1_LU1_2022.csv"))
  country <- country[,c(1:3)]
  est <- rbind(est,country)
}
tot2022 <- aggregate(cbind(Total,SE)~Variable,data=est,FUN=sum)
tot2022$Variable <- gsub("SURVEY_","",tot2022$Variable)
tot2022$CV <- tot2022$SE / tot2022$Total
tot2022$CI.l <- tot2022$Total - tot2022$SE * 1.96
tot2022$CI.u <- tot2022$Total + tot2022$SE * 1.96
colnames(tot2022)[2] <- "Area2022"
colnames(tot2022)[3:6] <- paste0(colnames(tot2022)[3:6],"_2022")
#-------------------------------------
# Compute settl_pc
tot2022$Area2022[tot2022$Variable=="settl_pc"] <- tot2022$Area2022[tot2022$Variable=="settlement1"] / pop$Pop2023[pop$NUTS0=="EU27_2012"]
tot2022$SE_2022[tot2022$Variable=="settl_pc"] <- sqrt( (1 / pop$Pop2023[pop$NUTS0=="EU27_2012"]^2 )  * (tot2022$SE[tot2022$Variable=="settlement1"])^2 )
tot2022$CV_2022[tot2022$Variable=="settl_pc"] <- tot2022$SE_2022[tot2022$Variable=="settl_pc"] / tot2022$Area2022[tot2022$Variable=="settl_pc"]
tot2022$CI.l_2022[tot2022$Variable=="settl_pc"] <- tot2022$Area2022[tot2022$Variable=="settl_pc"] - 1.96 * tot2022$SE_2022[tot2022$Variable=="settl_pc"]
tot2022$CI.u_2022[tot2022$Variable=="settl_pc"] <- tot2022$Area2022[tot2022$Variable=="settl_pc"] + 1.96 * tot2022$SE_2022[tot2022$Variable=="settl_pc"]
tot2022[tot2022$Variable=="settl_pc",] 
#-------------------------------------
# Compute settl_proportion
settl_proportion <- tot2022$Area2022[tot2022$Variable=="settlement1"] / (tot2022$Area2022[tot2022$Variable=="settlement0"] + tot2022$Area2022[tot2022$Variable=="settlement1"])
# V=var(sett1 / (sett0+sett1) ) = 1/(sett0+sett1)^2 * V(sett1)
settl_proportion_SE <- sqrt( (1 / (tot2022$Area2022[tot2022$Variable=="settlement0"] + tot2022$Area2022[tot2022$Variable=="settlement1"])^2 )  * (tot2022$SE[tot2022$Variable=="settlement1"])^2 )
settl_proportion_CV <- settl_proportion_SE / settl_proportion
settl_proportion_CI.l <- settl_proportion - 1.96 * settl_proportion_SE
settl_proportion_CI.u <- settl_proportion + 1.96 * settl_proportion_SE
tot2022 <- rbind(tot2022,c("settl_proportion",settl_proportion,settl_proportion_SE,settl_proportion_CV,settl_proportion_CI.l,settl_proportion_CI.u))
tot2022[, 2:6] <- lapply(tot2022[, 2:6], as.numeric)





tot26 <- merge(tot2012,tot2015,by="Variable",all.x=TRUE,all.y=TRUE)
tot26 <- merge(tot26,tot2018,by="Variable",all.x=TRUE,all.y=TRUE)
tot26 <- merge(tot26,tot2022,by="Variable",all.x=TRUE,all.y=TRUE)

# Write the summary to the first sheet
writeData(wb, sheet = "Europe26", tot26)


##################################################################

#-------------------------------------
# Europe 27 countries
#-------------------------------------
EU27 <- EU_structure$EU27_2020[!is.na(EU_structure$EU27_2020)]
EU27
length(EU27)


#-------------------------------------
# LUCAS 2015
#-------------------------------------
path_input <- "./estimates2015/"
est <- NULL
for (k in EU27) {
  country <- read.csv(paste0(path_input,k,"_est_LC1_LU1_2015.csv"))
  country <- country[,c(1:3)]
  est <- rbind(est,country)
}
tot2015 <- aggregate(cbind(Total,SE)~Variable,data=est,FUN=sum)
tot2015$Variable <- gsub("SURVEY_","",tot2015$Variable)
tot2015$CV <- tot2015$SE / tot2015$Total
tot2015$CI.l <- tot2015$Total - tot2015$SE * 1.96
tot2015$CI.u <- tot2015$Total + tot2015$SE * 1.96
colnames(tot2015)[2] <- "Area2015"
colnames(tot2015)[3:6] <- paste0(colnames(tot2015)[3:6],"_2015")
#-------------------------------------
# Compute settl_pc
tot2015$Area2015[tot2015$Variable=="settl_pc"] <- tot2015$Area2015[tot2015$Variable=="settlement1"] / pop$Pop2015[pop$NUTS0=="EU27_2020"]
tot2015$SE_2015[tot2015$Variable=="settl_pc"] <- sqrt( (1 / pop$Pop2015[pop$NUTS0=="EU27_2020"]^2 )  * (tot2015$SE[tot2015$Variable=="settlement1"])^2 )
tot2015$CV_2015[tot2015$Variable=="settl_pc"] <- tot2015$SE_2015[tot2015$Variable=="settl_pc"] / tot2015$Area2015[tot2015$Variable=="settl_pc"]
tot2015$CI.l_2015[tot2015$Variable=="settl_pc"] <- tot2015$Area2015[tot2015$Variable=="settl_pc"] - 1.96 * tot2015$SE_2015[tot2015$Variable=="settl_pc"]
tot2015$CI.u_2015[tot2015$Variable=="settl_pc"] <- tot2015$Area2015[tot2015$Variable=="settl_pc"] + 1.96 * tot2015$SE_2015[tot2015$Variable=="settl_pc"]
tot2015[tot2015$Variable=="settl_pc",] 
#-------------------------------------
# Compute settl_proportion
settl_proportion <- tot2015$Area2015[tot2015$Variable=="settlement1"] / (tot2015$Area2015[tot2015$Variable=="settlement0"] + tot2015$Area2015[tot2015$Variable=="settlement1"])
# V=var(sett1 / (sett0+sett1) ) = 1/(sett0+sett1)^2 * V(sett1)
settl_proportion_SE <- sqrt( (1 / (tot2015$Area2015[tot2015$Variable=="settlement0"] + tot2015$Area2015[tot2015$Variable=="settlement1"])^2 )  * (tot2015$SE[tot2015$Variable=="settlement1"])^2 )
settl_proportion_CV <- settl_proportion_SE / settl_proportion
settl_proportion_CI.l <- settl_proportion - 1.96 * settl_proportion_SE
settl_proportion_CI.u <- settl_proportion + 1.96 * settl_proportion_SE
tot2015 <- rbind(tot2015,c("settl_proportion",settl_proportion,settl_proportion_SE,settl_proportion_CV,settl_proportion_CI.l,settl_proportion_CI.u))
tot2015[, 2:6] <- lapply(tot2015[, 2:6], as.numeric)



#-------------------------------------
# LUCAS 2018
#-------------------------------------
path_input <- "./estimates2018/"
est <- NULL
for (k in EU27) {
  country <- read.csv(paste0(path_input,k,"_est_LC1_LU1_2018.csv"))
  country <- country[,c(1:3)]
  est <- rbind(est,country)
}
tot2018 <- aggregate(cbind(Total,SE)~Variable,data=est,FUN=sum)
tot2018$Variable <- gsub("SURVEY_","",tot2018$Variable)
tot2018$CV <- tot2018$SE / tot2018$Total
tot2018$CI.l <- tot2018$Total - tot2018$SE * 1.96
tot2018$CI.u <- tot2018$Total + tot2018$SE * 1.96
colnames(tot2018)[2] <- "Area2018"
colnames(tot2018)[3:6] <- paste0(colnames(tot2018)[3:6],"_2018")
#-------------------------------------
# Compute settl_pc
tot2018$Area2018[tot2018$Variable=="settl_pc"] <- tot2018$Area2018[tot2018$Variable=="settlement1"] / pop$Pop2018[pop$NUTS0=="EU27_2020"]
tot2018$SE_2018[tot2018$Variable=="settl_pc"] <- sqrt( (1 / pop$Pop2018[pop$NUTS0=="EU27_2020"]^2 )  * (tot2018$SE[tot2018$Variable=="settlement1"])^2 )
tot2018$CV_2018[tot2018$Variable=="settl_pc"] <- tot2018$SE_2018[tot2018$Variable=="settl_pc"] / tot2018$Area2018[tot2018$Variable=="settl_pc"]
tot2018$CI.l_2018[tot2018$Variable=="settl_pc"] <- tot2018$Area2018[tot2018$Variable=="settl_pc"] - 1.96 * tot2018$SE_2018[tot2018$Variable=="settl_pc"]
tot2018$CI.u_2018[tot2018$Variable=="settl_pc"] <- tot2018$Area2018[tot2018$Variable=="settl_pc"] + 1.96 * tot2018$SE_2018[tot2018$Variable=="settl_pc"]
tot2018[tot2018$Variable=="settl_pc",] 
#-------------------------------------
# Compute settl_proportion
settl_proportion <- tot2018$Area2018[tot2018$Variable=="settlement1"] / (tot2018$Area2018[tot2018$Variable=="settlement0"] + tot2018$Area2018[tot2018$Variable=="settlement1"])
# V=var(sett1 / (sett0+sett1) ) = 1/(sett0+sett1)^2 * V(sett1)
settl_proportion_SE <- sqrt( (1 / (tot2018$Area2018[tot2018$Variable=="settlement0"] + tot2018$Area2018[tot2018$Variable=="settlement1"])^2 )  * (tot2018$SE[tot2018$Variable=="settlement1"])^2 )
settl_proportion_CV <- settl_proportion_SE / settl_proportion
settl_proportion_CI.l <- settl_proportion - 1.96 * settl_proportion_SE
settl_proportion_CI.u <- settl_proportion + 1.96 * settl_proportion_SE
tot2018 <- rbind(tot2018,c("settl_proportion",settl_proportion,settl_proportion_SE,settl_proportion_CV,settl_proportion_CI.l,settl_proportion_CI.u))
tot2018[, 2:6] <- lapply(tot2018[, 2:6], as.numeric)



#-------------------------------------
# LUCAS 2022
#-------------------------------------
est <- NULL
for (k in EU27) {
  country <- read.csv(paste0("./4.TwoPhaseEstimates2022/",k,"_est_LC1_LU1_2022.csv"))
  country <- country[,c(1:3)]
  # a <-country[country$Variable == "SURVEY_LC1_18",]
  # if (nrow(a)) cat("\n country: ",k)
  est <- rbind(est,country)
}
a <- est[est$Variable == "SURVEY_LC1_18",]
tot2022 <- aggregate(cbind(Total,SE)~Variable,data=est,FUN=sum)
tot2022$Variable <- gsub("SURVEY_","",tot2022$Variable)
tot2022$CV <- tot2022$SE / tot2022$Total
tot2022$CI.l <- tot2022$Total - tot2022$SE * 1.96
tot2022$CI.u <- tot2022$Total + tot2022$SE * 1.96
colnames(tot2022)[2] <- "Area2022"
colnames(tot2022)[3:6] <- paste0(colnames(tot2022)[3:6],"_2022")
#-------------------------------------
# Compute settl_pc
tot2022$Area2022[tot2022$Variable=="settl_pc"] <- tot2022$Area2022[tot2022$Variable=="settlement1"] / pop$Pop2023[pop$NUTS0=="EU27_2020"]
tot2022$SE_2022[tot2022$Variable=="settl_pc"] <- sqrt( (1 / pop$Pop2023[pop$NUTS0=="EU27_2020"]^2 )  * (tot2022$SE[tot2022$Variable=="settlement1"])^2 )
tot2022$CV_2022[tot2022$Variable=="settl_pc"] <- tot2022$SE_2022[tot2022$Variable=="settl_pc"] / tot2022$Area2022[tot2022$Variable=="settl_pc"]
tot2022$CI.l_2022[tot2022$Variable=="settl_pc"] <- tot2022$Area2022[tot2022$Variable=="settl_pc"] - 1.96 * tot2022$SE_2022[tot2022$Variable=="settl_pc"]
tot2022$CI.u_2022[tot2022$Variable=="settl_pc"] <- tot2022$Area2022[tot2022$Variable=="settl_pc"] + 1.96 * tot2022$SE_2022[tot2022$Variable=="settl_pc"]
tot2022[tot2022$Variable=="settl_pc",] 
#-------------------------------------
# Compute settl_proportion
settl_proportion <- tot2022$Area2022[tot2022$Variable=="settlement1"] / (tot2022$Area2022[tot2022$Variable=="settlement0"] + tot2022$Area2022[tot2022$Variable=="settlement1"])
# V=var(sett1 / (sett0+sett1) ) = 1/(sett0+sett1)^2 * V(sett1)
settl_proportion_SE <- sqrt( (1 / (tot2022$Area2022[tot2022$Variable=="settlement0"] + tot2022$Area2022[tot2022$Variable=="settlement1"])^2 )  * (tot2022$SE[tot2022$Variable=="settlement1"])^2 )
settl_proportion_CV <- settl_proportion_SE / settl_proportion
settl_proportion_CI.l <- settl_proportion - 1.96 * settl_proportion_SE
settl_proportion_CI.u <- settl_proportion + 1.96 * settl_proportion_SE
tot2022 <- rbind(tot2022,c("settl_proportion",settl_proportion,settl_proportion_SE,settl_proportion_CV,settl_proportion_CI.l,settl_proportion_CI.u))
tot2022[, 2:6] <- lapply(tot2022[, 2:6], as.numeric)





tot27 <- merge(tot2015,tot2018,by="Variable",all.x=TRUE,all.y=TRUE)
tot27 <- merge(tot27,tot2022,by="Variable",all.x=TRUE,all.y=TRUE)

# Write the summary to the first sheet
writeData(wb, sheet = "Europe27", tot27)

# Apply styling to the header of the Diamonds Data sheet
headerStyle <- createStyle(textDecoration = "bold",halign="center", fontSize=14,fontColour = "#FFFFFF", fgFill = "#4F81BD")
bodyStyle <- createStyle(textDecoration = "bold", fontSize=12,fontColour = "#FFFFFF", fgFill = "#4F81BD")

addStyle(wb, sheet = "Europe22", style = headerStyle, rows = 1, cols = 1:ncol(tot22), gridExpand = TRUE)
addStyle(wb, sheet = "Europe22", bodyStyle, rows = 2:(nrow(tot22)+1), cols = 1, gridExpand = TRUE)

addStyle(wb, sheet = "Europe26", style = headerStyle, rows = 1, cols = 1:ncol(tot26), gridExpand = TRUE)
addStyle(wb, sheet = "Europe26", bodyStyle, rows = 2:(nrow(tot26)+1), cols = 1, gridExpand = TRUE)

addStyle(wb, sheet = "Europe27", style = headerStyle, rows = 1, cols = 1:ncol(tot27), gridExpand = TRUE)
addStyle(wb, sheet = "Europe27", bodyStyle, rows = 2:(nrow(tot27)+1), cols = 1, gridExpand = TRUE)

# Save the workbook
saveWorkbook(wb, paste0(direnew1,"Europe_estimates.xlsx"), overwrite = TRUE)

