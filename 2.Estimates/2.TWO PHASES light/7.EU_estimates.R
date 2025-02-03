#-----------------------------------------------------------
# Script to compare estimate trends at EU level (22, 26, 26) 
#-----------------------------------------------------------
options(stringsAsFactors = TRUE)
options(scipen=100)
# detach("package:xlsx", unload = TRUE)
library(openxlsx)
library(data.table)
path_output <- "D:/Google Drive/LUCAS 2025/Task 1 - ESTIMATES/2.TWO PHASES light/EU_estimates/"
setwd("D:/Google Drive/LUCAS 2025/Task 1 - ESTIMATES/2.TWO PHASES light/")
EU_structure <- read.csv("D:/Google Drive/LUCAS 2025/2.DATA/Eu_structure.csv")
# path_output <- "C:\\Users\\UTENTE\\Google Drive/LUCAS 2025/Task 1 - ESTIMATES/2.TWO PHASES light/EU_estimates/"
# setwd("C:\\Users\\UTENTE\\Google Drive/LUCAS 2025/Task 1 - ESTIMATES/2.TWO PHASES light/estimates2022")
# EU_structure <- read.csv("C:\\Users\\UTENTE\\Google Drive/LUCAS 2025/2.DATA/Eu_structure.csv")

# setwd("C:/Users/UTENTE/Desktop/Progetto_LUCAS/Task1/2.TWO PHASES light")
# path_output = "C:/Users/UTENTE/Desktop/Progetto_LUCAS/Task1/2.TWO PHASES light/EU_estimates/"
# EU_structure <- read.csv("C:/Users/UTENTE/Desktop/Progetto_LUCAS/Task1/DATA/Eu_structure.csv")
# path_previous_estimates="C:/Users/UTENTE/Desktop/Progetto_LUCAS/Task1/DATA/previous_estimates/"
path_previous_estimates="D:\\Google Drive\\LUCAS 2025\\previous estimates/"

EU_structure <- EU_structure[-nrow(EU_structure),]

dire <- getwd()
direnew1 <- paste(dire, "\\EU_estimates\\", sep = "")
# if (dir.exists(direnew1))
#   unlink(direnew1,recursive=TRUE)
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
path_input <- paste0(path_previous_estimates, "estimates2009\\")
# path_input <- "C:\\Users\\UTENTE\\Google Drive\\LUCAS 2018\\task 4\\estimates_FINAL\\estimates2009\\"
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
# LUCAS 2012
#-------------------------------------
path_input <- paste0(path_previous_estimates, "estimates2012\\")
# path_input <- "C:\\Users\\UTENTE\\Google Drive\\LUCAS 2018\\task 4\\estimates_FINAL\\estimates2012\\"
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
# LUCAS 2015
#-------------------------------------
path_input <- paste0(path_previous_estimates, "estimates2015\\")
# path_input <- "C:\\Users\\UTENTE\\Google Drive\\LUCAS 2018\\task 4\\estimates_FINAL\\estimates2015\\"

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
# LUCAS 2018
#-------------------------------------
path_input <- paste0(path_previous_estimates, "estimates2018\\")
# path_input <- "C:\\Users\\UTENTE\\Google Drive\\LUCAS 2018\\task 4\\estimates_FINAL\\estimates2018\\"
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
# LUCAS 2022
#-------------------------------------
est <- NULL
for (k in EU22) {
  country <- read.csv(paste0(getwd(),"\\estimates2022\\",k,"_est_LC1_LU1_2022.csv"))
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
path_input <- paste0(path_previous_estimates, "estimates2012\\")
# path_input <- "C:\\Users\\UTENTE\\Google Drive\\LUCAS 2018\\task 4\\estimates_FINAL\\estimates2012\\"
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
# LUCAS 2015
#-------------------------------------
path_input <- paste0(path_previous_estimates, "estimates2015\\")
# path_input <- "C:\\Users\\UTENTE\\Google Drive\\LUCAS 2018\\task 4\\estimates_FINAL\\estimates2015\\"
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
# LUCAS 2018
#-------------------------------------
path_input <- paste0(path_previous_estimates, "estimates2018\\")
# path_input <- "C:\\Users\\UTENTE\\Google Drive\\LUCAS 2018\\task 4\\estimates_FINAL\\estimates2018\\"
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
# LUCAS 2022
#-------------------------------------
est <- NULL
for (k in EU26) {
  country <- read.csv(paste0(getwd(),"\\estimates2022\\",k,"_est_LC1_LU1_2022.csv"))
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
path_input <- paste0(path_previous_estimates, "estimates2015\\")
# path_input <- "C:\\Users\\UTENTE\\Google Drive\\LUCAS 2018\\task 4\\estimates_FINAL\\estimates2015\\"
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
# LUCAS 2018
#-------------------------------------
path_input <- paste0(path_previous_estimates, "estimates2018\\")
# path_input <- "C:\\Users\\UTENTE\\Google Drive\\LUCAS 2018\\task 4\\estimates_FINAL\\estimates2018\\"
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
# LUCAS 2022
#-------------------------------------
est <- NULL
for (k in EU27) {
  country <- read.csv(paste0(getwd(),"\\estimates2022\\",k,"_est_LC1_LU1_2022.csv"))
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
saveWorkbook(wb, paste0(path_output,"Europe_estimates.xlsx"), overwrite = TRUE)

