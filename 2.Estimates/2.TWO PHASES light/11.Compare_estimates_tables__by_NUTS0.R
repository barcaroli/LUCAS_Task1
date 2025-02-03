#---------------------------------
# Script to compare 2 and 3 digits
# LC and LU estimates
# standard vs two-phases
# by country: NUTS0
#---------------------------------
library(formattable)
library(openxlsx)

# read countries:
# load("C:/Users/UTENTE/Desktop/Progetto_LUCAS/Task1/2.TWO PHASES light//Script/countries.RData")
load("D:/Google Drive/LUCAS 2025/Task 1 - ESTIMATES/2.TWO PHASES light//Script/countries.RData")

#create folder where results must be saved: 
#setwd:
# setwd("C:/Users/UTENTE/Desktop/Progetto_LUCAS/Task1/2.TWO PHASES light/")
setwd("D:/Google Drive/LUCAS 2025/Task 1 - ESTIMATES/2.TWO PHASES light/")
dire <- getwd()
direnew1 <- paste(dire, "\\Estimates_comparison\\", sep = "")
# if (dir.exists(direnew1)) 
#   unlink(direnew1,recursive=TRUE)
if (!dir.exists(direnew1)) 
  dir.create(direnew1)
# for each country save tables of differences of LC2, LC3, LU2, LU3
for(i in 1:length(countries)){
  print(countries[i])
  # create workbook
  wb <- createWorkbook()
  headerStyle <- createStyle(textDecoration = "bold",halign="center", fontSize=14,fontColour = "#FFFFFF", fgFill = "#4F81BD")
  bodyStyle <- createStyle(textDecoration = "bold", fontSize=12,fontColour = "#FFFFFF", fgFill = "#4F81BD")
  #load estimates: 
  
  #-------------------
  # Standard estimates
  #-------------------
  # setwd("C:/Users/UTENTE/Desktop/Progetto_LUCAS/Task1/1.STANDARD/allyears_estimates")
  setwd("D:/Google Drive/LUCAS 2025/Task 1 - ESTIMATES/1.STANDARD/allyears_estimates")
  
# leggo i dati delle stime europee nelle due procedure:
c1 <- read.csv(paste0(countries[i],"_est_all.csv"))
#seleziono solo le stime delle aree 2022
idx<- which(colnames(c1)==colnames(c1)[grep("2022",colnames(c1))])
colnames(c1)=gsub("\\.\\d+", '', colnames(c1))
#colnames(c1)=gsub('.4','',colnames(c1))
colnames(c1)[(idx+1):(idx+4)]=paste0(colnames(c1)[(idx+1):(idx+4)], "_2022")
vars <- colnames(c1)[grep("2022",colnames(c1))]
# salvo le variabili LC, LU a 2/3 digits
variables_LC2 <- c1$Variable[grep("LC1_2",c1$Variable)]
variables_LC3 <- c1$Variable[grep("LC1_3",c1$Variable)]
variables_LU2 <- c1$Variable[grep("LU1_2",c1$Variable)]
variables_LU3 <- c1$Variable[grep("LU1_3",c1$Variable)]

c1 <- c1[,c("Variable",vars)]
c1$Area_2022 <- round(c1$Area_2022)
c1$CV_2022 <- round(c1$CV_2022,3)

#-------------------
# twophasehase estimates
#-------------------
# setwd("C:/Users/UTENTE/Desktop/Progetto_LUCAS/Task1/2.TWO PHASES light/allyears_estimates")
setwd("D:/Google Drive/LUCAS 2025/Task 1 - ESTIMATES/2.TWO PHASES light/allyears_estimates")

# setwd("C:\\Users\\UTENTE\\Google Drive/LUCAS 2025/Task 1 - ESTIMATES/2.TWO PHASES light/EU_estimates")

c2 <- read.csv(paste0(countries[i],"_est_all.csv"))
#seleziono solo le stime delle aree 2022
idx<- which(colnames(c2)==colnames(c2)[grep("2022",colnames(c2))])
colnames(c2)=gsub("\\.\\d+", '', colnames(c2))
#colnames(c2)=gsub('.4','',colnames(c2))
colnames(c2)[(idx+1):(idx+4)]=paste0(colnames(c2)[(idx+1):(idx+4)], "_2022")

# c2[c2$Variable=="LC1_2A1","Area2022"]
c2 <- c2[,c("Variable",vars)]
c2$Area_2022 <- round(c2$Area_2022)
c2$CV_2022 <- round(c2$CV_2022,3)

#----------------------------
# Compare Land Cover 2 digits
#----------------------------
# prendo solo LC2
c1_2 <- c1[c1$Variable %in% variables_LC2,c("Variable",vars)]
c2_2 <- c2[c2$Variable %in% variables_LC2,c("Variable",vars)]

colnames(c1_2)[c(2:6)] <- paste0("standard_",colnames(c1_2)[c(2:6)])
colnames(c2_2)[c(2:6)] <- paste0("twophase_",colnames(c2_2)[c(2:6)])

est_2 <- merge(c1_2,c2_2,by="Variable",all.x=TRUE)
est_2$area_diff <- round((est_2$twophase_Area_2022 - est_2$standard_Area_2022),3)
est_2$area_rel_diff <- round((est_2$twophase_Area_2022 - est_2$standard_Area_2022) / est_2$standard_Area_2022,3)
est_2$cv_diff <- round((est_2$twophase_CV_2022 - est_2$standard_CV_2022),3)
est_2 <- est_2[,c("Variable","standard_Area_2022","twophase_Area_2022","area_diff", "area_rel_diff","standard_CV_2022","twophase_CV_2022","cv_diff")]
# est_2[,c("standard_Area2022","twophase_Area2022")] <- round(est_2[,c("standard_Area2022","twophase_Area2022")] )
# est_2[,c("standard_CV_2022","twophase_CV_2022")] <- round(est_2[,c("standard_CV_2022","twophase_CV_2022")],3 )
# est_2[est_2$Variable=="LC1_2A1",c("standard_Area2022","twophase_Area2022","standard_CV_2022","twophase_CV_2022")]



# Create a formattable table with in-cell color scales and bars
t <- formattable(
  est_2[,c("Variable","standard_Area_2022","twophase_Area_2022","area_diff", "area_rel_diff","standard_CV_2022","twophase_CV_2022","cv_diff")],
  list(
    standard_Area_2022 = color_bar("lightgreen"),
    twophase_Area_2022 = color_bar("orange"),
    area_diff = color_tile("lightblue","pink"),
    area_rel_diff = color_tile("lightblue","pink"),
    cv_diff = color_tile("lightblue","pink")
    # ,
    # rel_diff = formatter("span",
    #            style = x ~ style(font.weight = "bold", color = ifelse(abs(x) > 0.4, "red", "black")))
  )
)
t
addWorksheet(wb, sheetName = "LC 2 digits")
addStyle(wb, sheet = "LC 2 digits", style = headerStyle, rows = 1, cols = 1:ncol(t), gridExpand = TRUE)
addStyle(wb, sheet = "LC 2 digits", bodyStyle, rows = 2:(nrow(t)+1), cols = 1, gridExpand = TRUE)
writeData(wb, sheet = "LC 2 digits", x = t, colNames = TRUE, rowNames = FALSE)

#--------------------------
# Compare Land Use 2 digits
#--------------------------

c1_2 <- c1[c1$Variable %in% variables_LU2,c("Variable",vars)]
c2_2 <- c2[c2$Variable %in% variables_LU2,c("Variable",vars)]

colnames(c1_2)[c(2:6)] <- paste0("standard_",colnames(c1_2)[c(2:6)])
colnames(c2_2)[c(2:6)] <- paste0("twophase_",colnames(c2_2)[c(2:6)])

est_2 <- merge(c1_2,c2_2,by="Variable")
est_2$area_diff <- round((est_2$twophase_Area_2022 - est_2$standard_Area_2022),3)
est_2$area_rel_diff <- round((est_2$twophase_Area_2022 - est_2$standard_Area_2022) / est_2$standard_Area_2022,3)
est_2$cv_diff <- round((est_2$twophase_CV_2022 - est_2$standard_CV_2022) ,3)
est_2 <- est_2[,c("Variable","standard_Area_2022","twophase_Area_2022","area_diff", "area_rel_diff","standard_CV_2022","twophase_CV_2022","cv_diff")]
# est_2[,c("standard_Area_2022","twophase_Area_2022")] <- round(est_2[,c("standard_Area_2022","twophase_Area_2022")] )
# est_2[,c("standard_CV_2022","twophase_CV_2022")] <- round(est_2[,c("standard_CV_2022","twophase_CV_2022")],3 )
# est_2[est_2$Variable=="LC1_2A1",c("standard_Area_2022","twophase_Area_2022","standard_CV_2022","twophase_CV_2022")]


# Create a formattable table with in-cell color scales and bars
t <- formattable(
  est_2[,c("Variable","standard_Area_2022","twophase_Area_2022","area_diff", "area_rel_diff","standard_CV_2022","twophase_CV_2022","cv_diff")],
  list(
    standard_Area_2022 = color_bar("lightgreen"),
    twophase_Area_2022 = color_bar("orange"),
    area_diff = color_tile("lightblue","pink"),
    area_rel_diff = color_tile("lightblue","pink"),
    cv_diff = color_tile("lightblue","pink")
    # ,
    # rel_diff = formatter("span",
    #            style = x ~ style(font.weight = "bold", color = ifelse(abs(x) > 0.4, "red", "black")))
  )
)
t
addWorksheet(wb, sheetName = "LU 2 digits")
addStyle(wb, sheet = "LU 2 digits", style = headerStyle, rows = 1, cols = 1:ncol(t), gridExpand = TRUE)
addStyle(wb, sheet = "LU 2 digits", bodyStyle, rows = 2:(nrow(t)+1), cols = 1, gridExpand = TRUE)
writeData(wb, sheet = "LU 2 digits", x = t, colNames = TRUE, rowNames = FALSE)

#----------------------------
# Compare Land Cover 3 digits
#----------------------------

c1_2 <- c1[c1$Variable %in% variables_LC3,c("Variable",vars)]
c2_2 <- c2[c2$Variable %in% variables_LC3,c("Variable",vars)]

colnames(c1_2)[c(2:6)] <- paste0("standard_",colnames(c1_2)[c(2:6)])
colnames(c2_2)[c(2:6)] <- paste0("twophase_",colnames(c2_2)[c(2:6)])

est_2 <- merge(c1_2,c2_2,by="Variable")
est_2$area_diff <-round((est_2$twophase_Area_2022 - est_2$standard_Area_2022),3)
est_2$area_rel_diff <- round((est_2$twophase_Area_2022 - est_2$standard_Area_2022) / est_2$standard_Area_2022,3)
est_2$cv_diff <- round((est_2$twophase_CV_2022 - est_2$standard_CV_2022),3)
est_2 <- est_2[,c("Variable","standard_Area_2022","twophase_Area_2022","area_diff", "area_rel_diff","standard_CV_2022","twophase_CV_2022","cv_diff")]
# est_2[,c("standard_Area_2022","twophase_Area_2022")] <- round(est_2[,c("standard_Area_2022","twophase_Area_2022")] )
# est_2[,c("standard_CV_2022","twophase_CV_2022")] <- round(est_2[,c("standard_CV_2022","twophase_CV_2022")],3 )
# est_2[est_2$Variable=="LC1_2A1",c("standard_Area_2022","twophase_Area_2022","standard_CV_2022","twophase_CV_2022")]



# Create a formattable table with in-cell color scales and bars
t <- formattable(
  est_2[,c("Variable","standard_Area_2022","twophase_Area_2022","area_diff", "area_rel_diff","standard_CV_2022","twophase_CV_2022","cv_diff")],
  list(
    standard_Area_2022 = color_bar("lightgreen"),
    twophase_Area_2022 = color_bar("orange"),
    area_diff = color_tile("lightblue","pink"),
    area_rel_diff = color_tile("lightblue","pink"),
    cv_diff = color_tile("lightblue","pink")
    # ,
    # rel_diff = formatter("span",
    #            style = x ~ style(font.weight = "bold", color = ifelse(abs(x) > 0.4, "red", "black")))
  )
)
t
addWorksheet(wb, sheetName = "LC 3 digits")
addStyle(wb, sheet = "LC 3 digits", style = headerStyle, rows = 1, cols = 1:ncol(t), gridExpand = TRUE)
addStyle(wb, sheet = "LC 3 digits", bodyStyle, rows = 2:(nrow(t)+1), cols = 1, gridExpand = TRUE)
writeData(wb, sheet = "LC 3 digits", x = t, colNames = TRUE, rowNames = FALSE)

#--------------------------
# Compare Land Use 3 digits
#--------------------------

c1_2 <- c1[c1$Variable %in% variables_LU3,c("Variable",vars)]
c2_2 <- c2[c2$Variable %in% variables_LU3,c("Variable",vars)]

colnames(c1_2)[c(2:6)] <- paste0("standard_",colnames(c1_2)[c(2:6)])
colnames(c2_2)[c(2:6)] <- paste0("twophase_",colnames(c2_2)[c(2:6)])

est_2 <- merge(c1_2,c2_2,by="Variable")
est_2$area_diff <- round((est_2$twophase_Area_2022 - est_2$standard_Area_2022),3)
est_2$area_rel_diff <- round((est_2$twophase_Area_2022 - est_2$standard_Area_2022) / est_2$standard_Area_2022,3)
est_2$cv_diff <- round((est_2$twophase_CV_2022 - est_2$standard_CV_2022) / est_2$standard_CV_2022,3)
est_2 <- est_2[,c("Variable","standard_Area_2022","twophase_Area_2022","area_diff", "area_rel_diff","standard_CV_2022","twophase_CV_2022","cv_diff")]
# est_2[,c("standard_Area_2022","twophase_Area_2022")] <- round(est_2[,c("standard_Area_2022","twophase_Area_2022")] )
# est_2[,c("standard_CV_2022","twophase_CV_2022")] <- round(est_2[,c("standard_CV_2022","twophase_CV_2022")],3 )
# est_2[est_2$Variable=="LC1_2A1",c("standard_Area_2022","twophase_Area_2022","standard_CV_2022","twophase_CV_2022")]



# Create a formattable table with in-cell color scales and bars
t <- formattable(
  est_2[,c("Variable","standard_Area_2022","twophase_Area_2022","area_diff", "area_rel_diff","standard_CV_2022","twophase_CV_2022","cv_diff")],
  list(
    standard_Area_2022 = color_bar("lightgreen"),
    twophase_Area_2022 = color_bar("orange"),
    area_diff = color_tile("lightblue","pink"),
    area_rel_diff = color_tile("lightblue","pink"),
    cv_diff = color_tile("lightblue","pink")
    # ,
    # rel_diff = formatter("span",
    #            style = x ~ style(font.weight = "bold", color = ifelse(abs(x) > 0.4, "red", "black")))
  )
)
t
addWorksheet(wb, sheetName = "LU 3 digits")
addStyle(wb, sheet = "LU 3 digits", style = headerStyle, rows = 1, cols = 1:ncol(t), gridExpand = TRUE)
addStyle(wb, sheet = "LU 3 digits", bodyStyle, rows = 2:(nrow(t)+1), cols = 1, gridExpand = TRUE)
writeData(wb, sheet = "LU 3 digits", x = t, colNames = TRUE, rowNames = FALSE)
# Write all
#setwd("D:/Google Drive/LUCAS 2025/Task 1 - ESTIMATES/2.TWO PHASES light")
# setwd("C:/Users/UTENTE/Desktop/Progetto_LUCAS/Task1/2.TWO PHASES light/Estimates_comparison/")
setwd("D:/Google Drive/LUCAS 2025/Task 1 - ESTIMATES/2.TWO PHASES light/Estimates_comparison/")
saveWorkbook(wb, paste0(countries[i],"_Estimates_comparison.xlsx"), overwrite = TRUE)

}

#------------------------------------------------------------------



