#---------------------------------
# Script to compare 2 and 3 digits
# LC and LU estimates
# standard vs two-phases
# by country: NUTS2
#---------------------------------
library(formattable)
library(openxlsx)
library(dplyr)
library(tidyr)

# read countries:
# load("C:/Users/UTENTE/Desktop/Progetto_LUCAS/Task1/2.TWO PHASES light//Script/countries.RData")
load("D:/Google Drive/LUCAS 2025/Task 1 - ESTIMATES/2.TWO PHASES light//Script/countries.RData")

#create folder where results must be saved: 
#setwd:
# setwd("C:/Users/UTENTE/Desktop/Progetto_LUCAS/Task1/2.TWO PHASES light/")
setwd("D:/Google Drive/LUCAS 2025/Task 1 - ESTIMATES/2.TWO PHASES light")
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
  # setwd("C:/Users/UTENTE/Desktop/Progetto_LUCAS/Task1/1.STANDARD/estimates2022")
  setwd("D:/Google Drive/LUCAS 2025/Task 1 - ESTIMATES/1.STANDARD/estimates2022")
  
  # leggo i dati delle stime europee nelle due procedure:
  c1 <- read.csv(paste0(countries[i],"_est_LC1_LU1_NUTS2_24_2022_t.csv"))
  # manipulation 
  #c1 = c1 %>% filter(variable !="variable")
  c1=c1 %>% 
    pivot_longer(cols=starts_with(countries[i]), names_to = "NUTS2", values_to = "value")  %>%
    pivot_wider(names_from = variable, values_from = value)
  #seleziono solo le variabili di interesse
  c1<-  c1 %>% select(NUTS2, contains("Total.SURVEY_LC1_2"), contains("Total.SURVEY_LC1_3"), contains("Total.SURVEY_LU1_2"), contains("Total.SURVEY_LU1_3"))
  c1 <- c1 %>% mutate(across(starts_with("Total.SURVEY_LC1_2") | starts_with("Total.SURVEY_LC1_3")| starts_with("Total.SURVEY_LU1_2")| starts_with("Total.SURVEY_LU1_3"), ~round(.x, 0))) %>% mutate(across(starts_with("CV"), ~round(.x, 3)))
  
  c1=c1 %>% 
    pivot_longer(cols=-NUTS2, names_to = "Variable", values_to = "value")
  c1 = c1 %>% mutate(tipo = case_when(
    startsWith(Variable, "Total")~ "Estimate", 
    startsWith(Variable,"SE.")~ "SE",
    startsWith(Variable,"CI.l")~ "CI.l(95%)",
    startsWith(Variable,"CI.u")~ "CI.u(95%)",
    startsWith(Variable,"CV")~ "CV"))
  
  c1$Variable=gsub("(.*)\\Total.", '', c1$Variable)
  
  c1= c1 %>% pivot_wider(names_from = "tipo", values_from = "value")
  c1$Variable=paste0("Total.", c1$Variable)
  
  #-------------------
  # twophase estimates
  #-------------------
  # setwd("C:/Users/UTENTE/Desktop/Progetto_LUCAS/Task1/2.TWO PHASES light/estimates2022")
  setwd("D:/Google Drive/LUCAS 2025/Task 1 - ESTIMATES/2.TWO PHASES light/estimates2022")
  # leggo i dati delle stime europee nelle due procedure:
  c2 <- read.csv(paste0(countries[i],"_est_LC1_LU1_NUTS2_24_2022_t.csv"))
  c2 = c2 %>% filter(variable !="variable")
  # manipulation 
  c2=c2 %>% 
    pivot_longer(cols=starts_with(countries[i]), names_to = "NUTS2", values_to = "value")  %>%
    pivot_wider(names_from = variable, values_from = value)
  #seleziono solo le variabili di interesse
  c2<-  c2 %>% select(NUTS2, contains("Total.SURVEY_LC1_2"), contains("Total.SURVEY_LC1_3"), contains("Total.SURVEY_LU1_2"), contains("Total.SURVEY_LU1_3"))
  c2 <- c2 %>% mutate(across(starts_with("Total.SURVEY_LC1_2") | starts_with("Total.SURVEY_LC1_3")| starts_with("Total.SURVEY_LU1_2")| starts_with("Total.SURVEY_LU1_3"), ~round(.x, 0))) %>% mutate(across(starts_with("CV"), ~round(.x, 3)))
  
  c2=c2 %>% 
    pivot_longer(cols=-NUTS2, names_to = "Variable", values_to = "value")
  c2 = c2 %>% mutate(tipo = case_when(
    startsWith(Variable, "Total")~ "Estimate", 
    startsWith(Variable,"SE.")~ "SE",
    startsWith(Variable,"CI.l")~ "CI.l(95%)",
    startsWith(Variable,"CI.u")~ "CI.u(95%)",
    startsWith(Variable,"CV")~ "CV"))
  
  c2$Variable=gsub(".*\\Total.", '', c2$Variable)
  
  c2= c2 %>% pivot_wider(names_from = "tipo", values_from = "value")
  c2$Variable=paste0("Total.", c2$Variable)
  
  
  #----------------------------
  # Compare Land Cover 2 digits
  #----------------------------
  # prendo solo LC2
  c1_2 <- c1 %>% filter(startsWith(Variable,"Total.SURVEY_LC1_2"))
  c2_2 <-c2 %>% filter(startsWith(Variable,"Total.SURVEY_LC1_2"))
  
  colnames(c1_2)[3:7] <- paste0("standard_",colnames(c1_2)[3:7])
  colnames(c2_2)[3:7] <- paste0("twophase_",colnames(c2_2)[3:7])
  
  est_2 <- merge(c1_2,c2_2,by=c("NUTS2", "Variable"),all.x=TRUE)
  est_2$area_diff <- round((est_2$twophase_Estimate - est_2$standard_Estimate),3)
  est_2$area_rel_diff <- round((est_2$twophase_Estimate - est_2$standard_Estimate) / est_2$standard_Estimate,3)
  est_2$cv_diff <- round((est_2$twophase_CV - est_2$standard_CV),3)
  est_2 <- est_2[,c("NUTS2","Variable","standard_Estimate","twophase_Estimate","area_diff", "area_rel_diff","standard_CV","twophase_CV","cv_diff")]
  
  est_2 <- est_2 %>% rename(standard_Area_2022=standard_Estimate, twophase_Area_2022=twophase_Estimate, standard_CV_2022=standard_CV, twophase_CV_2022 = twophase_CV) 
  
  
  
  
  
  # Create a formattable table with in-cell color scales and bars
  t <- formattable(
    est_2[,c("NUTS2", "Variable","standard_Area_2022","twophase_Area_2022","area_diff", "area_rel_diff","standard_CV_2022","twophase_CV_2022","cv_diff")],
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
  addStyle(wb, sheet = "LC 2 digits", bodyStyle, rows = 2:(nrow(t)+1), cols = c(1,2), gridExpand = TRUE)
  writeData(wb, sheet = "LC 2 digits", x = t, colNames = TRUE, rowNames = FALSE)
  
  #--------------------------
  # Compare Land Use 2 digits
  #--------------------------
  c1_2 <- c1 %>% filter(startsWith(Variable,"Total.SURVEY_LU1_2"))
  c2_2 <-c2 %>% filter(startsWith(Variable,"Total.SURVEY_LU1_2"))
  
  colnames(c1_2)[3:7] <- paste0("standard_",colnames(c1_2)[3:7])
  colnames(c2_2)[3:7] <- paste0("twophase_",colnames(c2_2)[3:7])
  
  est_2 <- merge(c1_2,c2_2,by=c("NUTS2", "Variable"),all.x=TRUE)
  est_2$area_diff <- round((est_2$twophase_Estimate - est_2$standard_Estimate),3)
  est_2$area_rel_diff <- round((est_2$twophase_Estimate - est_2$standard_Estimate) / est_2$standard_Estimate,3)
  est_2$cv_diff <- round((est_2$twophase_CV - est_2$standard_CV),3)
  est_2 <- est_2[,c("NUTS2","Variable","standard_Estimate","twophase_Estimate","area_diff", "area_rel_diff","standard_CV","twophase_CV","cv_diff")]
  
  est_2 <- est_2 %>% rename(standard_Area_2022=standard_Estimate, twophase_Area_2022=twophase_Estimate, standard_CV_2022=standard_CV, twophase_CV_2022 = twophase_CV) 
  
  
  # Create a formattable table with in-cell color scales and bars
  t <- formattable(
    est_2[,c("NUTS2","Variable","standard_Area_2022","twophase_Area_2022","area_diff", "area_rel_diff","standard_CV_2022","twophase_CV_2022","cv_diff")],
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
  addStyle(wb, sheet = "LU 2 digits", bodyStyle, rows = 2:(nrow(t)+1), cols = c(1,2), gridExpand = TRUE)
  writeData(wb, sheet = "LU 2 digits", x = t, colNames = TRUE, rowNames = FALSE)
  
  #----------------------------
  # Compare Land Cover 3 digits
  #----------------------------
  
  c1_2 <- c1 %>% filter(startsWith(Variable,"Total.SURVEY_LC1_3"))
  c2_2 <-c2 %>% filter(startsWith(Variable,"Total.SURVEY_LC1_3"))
  
  colnames(c1_2)[3:7] <- paste0("standard_",colnames(c1_2)[3:7])
  colnames(c2_2)[3:7] <- paste0("twophase_",colnames(c2_2)[3:7])
  
  est_2 <- merge(c1_2,c2_2,by=c("NUTS2", "Variable"),all.x=TRUE)
  est_2$area_diff <- round((est_2$twophase_Estimate - est_2$standard_Estimate),3)
  est_2$area_rel_diff <- round((est_2$twophase_Estimate - est_2$standard_Estimate) / est_2$standard_Estimate,3)
  est_2$cv_diff <- round((est_2$twophase_CV - est_2$standard_CV),3)
  est_2 <- est_2[,c("NUTS2","Variable","standard_Estimate","twophase_Estimate","area_diff", "area_rel_diff","standard_CV","twophase_CV","cv_diff")]
  
  est_2 <- est_2 %>% rename(standard_Area_2022=standard_Estimate, twophase_Area_2022=twophase_Estimate, standard_CV_2022=standard_CV, twophase_CV_2022 = twophase_CV) 
  
  
  
  
  # Create a formattable table with in-cell color scales and bars
  t <- formattable(
    est_2[,c("NUTS2","Variable","standard_Area_2022","twophase_Area_2022","area_diff", "area_rel_diff","standard_CV_2022","twophase_CV_2022","cv_diff")],
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
  addStyle(wb, sheet = "LC 3 digits", bodyStyle, rows = 2:(nrow(t)+1), cols = c(1,2), gridExpand = TRUE)
  writeData(wb, sheet = "LC 3 digits", x = t, colNames = TRUE, rowNames = FALSE)
  
  #--------------------------
  # Compare Land Use 3 digits
  #--------------------------
  c1_2 <- c1 %>% filter(startsWith(Variable,"Total.SURVEY_LU1_3"))
  c2_2 <-c2 %>% filter(startsWith(Variable,"Total.SURVEY_LU1_3"))
  
  colnames(c1_2)[3:7] <- paste0("standard_",colnames(c1_2)[3:7])
  colnames(c2_2)[3:7] <- paste0("twophase_",colnames(c2_2)[3:7])
  
  est_2 <- merge(c1_2,c2_2,by=c("NUTS2", "Variable"),all.x=TRUE)
  est_2$area_diff <- round((est_2$twophase_Estimate - est_2$standard_Estimate),3)
  est_2$area_rel_diff <- round((est_2$twophase_Estimate - est_2$standard_Estimate) / est_2$standard_Estimate,3)
  est_2$cv_diff <- round((est_2$twophase_CV - est_2$standard_CV),3)
  est_2 <- est_2[,c("NUTS2","Variable","standard_Estimate","twophase_Estimate","area_diff", "area_rel_diff","standard_CV","twophase_CV","cv_diff")]
  
  est_2 <- est_2 %>% rename(standard_Area_2022=standard_Estimate, twophase_Area_2022=twophase_Estimate, standard_CV_2022=standard_CV, twophase_CV_2022 = twophase_CV) 
  
  
  
  
  # Create a formattable table with in-cell color scales and bars
  t <- formattable(
    est_2[,c("NUTS2","Variable","standard_Area_2022","twophase_Area_2022","area_diff", "area_rel_diff","standard_CV_2022","twophase_CV_2022","cv_diff")],
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
  addStyle(wb, sheet = "LU 3 digits", bodyStyle, rows = 2:(nrow(t)+1), cols = c(1,2), gridExpand = TRUE)
  writeData(wb, sheet = "LU 3 digits", x = t, colNames = TRUE, rowNames = FALSE)
  # Write all
  #setwd("D:/Google Drive/LUCAS 2025/Task 1 - ESTIMATES/2.TWO PHASES light")
  setwd(direnew1 )
  saveWorkbook(wb, paste0(countries[i],"_NUTS2_Estimates_comparison.xlsx"), overwrite = TRUE)
  
}

#------------------------------------------------------------------



