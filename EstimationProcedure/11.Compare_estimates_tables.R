#---------------------------------
# Script to compare 2 and 3 digits
# LC and LU estimates
# standard vs two-phases
# EU level
#---------------------------------
#----------------------------------------------------------
# Input: ./2.EU_estimates_standard/Europe_estimates.xlsx
#        ./7.EU_estimates/Europe_estimates.xlsx
# Output: ./11.Comparisons
#----------------------------------------------------------

dire <- getwd()
direnew1 <- paste(dire, "/11.Comparisons/", sep = "")
if (dir.exists(direnew1))
  unlink(direnew1,recursive=TRUE)
if (!dir.exists(direnew1))
  dir.create(direnew1)

library(formattable)
library(openxlsx)
library(flextable)
library(officer)
if ("xlsx" %in% loadedNamespaces()){
  detach("package:xlsx", unload = TRUE)
}

doc <- read_docx()


wb <- createWorkbook()
headerStyle <- createStyle(textDecoration = "bold",halign="center", fontSize=14,fontColour = "#FFFFFF", fgFill = "#4F81BD")
bodyStyle <- createStyle(textDecoration = "bold", fontSize=12,fontColour = "#FFFFFF", fgFill = "#4F81BD")
#-------------------
# Standard estimates
#-------------------
eu1 <- read.xlsx("./2.EU_estimates_standard/Europe_estimates.xlsx",sheet = 3)
#seleziono solo le stime delle aree 2022
vars <- colnames(eu1)[grep("2022",colnames(eu1))]
# salvo le variabili LC, LU a 2/3 digits
variables_LC2 <- eu1$Variable[grep("LC1_2",eu1$Variable)]
variables_LC3 <- eu1$Variable[grep("LC1_3",eu1$Variable)]
variables_LU2 <- eu1$Variable[grep("LU1_2",eu1$Variable)]
variables_LU3 <- eu1$Variable[grep("LU1_3",eu1$Variable)]

eu1 <- eu1[,c("Variable",vars)]
eu1$Area2022 <- round(eu1$Area2022)
eu1$CV_2022 <- round(eu1$CV_2022,3)

#-------------------
# twophase estimates
#-------------------
eu2 <- read.xlsx("./7.EU_estimates/Europe_estimates.xlsx",sheet = 3)
# eu2[eu2$Variable=="LC1_2A1","Area2022"]
eu2 <- eu2[,c("Variable",vars)]
eu2$Area2022 <- round(eu2$Area2022)
eu2$CV_2022 <- round(eu2$CV_2022,3)

#----------------------------
# Compare Land Cover 2 digits
#----------------------------
# prendo solo LC2
eu1_2 <- eu1[eu1$Variable %in% variables_LC2,c("Variable",vars)]
eu2_2 <- eu2[eu2$Variable %in% variables_LC2,c("Variable",vars)]

colnames(eu1_2)[c(2:6)] <- paste0("standard_",colnames(eu1_2)[c(2:6)])
colnames(eu2_2)[c(2:6)] <- paste0("twophase_",colnames(eu2_2)[c(2:6)])

est_2 <- merge(eu1_2,eu2_2,by="Variable",all.x=TRUE)
est_2$area_diff <- round((est_2$twophase_Area2022 - est_2$standard_Area2022),3)
est_2$area_rel_diff <- round((est_2$twophase_Area2022 - est_2$standard_Area2022) / est_2$standard_Area2022,3)
est_2$cv_diff <- round((est_2$twophase_CV_2022 - est_2$standard_CV_2022),3)
est_2 <- est_2[,c("Variable","standard_Area2022","twophase_Area2022","area_diff", "area_rel_diff","standard_CV_2022","twophase_CV_2022","cv_diff")]
# est_2[,c("standard_Area2022","twophase_Area2022")] <- round(est_2[,c("standard_Area2022","twophase_Area2022")] )
# est_2[,c("standard_CV_2022","twophase_CV_2022")] <- round(est_2[,c("standard_CV_2022","twophase_CV_2022")],3 )
# est_2[est_2$Variable=="LC1_2A1",c("standard_Area2022","twophase_Area2022","standard_CV_2022","twophase_CV_2022")]



# Create a formattable table with in-cell color scales and bars
t <- formattable(
  est_2[,c("Variable","standard_Area2022","twophase_Area2022","area_diff", "area_rel_diff","standard_CV_2022","twophase_CV_2022","cv_diff")],
  list(
    standard_Area2022 = color_bar("lightgreen"),
    twophase_Area2022 = color_bar("orange"),
    area_diff = color_tile("lightblue","pink"),
    area_rel_diff = color_tile("lightblue","pink"),
    cv_diff = color_tile("lightblue","pink")
    # ,
    # rel_diff = formatter("span",
    #            style = x ~ style(font.weight = "bold", color = ifelse(abs(x) > 0.4, "red", "black")))
  )
)
t

ft <- flextable(est_2[, c("Variable","standard_Area2022","twophase_Area2022",
                          "standard_CV_2022","twophase_CV_2022","cv_diff")])
ft <- set_caption(ft, caption = "CVs comparison for two-digit Land Cover estimates")
ft <- fontsize(ft, size = 8, part = "all")
ft <- set_table_properties(ft, layout = "fixed", width = 0.5)
ft <- autofit(ft)
ft
doc <- body_add_flextable(doc, value = ft)
print(doc, target = "compare_estimates.docx")

addWorksheet(wb, sheetName = "LC 2 digits")
addStyle(wb, sheet = "LC 2 digits", style = headerStyle, rows = 1, cols = 1:ncol(t), gridExpand = TRUE)
addStyle(wb, sheet = "LC 2 digits", bodyStyle, rows = 2:(nrow(t)+1), cols = 1, gridExpand = TRUE)
writeData(wb, sheet = "LC 2 digits", x = t, colNames = TRUE, rowNames = FALSE)

#--------------------------
# Compare Land Use 2 digits
#--------------------------

eu1_2 <- eu1[eu1$Variable %in% variables_LU2,c("Variable",vars)]
eu2_2 <- eu2[eu2$Variable %in% variables_LU2,c("Variable",vars)]

colnames(eu1_2)[c(2:6)] <- paste0("standard_",colnames(eu1_2)[c(2:6)])
colnames(eu2_2)[c(2:6)] <- paste0("twophase_",colnames(eu2_2)[c(2:6)])

est_2 <- merge(eu1_2,eu2_2,by="Variable")
est_2$area_diff <- round((est_2$twophase_Area2022 - est_2$standard_Area2022),3)
est_2$area_rel_diff <- round((est_2$twophase_Area2022 - est_2$standard_Area2022) / est_2$standard_Area2022,3)
est_2$cv_diff <- round((est_2$twophase_CV_2022 - est_2$standard_CV_2022) ,3)
est_2 <- est_2[,c("Variable","standard_Area2022","twophase_Area2022","area_diff", "area_rel_diff","standard_CV_2022","twophase_CV_2022","cv_diff")]
# est_2[,c("standard_Area2022","twophase_Area2022")] <- round(est_2[,c("standard_Area2022","twophase_Area2022")] )
# est_2[,c("standard_CV_2022","twophase_CV_2022")] <- round(est_2[,c("standard_CV_2022","twophase_CV_2022")],3 )
# est_2[est_2$Variable=="LC1_2A1",c("standard_Area2022","twophase_Area2022","standard_CV_2022","twophase_CV_2022")]


# Create a formattable table with in-cell color scales and bars
t <- formattable(
  est_2[,c("Variable","standard_Area2022","twophase_Area2022","area_diff", "area_rel_diff","standard_CV_2022","twophase_CV_2022","cv_diff")],
  list(
    standard_Area2022 = color_bar("lightgreen"),
    twophase_Area2022 = color_bar("orange"),
    area_diff = color_tile("lightblue","pink"),
    area_rel_diff = color_tile("lightblue","pink"),
    cv_diff = color_tile("lightblue","pink")
    # ,
    # rel_diff = formatter("span",
    #            style = x ~ style(font.weight = "bold", color = ifelse(abs(x) > 0.4, "red", "black")))
  )
)
t
ft <- flextable(est_2[, c("Variable","standard_Area2022","twophase_Area2022",
                          "standard_CV_2022","twophase_CV_2022","cv_diff")])
ft <- set_caption(ft, caption = "CVs comparison for two-digit Land Use estimates")
ft <- fontsize(ft, size = 8, part = "all")
ft <- set_table_properties(ft, layout = "fixed", width = 0.5)
ft <- autofit(ft)
ft
doc <- body_add_flextable(doc, value = ft)
print(doc, target = "compare_estimates.docx")

addWorksheet(wb, sheetName = "LU 2 digits")
addStyle(wb, sheet = "LU 2 digits", style = headerStyle, rows = 1, cols = 1:ncol(t), gridExpand = TRUE)
addStyle(wb, sheet = "LU 2 digits", bodyStyle, rows = 2:(nrow(t)+1), cols = 1, gridExpand = TRUE)
writeData(wb, sheet = "LU 2 digits", x = t, colNames = TRUE, rowNames = FALSE)

#----------------------------
# Compare Land Cover 3 digits
#----------------------------

eu1_2 <- eu1[eu1$Variable %in% variables_LC3,c("Variable",vars)]
eu2_2 <- eu2[eu2$Variable %in% variables_LC3,c("Variable",vars)]

colnames(eu1_2)[c(2:6)] <- paste0("standard_",colnames(eu1_2)[c(2:6)])
colnames(eu2_2)[c(2:6)] <- paste0("twophase_",colnames(eu2_2)[c(2:6)])

est_2 <- merge(eu1_2,eu2_2,by="Variable")
est_2$area_diff <- round((est_2$twophase_Area2022 - est_2$standard_Area2022),3)
est_2$area_rel_diff <- round((est_2$twophase_Area2022 - est_2$standard_Area2022) / est_2$standard_Area2022,3)
est_2$cv_diff <- round((est_2$twophase_CV_2022 - est_2$standard_CV_2022),3)
est_2 <- est_2[,c("Variable","standard_Area2022","twophase_Area2022","area_diff", "area_rel_diff","standard_CV_2022","twophase_CV_2022","cv_diff")]
# est_2[,c("standard_Area2022","twophase_Area2022")] <- round(est_2[,c("standard_Area2022","twophase_Area2022")] )
# est_2[,c("standard_CV_2022","twophase_CV_2022")] <- round(est_2[,c("standard_CV_2022","twophase_CV_2022")],3 )
# est_2[est_2$Variable=="LC1_2A1",c("standard_Area2022","twophase_Area2022","standard_CV_2022","twophase_CV_2022")]



# Create a formattable table with in-cell color scales and bars
t <- formattable(
  est_2[,c("Variable","standard_Area2022","twophase_Area2022","area_diff", "area_rel_diff","standard_CV_2022","twophase_CV_2022","cv_diff")],
  list(
    standard_Area2022 = color_bar("lightgreen"),
    twophase_Area2022 = color_bar("orange"),
    area_diff = color_tile("lightblue","pink"),
    area_rel_diff = color_tile("lightblue","pink"),
    cv_diff = color_tile("lightblue","pink")
    # ,
    # rel_diff = formatter("span",
    #            style = x ~ style(font.weight = "bold", color = ifelse(abs(x) > 0.4, "red", "black")))
  )
)
t
ft <- flextable(est_2[, c("Variable","standard_Area2022","twophase_Area2022",
                          "standard_CV_2022","twophase_CV_2022","cv_diff")])
ft <- set_caption(ft, caption = "CVs comparison for three-digit Land Cover estimates")
ft <- fontsize(ft, size = 8, part = "all")
ft <- set_table_properties(ft, layout = "fixed", width = 0.5)
ft <- autofit(ft)
ft
doc <- body_add_flextable(doc, value = ft)
print(doc, target = "compare_estimates.docx")


addWorksheet(wb, sheetName = "LC 3 digits")
addStyle(wb, sheet = "LC 3 digits", style = headerStyle, rows = 1, cols = 1:ncol(t), gridExpand = TRUE)
addStyle(wb, sheet = "LC 3 digits", bodyStyle, rows = 2:(nrow(t)+1), cols = 1, gridExpand = TRUE)
writeData(wb, sheet = "LC 3 digits", x = t, colNames = TRUE, rowNames = FALSE)

#--------------------------
# Compare Land Use 3 digits
#--------------------------

eu1_2 <- eu1[eu1$Variable %in% variables_LU3,c("Variable",vars)]
eu2_2 <- eu2[eu2$Variable %in% variables_LU3,c("Variable",vars)]

colnames(eu1_2)[c(2:6)] <- paste0("standard_",colnames(eu1_2)[c(2:6)])
colnames(eu2_2)[c(2:6)] <- paste0("twophase_",colnames(eu2_2)[c(2:6)])

est_2 <- merge(eu1_2,eu2_2,by="Variable")
est_2$area_diff <- round((est_2$twophase_Area2022 - est_2$standard_Area2022),3)
est_2$area_rel_diff <- round((est_2$twophase_Area2022 - est_2$standard_Area2022) / est_2$standard_Area2022,3)
est_2$cv_diff <- round((est_2$twophase_CV_2022 - est_2$standard_CV_2022) / est_2$standard_CV_2022,3)
est_2 <- est_2[,c("Variable","standard_Area2022","twophase_Area2022","area_diff", "area_rel_diff","standard_CV_2022","twophase_CV_2022","cv_diff")]
# est_2[,c("standard_Area2022","twophase_Area2022")] <- round(est_2[,c("standard_Area2022","twophase_Area2022")] )
# est_2[,c("standard_CV_2022","twophase_CV_2022")] <- round(est_2[,c("standard_CV_2022","twophase_CV_2022")],3 )
# est_2[est_2$Variable=="LC1_2A1",c("standard_Area2022","twophase_Area2022","standard_CV_2022","twophase_CV_2022")]



# Create a formattable table with in-cell color scales and bars
t <- formattable(
  est_2[,c("Variable","standard_Area2022","twophase_Area2022","area_diff","area_rel_diff","standard_CV_2022","twophase_CV_2022","cv_diff")],
  list(
    standard_Area2022 = color_bar("lightgreen"),
    twophase_Area2022 = color_bar("orange"),
    area_diff = color_tile("lightblue","pink"),
    area_rel_diff = color_tile("lightblue","pink"),
    cv_diff = color_tile("lightblue","pink")
    # ,
    # rel_diff = formatter("span",
    #            style = x ~ style(font.weight = "bold", color = ifelse(abs(x) > 0.4, "red", "black")))
  )
)
t
ft <- flextable(est_2[, c("Variable","standard_Area2022","twophase_Area2022",
                          "standard_CV_2022","twophase_CV_2022","cv_diff")])
ft <- set_caption(ft, caption = "CVs comparison for three-digit Land Use estimates")
ft <- fontsize(ft, size = 8, part = "all")
ft <- set_table_properties(ft, layout = "fixed", width = 0.5)
ft <- autofit(ft)
ft
doc <- body_add_flextable(doc, value = ft)
print(doc, target = "compare_estimates.docx")

addWorksheet(wb, sheetName = "LU 3 digits")
addStyle(wb, sheet = "LU 3 digits", style = headerStyle, rows = 1, cols = 1:ncol(t), gridExpand = TRUE)
addStyle(wb, sheet = "LU 3 digits", bodyStyle, rows = 2:(nrow(t)+1), cols = 1, gridExpand = TRUE)
writeData(wb, sheet = "LU 3 digits", x = t, colNames = TRUE, rowNames = FALSE)


#------------------------------------------------------------------
# Write all
saveWorkbook(wb,file=paste0(direnew1,"A_Estimates_comparison.xlsx"), overwrite = TRUE)



