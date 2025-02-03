##############################################################
# LUCAS - Check of estimates variations for all countries
##############################################################
options(stringsAsFactors = TRUE)
library(openxlsx)
library(data.table)
setwd("D:/Google Drive/LUCAS 2025/Task 1 - ESTIMATES/1.STANDARD/")
filename <- "D:/Google Drive/LUCAS 2025/Task 1 - ESTIMATES/1.STANDARD/tables_country/tables_all.xlsx"
# setwd("C:\\Users\\UTENTE\\Google Drive/LUCAS 2025/Task 1 - ESTIMATES/1.STANDARD/")
# filename <- "C:\\Users\\UTENTE\\Google Drive/LUCAS 2025/Task 1 - ESTIMATES/1.STANDARD/tables_country/tables_all.xlsx"
dire <- getwd()
direnew1 <- paste0(dire, "\\tables_country\\")
# if (dir.exists(direnew1)) 
#   unlink(direnew1,recursive=TRUE)
if (!dir.exists(direnew1)) 
  dir.create(direnew1)
load("countries.RData")
wb <- createWorkbook()
# Apply styling to the header of the Diamonds Data sheet
headerStyle <- createStyle(textDecoration = "bold",halign="center", fontSize=14,fontColour = "#FFFFFF", fgFill = "#4F81BD")
bodyStyle <- createStyle(textDecoration = "bold", fontSize=12,fontColour = "#FFFFFF", fgFill = "#4F81BD")

for (i in (1:length(countries))) {
  setwd("D:/Google Drive/LUCAS 2025/Task 1 - ESTIMATES/1.STANDARD/allyears_estimates")
  # setwd("C:\\Users\\UTENTE\\Google Drive/LUCAS 2025/Task 1 - ESTIMATES/1.STANDARD/allyears_estimates")
  cat("\n Country: ",countries[i],"\n")
  eval(parse(text=paste("df <- fread('",countries[i],"_est_all.csv',dec='.')",sep="")))
  addWorksheet(wb, sheetName = countries[i])
  addStyle(wb, sheet = countries[i], style = headerStyle, rows = 1, cols = 1:ncol(df), gridExpand = TRUE)
  addStyle(wb, sheet = countries[i], bodyStyle, rows = 2:(nrow(df)+1), cols = 1, gridExpand = TRUE)
  writeData(wb, sheet = countries[i], x = df, colNames = TRUE, rowNames = FALSE)
}
saveWorkbook(wb, filename, overwrite = TRUE)
