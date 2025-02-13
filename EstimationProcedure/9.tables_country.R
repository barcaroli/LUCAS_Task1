##############################################################
# LUCAS - Check of estimates variations for all countries
##############################################################
#----------------------------------------------------------
# Input:  ./6.allyears_estimates
#         countries.RData
# Output: ./9.Tables_country/all_countries_estimates.xlsx
#----------------------------------------------------------
options(stringsAsFactors = TRUE)
if ("xlsx" %in% loadedNamespaces()){
  detach("package:xlsx", unload = TRUE)
}
library(openxlsx)
library(data.table)

dire <- getwd()
direnew1 <- paste(dire, "./9.Tables_country/", sep = "")
if (dir.exists(direnew1))
  unlink(direnew1,recursive=TRUE)
if (!dir.exists(direnew1))
  dir.create(direnew1)


filename <- "./9.Tables_country/all_countries_estimates.xlsx"

load("countries.RData")
wb <- createWorkbook()
# Apply styling to the header of the Diamonds Data sheet
headerStyle <- createStyle(textDecoration = "bold",halign="center", fontSize=14,fontColour = "#FFFFFF", fgFill = "#4F81BD")
bodyStyle <- createStyle(textDecoration = "bold", fontSize=12,fontColour = "#FFFFFF", fgFill = "#4F81BD")

for (i in (1:length(countries))) {
  cat("\n Country: ",countries[i],"\n")
  eval(parse(text=paste("df <- fread('./6.allyears_estimates/",countries[i],"_est_all.csv',dec='.')",sep="")))
  addWorksheet(wb, sheetName = countries[i])
  addStyle(wb, sheet = countries[i], style = headerStyle, rows = 1, cols = 1:ncol(df), gridExpand = TRUE)
  addStyle(wb, sheet = countries[i], bodyStyle, rows = 2:(nrow(df)+1), cols = 1, gridExpand = TRUE)
  writeData(wb, sheet = countries[i], x = df, colNames = TRUE, rowNames = FALSE)
}
saveWorkbook(wb, filename, overwrite = TRUE)
