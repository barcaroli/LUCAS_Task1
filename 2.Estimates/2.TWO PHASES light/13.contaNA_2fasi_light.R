### leggi file confronto e conta NA delle stime standard###
library(formattable)
library(openxlsx)
#library(xlsx)
#set wd
# setwd("C:/Users/UTENTE/Desktop/Progetto_LUCAS/Task1/2.TWO PHASES light")
setwd("D:/Google Drive/LUCAS 2025/Task 1 - ESTIMATES/2.TWO PHASES light")
# read countries:
# load("C:/Users/UTENTE/Desktop/Progetto_LUCAS/Task1/2.TWO PHASES light/Script/countries.RData")
load("D:/Google Drive/LUCAS 2025/Task 1 - ESTIMATES/2.TWO PHASES light/Script/countries.RData")

# dire_comparison="C:/Users/UTENTE/Desktop/Progetto_LUCAS/Task1/2.TWO PHASES light/Estimates_comparison/"
dire_comparison="D:/Google Drive/LUCAS 2025/Task 1 - ESTIMATES/2.TWO PHASES light/Estimates_comparison/"

wb=createWorkbook()
# comparison European level-------


D=as.data.frame(matrix(ncol=17, nrow=1))
colnames(D)=c("Domain", 
              "NA_LC_2_digits","NA_LC_2_digits_0std","perc_NA_LC_2_digits",   "vars_NA_LC_2_digits",
              "NA_LU_2_digits","NA_LU_2_digits_0std", "perc_NA_LU_2_digits", "vars_NA_LU_2_digits",
              "NA_LC_3_digits", "NA_LC_3_digits_0std","perc_NA_LC_3_digits",  "vars_NA_LC_3_digits",              "NA_LU_3_digits","NA_LU_3_digits_0std", "perc_NA_LU_3_digits",  "vars_NA_LU_3_digits")

  # LC 2 digit
  F= openxlsx::read.xlsx(paste0(dire_comparison, "A_Estimates_comparison.xlsx"), sheet=1)
  #
  if(length(grep("x",F$Variable))>0){
    F <- F[-grep("x",F$Variable),]
  }
  if(length(grep("X",F$Variable))>0){
    F <- F[-grep("X",F$Variable),]
  }
  
  # Select only obs without given LC1 and LU1 values
  
  F <- F[!substr(F$Variable, 12, 15) %in% c("A00","A10","A20",
                                            "B00","B10","B20","B30","B40","B50","B60","B70","B80",
                                            "C00","C20",
                                            "D00",
                                            "E00",
                                            "F00",
                                            "G00","G10","G20",
                                            "H00","H10","H20","H30"),]
  #
  
  D[1,1]="Europe"
  D[1,2]=sum(is.na(F$twophase_Area_2022[(!is.na(F$standard_Area_2022))]))
  D[1,3]=sum(is.na(F$twophase_Area_2022[!is.na(F$standard_Area_2022) & F$standard_Area_2022==0]))
  D[1,4]=round(sum(is.na(F$twophase_Area_2022[(!is.na(F$standard_Area_2022))]))/length(F$twophase_Area_2022[(!is.na(F$standard_Area_2022))])*100,2)
  D[1,5]=paste(F$Variable[(is.na(F$twophase_Area_2022) & !is.na(F$standard_Area_2022))], collapse="; ")
  
  # LU 2 digits
  F= openxlsx::read.xlsx(paste0(dire_comparison, "A_Estimates_comparison.xlsx"), sheet=2)
  #
  if(length(grep("x",F$Variable))>0){
    F <- F[-grep("x",F$Variable),]
  }
  if(length(grep("X",F$Variable))>0){
    F <- F[-grep("X",F$Variable),]
  }
  ## Select only obs without given LC1 and LU1 values
  F <- F[!substr(F$Variable, 13, 16) %in% c("8","U100","U110",
                                            "U200","U220",
                                            "U300","U310","U320","U360",
                                            "U400"),]
  #
  
  D[1,6]=sum(is.na(F$twophase_Area_2022[(!is.na(F$standard_Area_2022))]))
  D[1,7]=sum(is.na(F$twophase_Area_2022[!is.na(F$standard_Area_2022) & F$standard_Area_2022==0]))
  D[1,8]=round(sum(is.na(F$twophase_Area_2022[(!is.na(F$standard_Area_2022))]))/length(F$twophase_Area_2022[(!is.na(F$standard_Area_2022))])*100,2)
  D[1,9]= paste(F$Variable[(is.na(F$twophase_Area_2022) & !is.na(F$standard_Area_2022))], collapse="; ")
    
  # LC 3 digits
  F= openxlsx::read.xlsx(paste0(dire_comparison, "A_Estimates_comparison.xlsx"), sheet=3)
  #
  if(length(grep("x",F$Variable))>0){
    F <- F[-grep("x",F$Variable),]
  }
  if(length(grep("X",F$Variable))>0){
    F <- F[-grep("X",F$Variable),]
  }
  #
  # Select only obs without given LC1 and LU1 values
  
  F <- F[!substr(F$Variable, 12, 15) %in% c("A00","A10","A20",
                                            "B00","B10","B20","B30","B40","B50","B60","B70","B80",
                                            "C00","C20",
                                            "D00",
                                            "E00",
                                            "F00",
                                            "G00","G10","G20",
                                            "H00","H10","H20","H30"),]
  #
  D[1,10]=sum(is.na(F$twophase_Area_2022[(!is.na(F$standard_Area_2022))]))
  D[1,11]=sum(is.na(F$twophase_Area_2022[!is.na(F$standard_Area_2022) & F$standard_Area_2022==0]))
  D[1,12]=round(sum(is.na(F$twophase_Area_2022[(!is.na(F$standard_Area_2022))]))/length(F$twophase_Area_2022[(!is.na(F$standard_Area_2022))])*100,2)
  D[1,13]=paste(F$Variable[(is.na(F$twophase_Area_2022) & !is.na(F$standard_Area_2022))], collapse="; ")
  
  # LU 3 digits 
  F= openxlsx::read.xlsx(paste0(dire_comparison, "A_Estimates_comparison.xlsx"), sheet=4)
  #
  if(length(grep("x",F$Variable))>0){
    F <- F[-grep("x",F$Variable),]
  }
  if(length(grep("X",F$Variable))>0){
    F <- F[-grep("X",F$Variable),]
  }
  #
  ## Select only obs without given LC1 and LU1 values
  F <- F[!substr(F$Variable, 13, 16) %in% c("8","U100","U110",
                                            "U200","U220",
                                            "U300","U310","U320","U360",
                                            "U400"),]
  #
  D[1,14]=sum(is.na(F$twophase_Area_2022[(!is.na(F$standard_Area_2022))]))
  D[1,15]=sum(is.na(F$twophase_Area_2022[!is.na(F$standard_Area_2022) & F$standard_Area_2022==0]))
  D[1,16]=round(sum(is.na(F$twophase_Area_2022[(!is.na(F$standard_Area_2022))]))/length(F$twophase_Area_2022[(!is.na(F$standard_Area_2022))])*100,2)
 D[1,17]=paste(F$Variable[(is.na(F$twophase_Area_2022) & !is.na(F$standard_Area_2022))], collapse="; ")
#xlsx::write.xlsx(D, file="Output/NA_count_2phases.xlsx", sheetName="Europe", row.names = FALSE)
  addWorksheet(wb, sheetName = "Europe")
  writeData(wb, sheet = "Europe", x = D, colNames = TRUE, rowNames = FALSE)
  


# comparison by country----------

D=as.data.frame(matrix(ncol=17, nrow=length(countries)))
colnames(D)=c("Country", 
              "NA_LC_2_digits","NA_LC_2_digits_0std","perc_NA_LC_2_digits", "vars_NA_LC_2_digits" ,
              "NA_LU_2_digits","NA_LU_2_digits_0std", "perc_NA_LU_2_digits",    "vars_NA_LU_2_digits",
              "NA_LC_3_digits", "NA_LC_3_digits_0std","perc_NA_LC_3_digits",  "vars_NA_LC_3_digits"   ,
              "NA_LU_3_digits","NA_LU_3_digits_0std", "perc_NA_LU_3_digits", "vars_NA_LU_3_digits")
for(i in 1:length(countries)){
  print(countries[i])
  # LC 2 digit
  F= openxlsx::read.xlsx(paste0(dire_comparison, countries[i],"_Estimates_comparison.xlsx"), sheet=1)
  #
  if(length(grep("x",F$Variable))>0){
    F <- F[-grep("x",F$Variable),]
  }
  if(length(grep("X",F$Variable))>0){
    F <- F[-grep("X",F$Variable),]
  }
  
  # Select only obs without given LC1 and LU1 values

  F <- F[!substr(F$Variable, 12, 15) %in% c("A00","A10","A20",
                                 "B00","B10","B20","B30","B40","B50","B60","B70","B80",
                                 "C00","C20",
                                 "D00",
                                 "E00",
                                 "F00",
                                 "G00","G10","G20",
                                 "H00","H10","H20","H30"),]
  #
  
 D[i,1]=countries[i]
 D[i,2]=sum(is.na(F$twophase_Area_2022[(!is.na(F$standard_Area_2022))]))
 D[i,3]=sum(is.na(F$twophase_Area_2022[!is.na(F$standard_Area_2022) & F$standard_Area_2022==0]))
 D[i,4]=round(sum(is.na(F$twophase_Area_2022[(!is.na(F$standard_Area_2022))]))/length(F$twophase_Area_2022[(!is.na(F$standard_Area_2022))])*100,2)
 D[i,5]= paste(F$Variable[(is.na(F$twophase_Area_2022) & !is.na(F$standard_Area_2022))], collapse="; ")
 
 # LU 2 digits
 F= openxlsx::read.xlsx(paste0(dire_comparison, countries[i],"_Estimates_comparison.xlsx"), sheet=2)
 #
 if(length(grep("x",F$Variable))>0){
   F <- F[-grep("x",F$Variable),]
 }
 if(length(grep("X",F$Variable))>0){
   F <- F[-grep("X",F$Variable),]
 }
 ## Select only obs without given LC1 and LU1 values
 F <- F[!substr(F$Variable, 13, 16) %in% c("8","U100","U110",
                             "U200","U220",
                             "U300","U310","U320","U360",
                             "U400"),]
 #
 
 D[i,6]=sum(is.na(F$twophase_Area_2022[(!is.na(F$standard_Area_2022))]))
 D[i,7]=sum(is.na(F$twophase_Area_2022[!is.na(F$standard_Area_2022) & F$standard_Area_2022==0]))
 D[i,8]=round(sum(is.na(F$twophase_Area_2022[(!is.na(F$standard_Area_2022))]))/length(F$twophase_Area_2022[(!is.na(F$standard_Area_2022))])*100,2)
 D[i,9]=paste(F$Variable[(is.na(F$twophase_Area_2022) & !is.na(F$standard_Area_2022))], collapse="; ")
 
 # LC 3 digits
 F= openxlsx::read.xlsx(paste0(dire_comparison, countries[i],"_Estimates_comparison.xlsx"), sheet=3)
 #
 if(length(grep("x",F$Variable))>0){
   F <- F[-grep("x",F$Variable),]
 }
 if(length(grep("X",F$Variable))>0){
   F <- F[-grep("X",F$Variable),]
 }
 #
 # Select only obs without given LC1 and LU1 values
 
 F <- F[!substr(F$Variable, 12, 15) %in% c("A00","A10","A20",
                                           "B00","B10","B20","B30","B40","B50","B60","B70","B80",
                                           "C00","C20",
                                           "D00",
                                           "E00",
                                           "F00",
                                           "G00","G10","G20",
                                           "H00","H10","H20","H30"),]
 #
 D[i,10]=sum(is.na(F$twophase_Area_2022[(!is.na(F$standard_Area_2022))]))
 D[i,11]=sum(is.na(F$twophase_Area_2022[!is.na(F$standard_Area_2022) & F$standard_Area_2022==0]))
 D[i,12]=round(sum(is.na(F$twophase_Area_2022[(!is.na(F$standard_Area_2022))]))/length(F$twophase_Area_2022[(!is.na(F$standard_Area_2022))])*100,2)
 D[i,13]= paste(F$Variable[(is.na(F$twophase_Area_2022) & !is.na(F$standard_Area_2022))], collapse="; ")
 
 # LU 3 digits 
 F=openxlsx::read.xlsx(paste0(dire_comparison, countries[i],"_Estimates_comparison.xlsx"), sheet=4)
 #
 if(length(grep("x",F$Variable))>0){
   F <- F[-grep("x",F$Variable),]
 }
 if(length(grep("X",F$Variable))>0){
   F <- F[-grep("X",F$Variable),]
 }
 #
 ## Select only obs without given LC1 and LU1 values
 F <- F[!substr(F$Variable, 13, 16) %in% c("8","U100","U110",
                                           "U200","U220",
                                           "U300","U310","U320","U360",
                                           "U400"),]
 #
 D[i,14]=sum(is.na(F$twophase_Area_2022[(!is.na(F$standard_Area_2022))]))
 D[i,15]=sum(is.na(F$twophase_Area_2022[!is.na(F$standard_Area_2022) & F$standard_Area_2022==0]))
 D[i,16]=round(sum(is.na(F$twophase_Area_2022[(!is.na(F$standard_Area_2022))]))/length(F$twophase_Area_2022[(!is.na(F$standard_Area_2022))])*100,2)
 D[i,17]= paste(F$Variable[(is.na(F$twophase_Area_2022) & !is.na(F$standard_Area_2022))], collapse="; ")
} 
#xlsx::write.xlsx(D, file="Output/NA_count_2phases.xlsx", sheetName="NUTS0", row.names = FALSE, append=TRUE)
addWorksheet(wb, sheetName = "NUTS0")
writeData(wb, sheet = "NUTS0", x = D, colNames = TRUE, rowNames = FALSE)

# comparison by country and by riparizioni: NUTS1---
i=1
print(countries[i])
# LC 2 digits
F= openxlsx::read.xlsx(paste0(dire_comparison, countries[i],"_NUTS1_Estimates_comparison.xlsx"), sheet=1)
#
if(length(grep("x",F$Variable))>0){
  F <- F[-grep("x",F$Variable),]
}
if(length(grep("X",F$Variable))>0){
  F <- F[-grep("X",F$Variable),]
}

# Select only obs without given LC1 and LU1 values

F <- F[!substr(F$Variable, 12, 15) %in% c("A00","A10","A20",
                                          "B00","B10","B20","B30","B40","B50","B60","B70","B80",
                                          "C00","C20",
                                          "D00",
                                          "E00",
                                          "F00",
                                          "G00","G10","G20",
                                          "H00","H10","H20","H30"),]
#

rip=length(unique(F$NUTS1))

D=as.data.frame(matrix(ncol=18, nrow=rip))
colnames(D)=c("Country", "NUTS1", 
              "NA_LC_2_digits", "NA_LC_2_digits_0std","perc_NA_LC_2_digits", "vars_NA_LC_2_digits",
              "NA_LU_2_digits","NA_LU_2_digits_0std", "perc_NA_LU_2_digits","vars_NA_LU_2_digits" ,
              "NA_LC_3_digits","NA_LC_3_digits_0std","perc_NA_LC_3_digits", "vars_NA_LC_3_digits",
              "NA_LU_3_digits", "NA_LU_3_digits_0std", "perc_NA_LU_3_digits", "vars_NA_LU_3_digits")
D$Country=countries[i]
D$NUTS1=unique(F$NUTS1) 

D[,3]=(aggregate(is.na(F$twophase_Area_2022[(!is.na(F$standard_Area_2022))])~NUTS1, F, sum))[,2]
#
D[,4]=(aggregate((!is.na(F$standard_Area_2022) & F$standard_Area_2022==0 & is.na(F$twophase_Area_2022))~NUTS1, F, sum))[,2]
#
D[,5]=round((aggregate(is.na(F$twophase_Area_2022[(!is.na(F$standard_Area_2022))])~NUTS1, F, sum))[,2]/aggregate(!is.na(F$standard_Area_2022)~NUTS1, F, sum)[,2]*100,2)

for(j in 1:length(unique(D$NUTS1))){
  
  D[j,6] = paste(F$Variable[(is.na(F$twophase_Area_2022) & !is.na(F$standard_Area_2022)) & F$NUTS1==D$NUTS1[j]], collapse="; ")
}

# LU 2 digits
F= openxlsx::read.xlsx(paste0(dire_comparison, countries[i],"_NUTS1_Estimates_comparison.xlsx"), sheet=2)
#
if(length(grep("x",F$Variable))>0){
  F <- F[-grep("x",F$Variable),]
}
if(length(grep("X",F$Variable))>0){
  F <- F[-grep("X",F$Variable),]
}
## Select only obs without given LC1 and LU1 values
F <- F[!substr(F$Variable, 13, 16) %in% c("8","U100","U110",
                                          "U200","U220",
                                          "U300","U310","U320","U360",
                                          "U400"),]
#


D[,7]=(aggregate(is.na(F$twophase_Area_2022[(!is.na(F$standard_Area_2022))])~NUTS1, F, sum))[,2]
#
D[,8]=(aggregate((!is.na(F$standard_Area_2022) & F$standard_Area_2022==0 & is.na(F$twophase_Area_2022))~NUTS1, F, sum))[,2]
#
D[,9]=round((aggregate(is.na(F$twophase_Area_2022[(!is.na(F$standard_Area_2022))])~NUTS1, F, sum))[,2]/aggregate(!is.na(F$standard_Area_2022)~NUTS1, F, sum)[,2]*100,2)


for(j in 1:length(unique(D$NUTS1))){
  
  D[j,10] = paste(F$Variable[(is.na(F$twophase_Area_2022) & !is.na(F$standard_Area_2022)) & F$NUTS1==D$NUTS1[j]], collapse="; ")
}

# LC 3 digits
F= openxlsx::read.xlsx(paste0(dire_comparison, countries[i],"_NUTS1_Estimates_comparison.xlsx"), sheet=3)
#
if(length(grep("x",F$Variable))>0){
  F <- F[-grep("x",F$Variable),]
}
if(length(grep("X",F$Variable))>0){
  F <- F[-grep("X",F$Variable),]
}

# Select only obs without given LC1 and LU1 values

F <- F[!substr(F$Variable, 12, 15) %in% c("A00","A10","A20",
                                          "B00","B10","B20","B30","B40","B50","B60","B70","B80",
                                          "C00","C20",
                                          "D00",
                                          "E00",
                                          "F00",
                                          "G00","G10","G20",
                                          "H00","H10","H20","H30"),]
#


D[,11]=(aggregate(is.na(F$twophase_Area_2022[(!is.na(F$standard_Area_2022))])~NUTS1, F, sum))[,2]
#

D[,12]=(aggregate((!is.na(F$standard_Area_2022) & F$standard_Area_2022==0 & is.na(F$twophase_Area_2022))~NUTS1, F, sum))[,2]
#
D[,13]=round((aggregate(is.na(F$twophase_Area_2022[(!is.na(F$standard_Area_2022))])~NUTS1, F, sum))[,2]/aggregate(!is.na(F$standard_Area_2022)~NUTS1, F, sum)[,2]*100,2)


for(j in 1:length(unique(D$NUTS1))){
  
  D[j,14] = paste(F$Variable[(is.na(F$twophase_Area_2022) & !is.na(F$standard_Area_2022)) & F$NUTS1==D$NUTS1[j]], collapse="; ")
}

# LU 3 digits
F= openxlsx::read.xlsx(paste0(dire_comparison, countries[i],"_NUTS1_Estimates_comparison.xlsx"), sheet=4)
#
if(length(grep("x",F$Variable))>0){
  F <- F[-grep("x",F$Variable),]
}
if(length(grep("X",F$Variable))>0){
  F <- F[-grep("X",F$Variable),]
}
## Select only obs without given LC1 and LU1 values
F <- F[!substr(F$Variable, 13, 16) %in% c("8","U100","U110",
                                          "U200","U220",
                                          "U300","U310","U320","U360",
                                          "U400"),]
#

D[,15]=(aggregate(is.na(F$twophase_Area_2022[(!is.na(F$standard_Area_2022))])~NUTS1, F, sum))[,2]
#

D[,16]=(aggregate((!is.na(F$standard_Area_2022) & F$standard_Area_2022==0 & is.na(F$twophase_Area_2022))~NUTS1, F, sum))[,2]
#
D[,17]=round((aggregate(is.na(F$twophase_Area_2022[(!is.na(F$standard_Area_2022))])~NUTS1, F, sum))[,2]/aggregate(!is.na(F$standard_Area_2022)~NUTS1, F, sum)[,2]*100,2)


for(j in 1:length(unique(D$NUTS1))){
  
  D[j,18] = paste(F$Variable[(is.na(F$twophase_Area_2022) & !is.na(F$standard_Area_2022)) & F$NUTS1==D$NUTS1[j]], collapse="; ")
}

E=D

for(i in 2:length(countries)){
  print(countries[i])
  # LC 2 digits
  F= openxlsx::read.xlsx(paste0(dire_comparison, countries[i],"_NUTS1_Estimates_comparison.xlsx"), sheet=1)
  #
  if(length(grep("x",F$Variable))>0){
    F <- F[-grep("x",F$Variable),]
  }
  if(length(grep("X",F$Variable))>0){
    F <- F[-grep("X",F$Variable),]
  }
  
  # Select only obs without given LC1 and LU1 values
  
  F <- F[!substr(F$Variable, 12, 15) %in% c("A00","A10","A20",
                                            "B00","B10","B20","B30","B40","B50","B60","B70","B80",
                                            "C00","C20",
                                            "D00",
                                            "E00",
                                            "F00",
                                            "G00","G10","G20",
                                            "H00","H10","H20","H30"),]
  #
  
  rip=length(unique(F$NUTS1))
  
  D=as.data.frame(matrix(ncol=18, nrow=rip))
  colnames(D)=c("Country", "NUTS1", 
                "NA_LC_2_digits", "NA_LC_2_digits_0std","perc_NA_LC_2_digits", "vars_NA_LC_2_digits",
                "NA_LU_2_digits","NA_LU_2_digits_0std", "perc_NA_LU_2_digits", "vars_NA_LU_2_digits",
                "NA_LC_3_digits","NA_LC_3_digits_0std","perc_NA_LC_3_digits","vars_NA_LC_3_digits",
                "NA_LU_3_digits", "NA_LU_3_digits_0std", "perc_NA_LU_3_digits", "vars_NA_LU_3_digits")
  D$Country=countries[i]
  D$NUTS1=unique(F$NUTS1) 
  
  D[,3]=(aggregate(is.na(F$twophase_Area_2022[(!is.na(F$standard_Area_2022))])~NUTS1, F, sum))[,2]
  #
  D[,4]=(aggregate((!is.na(F$standard_Area_2022) & F$standard_Area_2022==0 & is.na(F$twophase_Area_2022))~NUTS1, F, sum))[,2]
  #
  D[,5]=round((aggregate(is.na(F$twophase_Area_2022[(!is.na(F$standard_Area_2022))])~NUTS1, F, sum))[,2]/aggregate(!is.na(F$standard_Area_2022)~NUTS1, F, sum)[,2]*100,2)
  
  for(n in 1:length(unique(D$NUTS1))){
    
    D[n,6] = paste(F$Variable[(is.na(F$twophase_Area_2022) & !is.na(F$standard_Area_2022)) & F$NUTS1==D$NUTS1[n]], collapse="; ")
  }
  
  #LU 2 digits
  F= openxlsx::read.xlsx(paste0(dire_comparison, countries[i],"_NUTS1_Estimates_comparison.xlsx"), sheet=2)
  #
  if(length(grep("x",F$Variable))>0){
    F <- F[-grep("x",F$Variable),]
  }
  if(length(grep("X",F$Variable))>0){
    F <- F[-grep("X",F$Variable),]
  }
  ## Select only obs without given LC1 and LU1 values
  F <- F[!substr(F$Variable, 13, 16) %in% c("8","U100","U110",
                                            "U200","U220",
                                            "U300","U310","U320","U360",
                                            "U400"),]
  #
  
  
  D[,7]=(aggregate(is.na(F$twophase_Area_2022[(!is.na(F$standard_Area_2022))])~NUTS1, F, sum))[,2]
  #
  D[,8]=(aggregate((!is.na(F$standard_Area_2022) & F$standard_Area_2022==0 & is.na(F$twophase_Area_2022))~NUTS1, F, sum))[,2]
  #
  D[,9]=round((aggregate(is.na(F$twophase_Area_2022[(!is.na(F$standard_Area_2022))])~NUTS1, F, sum))[,2]/aggregate(!is.na(F$standard_Area_2022)~NUTS1, F, sum)[,2]*100,2)
  
  for(n in 1:length(unique(D$NUTS1))){
    
    D[n,10] = paste(F$Variable[(is.na(F$twophase_Area_2022) & !is.na(F$standard_Area_2022)) & F$NUTS1==D$NUTS1[n]], collapse="; ")
  }
  # LC 3 digits
  F= openxlsx::read.xlsx(paste0(dire_comparison, countries[i],"_NUTS1_Estimates_comparison.xlsx"), sheet=3)
  #
  if(length(grep("x",F$Variable))>0){
    F <- F[-grep("x",F$Variable),]
  }
  if(length(grep("X",F$Variable))>0){
    F <- F[-grep("X",F$Variable),]
  }
  
  # Select only obs without given LC1 and LU1 values
  
  F <- F[!substr(F$Variable, 12, 15) %in% c("A00","A10","A20",
                                            "B00","B10","B20","B30","B40","B50","B60","B70","B80",
                                            "C00","C20",
                                            "D00",
                                            "E00",
                                            "F00",
                                            "G00","G10","G20",
                                            "H00","H10","H20","H30"),]
  #
  
  
  D[,11]=(aggregate(is.na(F$twophase_Area_2022[(!is.na(F$standard_Area_2022))])~NUTS1, F, sum))[,2]
  #
  
  D[,12]=(aggregate((!is.na(F$standard_Area_2022) & F$standard_Area_2022==0 & is.na(F$twophase_Area_2022))~NUTS1, F, sum))[,2]
  #
  D[,13]=round((aggregate(is.na(F$twophase_Area_2022[(!is.na(F$standard_Area_2022))])~NUTS1, F, sum))[,2]/aggregate(!is.na(F$standard_Area_2022)~NUTS1, F, sum)[,2]*100,2)
  
  for(n in 1:length(unique(D$NUTS1))){
    
    D[n,14] = paste(F$Variable[(is.na(F$twophase_Area_2022) & !is.na(F$standard_Area_2022)) & F$NUTS1==D$NUTS1[n]], collapse="; ")
  }
  # LU 3 digits
  F= openxlsx::read.xlsx(paste0(dire_comparison, countries[i],"_NUTS1_Estimates_comparison.xlsx"), sheet=4)
  #
  if(length(grep("x",F$Variable))>0){
    F <- F[-grep("x",F$Variable),]
  }
  if(length(grep("X",F$Variable))>0){
    F <- F[-grep("X",F$Variable),]
  }
  ## Select only obs without given LC1 and LU1 values
  F <- F[!substr(F$Variable, 13, 16) %in% c("8","U100","U110",
                                            "U200","U220",
                                            "U300","U310","U320","U360",
                                            "U400"),]
  #
  
  
  D[,15]=(aggregate(is.na(F$twophase_Area_2022[(!is.na(F$standard_Area_2022))])~NUTS1, F, sum))[,2]
  #
  
  D[,16]=(aggregate((!is.na(F$standard_Area_2022) & F$standard_Area_2022==0 & is.na(F$twophase_Area_2022))~NUTS1, F, sum))[,2]
  #
  D[,17]=round((aggregate(is.na(F$twophase_Area_2022[(!is.na(F$standard_Area_2022))])~NUTS1, F, sum))[,2]/aggregate(!is.na(F$standard_Area_2022)~NUTS1, F, sum)[,2]*100,2)
  
  for(n in 1:length(unique(D$NUTS1))){
    
    D[n,18] = paste(F$Variable[(is.na(F$twophase_Area_2022) & !is.na(F$standard_Area_2022)) & F$NUTS1==D$NUTS1[n]], collapse="; ")
  }
  
  E=rbind(E, D)
}
#xlsx::write.xlsx(E, file="Output/NA_count_2phases.xlsx", sheetName="NUTS1", append=TRUE, row.names = FALSE)
addWorksheet(wb, sheetName = "NUTS1")
writeData(wb, sheet = "NUTS1", x = E, colNames = TRUE, rowNames = FALSE)



# # comparison by country and by regioni: NUTS2---
i=1
print(countries[i])
# LC 2 digits
F= openxlsx::read.xlsx(paste0(dire_comparison, countries[i],"_NUTS2_Estimates_comparison.xlsx"), sheet=1)
#
if(length(grep("x",F$Variable))>0){
  F <- F[-grep("x",F$Variable),]
}
if(length(grep("X",F$Variable))>0){
  F <- F[-grep("X",F$Variable),]
}

# Select only obs without given LC1 and LU1 values

F <- F[!substr(F$Variable, 12, 15) %in% c("A00","A10","A20",
                                          "B00","B10","B20","B30","B40","B50","B60","B70","B80",
                                          "C00","C20",
                                          "D00",
                                          "E00",
                                          "F00",
                                          "G00","G10","G20",
                                          "H00","H10","H20","H30"),]
#

rip=length(unique(F$NUTS2))

D=as.data.frame(matrix(ncol=18, nrow=rip))
colnames(D)=c("Country", "NUTS2", 
              "NA_LC_2_digits", "NA_LC_2_digits_0std","perc_NA_LC_2_digits", "vars_NA_LC_2_digits",
              "NA_LU_2_digits","NA_LU_2_digits_0std", "perc_NA_LU_2_digits", "vars_NA_LU_2_digits",
              "NA_LC_3_digits","NA_LC_3_digits_0std","perc_NA_LC_3_digits","vars_NA_LC_3_digits",
              "NA_LU_3_digits", "NA_LU_3_digits_0std", "perc_NA_LU_3_digits", "vars_NA_LU_3_digits")
D$Country=countries[i]
D$NUTS2=unique(F$NUTS2) 

D[,3]=(aggregate(is.na(F$twophase_Area_2022[(!is.na(F$standard_Area_2022))])~NUTS2, F, sum))[,2]
#
D[,4]=(aggregate((!is.na(F$standard_Area_2022) & F$standard_Area_2022==0 & is.na(F$twophase_Area_2022))~NUTS2, F, sum))[,2]
#
D[,5]=round((aggregate(is.na(F$twophase_Area_2022[(!is.na(F$standard_Area_2022))])~NUTS2, F, sum))[,2]/aggregate(!is.na(F$standard_Area_2022)~NUTS2, F, sum)[,2]*100,2)

for(n in 1:length(unique(D$NUTS2))){
  
  D[n,6] = paste(F$Variable[(is.na(F$twophase_Area_2022) & !is.na(F$standard_Area_2022)) & F$NUTS2==D$NUTS2[n]], collapse="; ")
}

# LU 2 digits
F= openxlsx::read.xlsx(paste0(dire_comparison, countries[i],"_NUTS2_Estimates_comparison.xlsx"), sheet=2)
#
if(length(grep("x",F$Variable))>0){
  F <- F[-grep("x",F$Variable),]
}
if(length(grep("X",F$Variable))>0){
  F <- F[-grep("X",F$Variable),]
}
## Select only obs without given LC1 and LU1 values
F <- F[!substr(F$Variable, 13, 16) %in% c("8","U100","U110",
                                          "U200","U220",
                                          "U300","U310","U320","U360",
                                          "U400"),]
#


D[,7]=(aggregate(is.na(F$twophase_Area_2022[(!is.na(F$standard_Area_2022))])~NUTS2, F, sum))[,2]
#
D[,8]=(aggregate((!is.na(F$standard_Area_2022) & F$standard_Area_2022==0 & is.na(F$twophase_Area_2022))~NUTS2, F, sum))[,2]
#
D[,9]=round((aggregate(is.na(F$twophase_Area_2022[(!is.na(F$standard_Area_2022))])~NUTS2, F, sum))[,2]/aggregate(!is.na(F$standard_Area_2022)~NUTS2, F, sum)[,2]*100,2)

for(n in 1:length(unique(D$NUTS2))){
  
  D[n,10] = paste(F$Variable[(is.na(F$twophase_Area_2022) & !is.na(F$standard_Area_2022)) & F$NUTS2==D$NUTS2[n]], collapse="; ")
}

# LC 3 digits
F= openxlsx::read.xlsx(paste0(dire_comparison, countries[i],"_NUTS2_Estimates_comparison.xlsx"), sheet=3)
#
if(length(grep("x",F$Variable))>0){
  F <- F[-grep("x",F$Variable),]
}
if(length(grep("X",F$Variable))>0){
  F <- F[-grep("X",F$Variable),]
}

# Select only obs without given LC1 and LU1 values

F <- F[!substr(F$Variable, 12, 15) %in% c("A00","A10","A20",
                                          "B00","B10","B20","B30","B40","B50","B60","B70","B80",
                                          "C00","C20",
                                          "D00",
                                          "E00",
                                          "F00",
                                          "G00","G10","G20",
                                          "H00","H10","H20","H30"),]
#

D[,11]=(aggregate(is.na(F$twophase_Area_2022[(!is.na(F$standard_Area_2022))])~NUTS2, F, sum))[,2]
#

D[,12]=(aggregate((!is.na(F$standard_Area_2022) & F$standard_Area_2022==0 & is.na(F$twophase_Area_2022))~NUTS2, F, sum))[,2]
#
D[,13]=round((aggregate(is.na(F$twophase_Area_2022[(!is.na(F$standard_Area_2022))])~NUTS2, F, sum))[,2]/aggregate(!is.na(F$standard_Area_2022)~NUTS2, F, sum)[,2]*100,2)

for(n in 1:length(unique(D$NUTS2))){
  
  D[n,14] = paste(F$Variable[(is.na(F$twophase_Area_2022) & !is.na(F$standard_Area_2022)) & F$NUTS2==D$NUTS2[n]], collapse="; ")
}

#LU 3 digits
F= openxlsx::read.xlsx(paste0(dire_comparison, countries[i],"_NUTS2_Estimates_comparison.xlsx"), sheet=4)
#
if(length(grep("x",F$Variable))>0){
  F <- F[-grep("x",F$Variable),]
}
if(length(grep("X",F$Variable))>0){
  F <- F[-grep("X",F$Variable),]
}
## Select only obs without given LC1 and LU1 values
F <- F[!substr(F$Variable, 13, 16) %in% c("8","U100","U110",
                                          "U200","U220",
                                          "U300","U310","U320","U360",
                                          "U400"),]
#


D[,15]=(aggregate(is.na(F$twophase_Area_2022[(!is.na(F$standard_Area_2022))])~NUTS2, F, sum))[,2]
#

D[,16]=(aggregate((!is.na(F$standard_Area_2022) & F$standard_Area_2022==0 & is.na(F$twophase_Area_2022))~NUTS2, F, sum))[,2]
#
D[,17]=round((aggregate(is.na(F$twophase_Area_2022[(!is.na(F$standard_Area_2022))])~NUTS2, F, sum))[,2]/aggregate(!is.na(F$standard_Area_2022)~NUTS2, F, sum)[,2]*100,2)

for(n in 1:length(unique(D$NUTS2))){
  
  D[n,18] = paste(F$Variable[(is.na(F$twophase_Area_2022) & !is.na(F$standard_Area_2022)) & F$NUTS2==D$NUTS2[n]], collapse="; ")
}


E=D

for(i in 2:length(countries)){
  print(countries[i])
  # LC 2 digits
  F= openxlsx::read.xlsx(paste0(dire_comparison, countries[i],"_NUTS2_Estimates_comparison.xlsx"), sheet=1)
  #
  if(length(grep("x",F$Variable))>0){
    F <- F[-grep("x",F$Variable),]
  }
  if(length(grep("X",F$Variable))>0){
    F <- F[-grep("X",F$Variable),]
  }
  
  # Select only obs without given LC1 and LU1 values
  
  F <- F[!substr(F$Variable, 12, 15) %in% c("A00","A10","A20",
                                            "B00","B10","B20","B30","B40","B50","B60","B70","B80",
                                            "C00","C20",
                                            "D00",
                                            "E00",
                                            "F00",
                                            "G00","G10","G20",
                                            "H00","H10","H20","H30"),]
  #
  
  rip=length(unique(F$NUTS2))
  
  D=as.data.frame(matrix(ncol=18, nrow=rip))
  colnames(D)=c("Country", "NUTS2", 
                "NA_LC_2_digits", "NA_LC_2_digits_0std","perc_NA_LC_2_digits", "vars_NA_LC_2_digits",
                "NA_LU_2_digits","NA_LU_2_digits_0std", "perc_NA_LU_2_digits", "vars_NA_LU_2_digits",
                "NA_LC_3_digits","NA_LC_3_digits_0std","perc_NA_LC_3_digits","vars_NA_LC_3_digits",
                "NA_LU_3_digits", "NA_LU_3_digits_0std", "perc_NA_LU_3_digits", "vars_NA_LU_3_digits")
  D$Country=countries[i]
  D$NUTS2=unique(F$NUTS2) 
  
  D[,3]=(aggregate(is.na(F$twophase_Area_2022[(!is.na(F$standard_Area_2022))])~NUTS2, F, sum))[,2]
  #
  D[,4]=(aggregate((!is.na(F$standard_Area_2022) & F$standard_Area_2022==0 & is.na(F$twophase_Area_2022))~NUTS2, F, sum))[,2]
  #
  D[,5]=round((aggregate(is.na(F$twophase_Area_2022[(!is.na(F$standard_Area_2022))])~NUTS2, F, sum))[,2]/aggregate(!is.na(F$standard_Area_2022)~NUTS2, F, sum)[,2]*100,2)
  
  for(n in 1:length(unique(D$NUTS2))){
    
    D[n,6] = paste(F$Variable[(is.na(F$twophase_Area_2022) & !is.na(F$standard_Area_2022)) & F$NUTS2==D$NUTS2[n]], collapse="; ")
  }
  
  #LU 2 digits
  F= openxlsx::read.xlsx(paste0(dire_comparison, countries[i],"_NUTS2_Estimates_comparison.xlsx"), sheet=2)
  #
  if(length(grep("x",F$Variable))>0){
    F <- F[-grep("x",F$Variable),]
  }
  if(length(grep("X",F$Variable))>0){
    F <- F[-grep("X",F$Variable),]
  }
  ## Select only obs without given LC1 and LU1 values
  F <- F[!substr(F$Variable, 13, 16) %in% c("8","U100","U110",
                                            "U200","U220",
                                            "U300","U310","U320","U360",
                                            "U400"),]
  #
  
  
  D[,7]=(aggregate(is.na(F$twophase_Area_2022[(!is.na(F$standard_Area_2022))])~NUTS2, F, sum))[,2]
  #
  D[,8]=(aggregate((!is.na(F$standard_Area_2022) & F$standard_Area_2022==0 & is.na(F$twophase_Area_2022))~NUTS2, F, sum))[,2]
  #
  D[,9]=round((aggregate(is.na(F$twophase_Area_2022[(!is.na(F$standard_Area_2022))])~NUTS2, F, sum))[,2]/aggregate(!is.na(F$standard_Area_2022)~NUTS2, F, sum)[,2]*100,2)
  
  for(n in 1:length(unique(D$NUTS2))){
    
    D[n,10] = paste(F$Variable[(is.na(F$twophase_Area_2022) & !is.na(F$standard_Area_2022)) & F$NUTS2==D$NUTS2[n]], collapse="; ")
  }
  
  # LC 3 digits
  F= openxlsx::read.xlsx(paste0(dire_comparison, countries[i],"_NUTS2_Estimates_comparison.xlsx"), sheet=3)
  #
  if(length(grep("x",F$Variable))>0){
    F <- F[-grep("x",F$Variable),]
  }
  if(length(grep("X",F$Variable))>0){
    F <- F[-grep("X",F$Variable),]
  }
  
  # Select only obs without given LC1 and LU1 values
  
  F <- F[!substr(F$Variable, 12, 15) %in% c("A00","A10","A20",
                                            "B00","B10","B20","B30","B40","B50","B60","B70","B80",
                                            "C00","C20",
                                            "D00",
                                            "E00",
                                            "F00",
                                            "G00","G10","G20",
                                            "H00","H10","H20","H30"),]
  #
  
  
  D[,11]=(aggregate(is.na(F$twophase_Area_2022[(!is.na(F$standard_Area_2022))])~NUTS2, F, sum))[,2]
  #
  
  D[,12]=(aggregate((!is.na(F$standard_Area_2022) & F$standard_Area_2022==0 & is.na(F$twophase_Area_2022))~NUTS2, F, sum))[,2]
  #
  D[,13]=round((aggregate(is.na(F$twophase_Area_2022[(!is.na(F$standard_Area_2022))])~NUTS2, F, sum))[,2]/aggregate(!is.na(F$standard_Area_2022)~NUTS2, F, sum)[,2]*100,2)
  
  for(n in 1:length(unique(D$NUTS2))){
    
    D[n,14] = paste(F$Variable[(is.na(F$twophase_Area_2022) & !is.na(F$standard_Area_2022)) & F$NUTS2==D$NUTS2[n]], collapse="; ")
  }
  
  # LU 3 digits
  F= openxlsx::read.xlsx(paste0(dire_comparison, countries[i],"_NUTS2_Estimates_comparison.xlsx"), sheet=4)
  #
  if(length(grep("x",F$Variable))>0){
    F <- F[-grep("x",F$Variable),]
  }
  if(length(grep("X",F$Variable))>0){
    F <- F[-grep("X",F$Variable),]
  }
  ## Select only obs without given LC1 and LU1 values
  F <- F[!substr(F$Variable, 13, 16) %in% c("8","U100","U110",
                                            "U200","U220",
                                            "U300","U310","U320","U360",
                                            "U400"),]
  #
  
  
  D[,15]=(aggregate(is.na(F$twophase_Area_2022[(!is.na(F$standard_Area_2022))])~NUTS2, F, sum))[,2]
  #
  
  D[,16]=(aggregate((!is.na(F$standard_Area_2022) & F$standard_Area_2022==0 & is.na(F$twophase_Area_2022))~NUTS2, F, sum))[,2]
  #
  D[,17]=round((aggregate(is.na(F$twophase_Area_2022[(!is.na(F$standard_Area_2022))])~NUTS2, F, sum))[,2]/aggregate(!is.na(F$standard_Area_2022)~NUTS2, F, sum)[,2]*100,2)
  
  for(n in 1:length(unique(D$NUTS2))){
    
    D[n,18] = paste(F$Variable[(is.na(F$twophase_Area_2022) & !is.na(F$standard_Area_2022)) & F$NUTS2==D$NUTS2[n]], collapse="; ")
  }
  
  E=rbind(E, D)
}

#xlsx::write.xlsx(E, file="Output/NA_count_2phases.xlsx", sheetName="NUTS2",append=TRUE, row.names = FALSE)
addWorksheet(wb, sheetName = "NUTS2")
writeData(wb, sheet = "NUTS2", x = E, colNames = TRUE, rowNames = FALSE)

# Write all
#setwd("D:/Google Drive/LUCAS 2025/Task 1 - ESTIMATES/2.TWO PHASES light")
#setwd(direnew1 )
saveWorkbook(wb, "Output/NA_count_2phases_light.xlsx", overwrite = TRUE)
