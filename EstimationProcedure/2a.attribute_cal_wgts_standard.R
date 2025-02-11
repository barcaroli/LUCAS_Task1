#---------------------------------------------------
# Script to assign calibrated weights to survey data
#---------------------------------------------------
# Input: LUCAS22_corrected_complete.csv
#        /2.StandardEstimates202
# Output: Survey_2022_cal_wgt_standard.txt
#---------------------------------------------------
library(data.table)
options(stringsAsFactors = TRUE)
#---------------------------------------------------------------------------
# 2022
s <- fread("LUCAS22_corrected_complete.csv")
countries <- levels(as.factor(s$NUTS0_24))
s_wgt <- NULL
for (i in (1:length(countries))) {
  cat("\n Country: ",countries[i],"\n")
  st <- paste("wgt <- read.delim('./2.StandardWeights2022/",countries[i],"_calibrated_wgts_2022.txt',dec='.')",sep="")
  eval(parse(text=st))
  s2 <- merge(s,wgt,by=c("POINT_ID"))
  s_wgt <- rbind(s_wgt,s2)
  summary(s_wgt$cal_wgt)
}
summary(s_wgt$cal_wgt)
dim(s_wgt)
write.table(s_wgt,file="Survey_2022_cal_wgt_standard.txt",sep="\t",row.names=F,col.names=T,quote=F)
