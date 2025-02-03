#---------------------------------------------------
# Script to assign calibrated weights to survey data
#---------------------------------------------------
library(data.table)
options(stringsAsFactors = TRUE)
setwd("D:/Google Drive/LUCAS 2025/Task 1 - ESTIMATES/1.STANDARD")
path_data <- "D:/Google Drive/LUCAS 2025/2.DATA/"
#---------------------------------------------------------------------------
# 2022
s <- fread(paste0(path_data,"LUCAS22_corrected_complete.csv"))
# s <- merge(s,m)
# colnames(s)[3] <- "initial_weights"
# s$stratum <- as.factor(paste(s$NUTS2_16,s$STRATUM_LABEL,sep="_"))
countries <- levels(as.factor(s$NUTS0_24))
setwd("D:/Google Drive/LUCAS 2025/Task 1 - ESTIMATES/1.STANDARD/weights2022")
s_wgt <- NULL
for (i in (1:length(countries))) {
  cat("\n Country: ",countries[i],"\n")
  st <- paste("wgt <- read.delim('",countries[i],"_calibrated_wgts_2022.txt',dec='.')",sep="")
  eval(parse(text=st))
  s2 <- merge(s,wgt,by=c("POINT_ID"))
  s_wgt <- rbind(s_wgt,s2)
  summary(s_wgt$cal_wgt)
}
summary(s_wgt$cal_wgt)
dim(s_wgt)
write.table(s_wgt,file=paste0(path_data,"Survey_2022_cal_wgt_standard.txt"),sep="\t",row.names=F,col.names=T,quote=F)
