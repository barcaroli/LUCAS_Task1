#---------------------------------------------------------------
# Script to assign calibrated weights (two-phase) to survey data
#---------------------------------------------------------------
#----------------------------------------------------------
# Input: LUCAS22_corrected_complete.csv
#        /4.TwoPhaseWeights2022
# Output: Survey_2022_wgt_2nd_phase.txt
#----------------------------------------------------------
library(data.table)
options(stringsAsFactors = TRUE)

#---------------------------------------------------------------------------
# 2022
s <- fread("LUCAS22_corrected_complete.csv")
countries <- levels(as.factor(s$NUTS0_24))

s_wgt <- NULL
for (i in (1:length(countries))) {
  cat("\n Country: ",countries[i],"\n")
  st <- paste("wgt <- read.delim('./4.TwoPhaseWeights2022/",countries[i],"_wgts_2nd_phase.txt',dec='.')",sep="")
  eval(parse(text=st))
  s1 <- s[s$NUTS0_24 == countries[i],]
  s2 <- merge(s1,wgt,by=c("POINT_ID"),all.x=TRUE)
  s_wgt <- rbind(s_wgt,s2)
  summary(s_wgt$wgt_2nd_phase)
}
summary(s_wgt$wgt_2nd_phase)
s_wgt$wgt_2nd_phase <- ifelse(is.na(s_wgt$wgt_2nd_phase),0,s_wgt$wgt_2nd_phase)
summary(s_wgt$wgt_2nd_phase)
sum(s_wgt$wgt_2nd_phase)
dim(s_wgt)
a <- s_wgt[s_wgt$wgt_2nd_phase == 0,]
nrow(a) / nrow(s_wgt)
write.table(s_wgt,file="Survey_2022_cal_wgt_2nd_phase.txt",sep="\t",row.names=F,col.names=T,quote=F)


