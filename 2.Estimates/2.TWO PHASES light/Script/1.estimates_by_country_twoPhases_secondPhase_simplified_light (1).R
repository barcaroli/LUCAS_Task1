#----------------------------------------------
# Script to execute second phase estimation
# based on known totals of LC and LU
# obtained from the first phase
#----------------------------------------------
# SIMPLIFIED VERSION: 
# calibration only on LC and LU estimates
# 1 digit from first phase
# EXCLUDES ONLY ANOMALOUS VALUES (MAINTAINS PI)
#----------------------------------------------
# N.B. this script works only if called 
# by 1.estimates_by_country_twoPhases.R
#----------------------------------------------
# library(ReGenesees)
# load("firstPhase.RData")

# dir_estimate_toadd="C:/Users/UTENTE/Desktop/Progetto_LUCAS/Task1/2.TWO PHASES/estimates2022proTwoPhasesNoField/estimates2022proTwoPhasesNoField/"
dir_estimate_toadd="D:/Google Drive/LUCAS 2025/Task 1 - ESTIMATES/2.TWO PHASES/estimates2022proTwoPhasesNoField/"

# flag data to identify observations in the second sample 
# Select only Field
#s2 <- s[s$SURVEY_OBS_TYPE == 1,]
# if no filter on obs_type:
s2 <- s
cat("\nN.obs before selection:",nrow(s2))
# Select only obs without 'x' in LC1 and LU1
s2 <- droplevels(s2)
s$ones=1
s2$ones=1

table(s2$SURVEY_LC1)
table(s2$SURVEY_LU1)
if(length(grep("x",s2$SURVEY_LC1))>0){
  s2 <- s2[-grep("x",s2$SURVEY_LC1),]
}
if(length(grep("x",s2$SURVEY_LU1))>0){
  s2 <- s2[-grep("x",s2$SURVEY_LU1),]
}
if(length(grep("X",s2$SURVEY_LC1))>0){
  s2 <-  s2[-grep("X",s2$SURVEY_LC1),]
}
if(length(grep("X",s2$SURVEY_LU1))>0){
  s2 <-  s2[-grep("X",s2$SURVEY_LU1),]
}
s2 <- droplevels(s2)

# Select only obs without given LC1 and LU1 values
s2 <- s2[!s2$SURVEY_LC1 %in% c("A00","A10","A20",
                               "B00","B10","B20","B30","B40","B50","B60","B70","B80",
                               "C00","C20",
                               "D00",
                               "E00",
                               "F00",
                               "G00","G10","G20",
                               "H00","H10","H20","H30"),]
s2 <- s2[!s2$SURVEY_LU1 %in% c("8","U100","U110",
                               "U200","U220",
                               "U300","U310","U320","U360",
                               "U400"),]
cat("\nN.obs after selection:",nrow(s2))
s2 <- droplevels(s2)
table(s2$SURVEY_LC1)
table(s2$SURVEY_LU1)

###########################################################
# Update fpc
###########################################################
num<-aggregate(ones~STRATUM_LUCAS,s2, FUN=sum)
colnames(num)[2] = "n2"
den<-aggregate(ones~STRATUM_LUCAS,s, FUN=sum)
colnames(den)[2] = "n1"
fpc<-merge(num,den,by="STRATUM_LUCAS")
fpc$fpc=fpc$n2/fpc$n1
s2$fpc = NULL
s2<-merge(s2,fpc[,c("STRATUM_LUCAS","fpc")],by="STRATUM_LUCAS")

save(s, file=paste0("Samples/", country, "_s_1stphase.RData"))
save(s2, file=paste0("Samples/", country, "_s_2ndphase.RData"))
# design
s2 <- s2[order(s2$STRATUM_LUCAS),]
des2 <- e.svydesign(data=s2, 
                    ids= ~ POINT_ID, 
                    strata= ~ STRATUM_LUCAS, 
                    # weights = ~ WGT_LUCAS,
                    weights = ~ cal_wgt,
                    self.rep.str= NULL, 
                    fpc= ~fpc, 
                    check.data= TRUE)
ls <- find.lon.strata(des2)
print(ls)
if (!is.null(ls)) {
  des2 <-tryCatch({
    des2 <- collapse.strata(des2, block.vars = ~NUTS2_24)
  },
  error = function(e) {
    cat("Error:", e$message, "\n")  
    des2 <- collapse.strata(des2)  
    return(des2)  
  })
}



#-----------------------------
# Prepare calibration
#-----------------------------
# Add LC1 and LU1 known totals to the current known totals vector
popfill2 <- pop.template(data=des2,
                         calmodel=   ~
                           SURVEY_LC1_1 +
                           SURVEY_LU1_1 - 1,
)
popfill2[1,] <- est_LC1_LU1[row.names(est_LC1_LU1) %in% names(popfill2),c("Total")]
cal2 <- e.calibrate(design=des2,
                    df.population= popfill2,
                    calmodel=   ~
                      SURVEY_LC1_1 +
                      SURVEY_LU1_1 - 1,
                    calfun= 'linear', bounds =   c(0.05,Inf))
check.cal(cal2)
#print(a)
#UWE(cal2)
# summary(s$WGT_LUCAS)
# sum(s$WGT_LUCAS)
# sum(m$ones)
# sum(m$point_area)
# cal2$prob <- cal2$prob/cal2$variables$point_area
sum(weights(cal2))
summary(weights(cal2))

s2$cal_wgt <- weights(cal2)

# design
des3 <- e.svydesign(data=s2, 
                    ids= ~ POINT_ID, 
                    strata= ~ STRATUM_LUCAS, 
                    weights = ~ cal_wgt, 
                    self.rep.str= NULL, 
                    fpc= ~fpc, 
                    check.data= TRUE)
ls <- find.lon.strata(des3)
#if (!is.null(ls)) des3 <- collapse.strata(des3)
if (!is.null(ls)) {
  des3 <-tryCatch({
    des3 <- collapse.strata(des3, block.vars = ~NUTS2_24)
  },
  error = function(e) {
    cat("Error:", e$message, "\n")  
    des3 <- collapse.strata(des3)  
    return(des3)  
  })
}

est_LC1_LU1_2nd <- svystatTM(des3, ~ 
                             SURVEY_LC1_2+
                             SURVEY_LU1_2+
                             SURVEY_LC1_3+
                             SURVEY_LU1_3,
                             estimator="Total",
                             vartype=c("se"))
                             # vartype=c("se","cv"),
                             # conf.int= TRUE, 
                             # conf.lev= 0.95)

# update SE 
est_LC1_LU1_2nd$key = rownames(est_LC1_LU1_2nd)
est_toadd = read.csv(paste0(dir_estimate_toadd, "\\", country, "_est_LC1_LU1_NUTS0_24_2022.csv"))

est_toadd = est_toadd[startsWith(x=est_toadd$variable, prefix="SE"),c("Yes", "variable")]
#est_toadd$key = sub("\\_2nd_phase.*", "", est_toadd$variable)
est_toadd$key = sub("SE.Total.", "", est_toadd$variable)
# to long format
est_toadd= est_toadd %>% select(-variable) %>%  pivot_longer(-key,names_to="NUTS0_24", values_to = "SE") %>% select(-NUTS0_24)


# est_toadd = read.csv("\\\\pc.istat.it\\xendesktop\\DaaS\\ilaria.bombelli\\Desktop\\GruppiDiLAvoro\\Progetto_LUCAS\\Task1\\1.STANDARD\\estimates2022\\AT2_est_LC1_LU1_2022 - Copia.csv")
# est_toadd = est_toadd %>% select(Variable, SE)
# est_toadd = est_toadd[endsWith(est_toadd$Variable, "_2nd_phase"),]
# est_toadd$key = sub("\\_2nd_phase.*", "", est_toadd$Variable)
est_LC1_LU1_2nd = merge(est_LC1_LU1_2nd, est_toadd[,c("key", "SE")], by="key", all.x=TRUE)
est_LC1_LU1_2nd$SE = sqrt(est_LC1_LU1_2nd$SE.x^2+est_LC1_LU1_2nd$SE.y^2)
est_LC1_LU1_2nd$'CI.l(95%)' = est_LC1_LU1_2nd$Total - 1.96*est_LC1_LU1_2nd$SE
est_LC1_LU1_2nd$'CI.u(95%)' = est_LC1_LU1_2nd$Total + 1.96*est_LC1_LU1_2nd$SE
est_LC1_LU1_2nd$CV = est_LC1_LU1_2nd$SE/est_LC1_LU1_2nd$Total
rownames(est_LC1_LU1_2nd)=est_LC1_LU1_2nd$key
est_LC1_LU1_2nd$key <- NULL
est_LC1_LU1_2nd$SE.x <- NULL
est_LC1_LU1_2nd$SE.y <- NULL


# by nuts1
est_LC1_LU1_NUTS1_24_2nd <- svystatTM(des3, ~ 
                                        SURVEY_LC1_2+
                                        SURVEY_LU1_2+
                                        SURVEY_LC1_3+
                                        SURVEY_LU1_3,
                                      by = ~ NUTS1_24,
                                      estimator="Total",
                                      vartype=c("se"))
                                      # vartype=c("se","cv"),
                                      # conf.int= TRUE,
                                      # conf.lev= 0.95)

#manipulation
est_LC1_LU1_NUTS1_24_2nd = est_LC1_LU1_NUTS1_24_2nd%>% 
  gather(v, value, -NUTS1_24) %>% 
  separate(v, sep="\\.", into = c("var","var2", "col")) %>% 
  mutate(key = case_when(is.na(col)~paste(var, var2, sep="."), 
                         !is.na(col)~paste(var2, col, sep=".")), 
         var = replace_na(var, "Total")) %>% 
  select(-var2, -col)%>% 
  arrange(NUTS1_24, key) %>% 
  spread(var, value)


#import file:
est_toadd = read.csv(paste0(dir_estimate_toadd, "\\", country, "_est_LC1_LU1_NUTS1_24_2022.csv"))

est_toadd = est_toadd[startsWith(x=est_toadd$variable, prefix="SE"),]
est_toadd = est_toadd %>% select(c(variable, contains("Yes")))
# change colnames
colnames(est_toadd)[grep("Yes", colnames(est_toadd))] = sub(".Yes", "",colnames(est_toadd)[grep("Yes", colnames(est_toadd))] )

#est_toadd$key = sub("\\_2nd_phase.*", "", est_toadd$variable)
est_toadd$key = sub("SE.", "", est_toadd$variable)
# to long format
est_toadd= est_toadd %>% select(-variable) %>%  pivot_longer(-key,names_to="NUTS1_24", values_to = "SE") %>% mutate(SE=as.numeric(SE))

# merge
est_LC1_LU1_NUTS1_24_2nd = merge(est_LC1_LU1_NUTS1_24_2nd, est_toadd, by=c("NUTS1_24", "key"), all.x=TRUE)

#new updated variables
est_LC1_LU1_NUTS1_24_2nd$SE = sqrt(est_LC1_LU1_NUTS1_24_2nd$SE.x^2+est_LC1_LU1_NUTS1_24_2nd$SE.y^2)
est_LC1_LU1_NUTS1_24_2nd$'CI.l(95%)' = est_LC1_LU1_NUTS1_24_2nd$Total - 1.96*est_LC1_LU1_NUTS1_24_2nd$SE
est_LC1_LU1_NUTS1_24_2nd$'CI.u(95%)' = est_LC1_LU1_NUTS1_24_2nd$Total + 1.96*est_LC1_LU1_NUTS1_24_2nd$SE
est_LC1_LU1_NUTS1_24_2nd$CV = est_LC1_LU1_NUTS1_24_2nd$SE/est_LC1_LU1_NUTS1_24_2nd$Total
est_LC1_LU1_NUTS1_24_2nd$SE.x <- NULL
est_LC1_LU1_NUTS1_24_2nd$SE.y <- NULL

# from long to wide. 
est_LC1_LU1_NUTS1_24_2nd = est_LC1_LU1_NUTS1_24_2nd %>%  pivot_wider(
  names_from = key,
  values_from = c(Total, SE, 'CI.l(95%)', 'CI.u(95%)', CV), 
  names_sep = "."
) 

est_LC1_LU1_NUTS1_24_2nd=as.data.frame(est_LC1_LU1_NUTS1_24_2nd)
#adjust colnames
colnames(est_LC1_LU1_NUTS1_24_2nd)[grep("Total.Total",colnames(est_LC1_LU1_NUTS1_24_2nd))] = sub("Total.", "", colnames(est_LC1_LU1_NUTS1_24_2nd)[grep("Total.Total",colnames(est_LC1_LU1_NUTS1_24_2nd))])

#adjust rownames
rownames(est_LC1_LU1_NUTS1_24_2nd)=est_LC1_LU1_NUTS1_24_2nd$NUTS1_24




est_LC1_LU1_NUTS2_24_2nd <- svystatTM(des3, ~ 
                                        SURVEY_LC1_2+
                                        SURVEY_LU1_2+
                                        SURVEY_LC1_3+
                                        SURVEY_LU1_3,
                                      by = ~ NUTS2_24,
                                      estimator="Total",
                                      vartype="se")
                                      # vartype=c("se","cv"),
                                      # conf.int= TRUE, 
                                      # conf.lev= 0.95)
#manipulation
est_LC1_LU1_NUTS2_24_2nd = est_LC1_LU1_NUTS2_24_2nd%>% 
  gather(v, value, -NUTS2_24) %>% 
  separate(v, sep="\\.", into = c("var","var2", "col")) %>% 
  mutate(key = case_when(is.na(col)~paste(var, var2, sep="."), 
                         !is.na(col)~paste(var2, col, sep=".")), 
         var = replace_na(var, "Total")) %>% 
  select(-var2, -col)%>% 
  arrange(NUTS2_24, key) %>% 
  spread(var, value)


#import file:
est_toadd = read.csv(paste0(dir_estimate_toadd, "\\", country, "_est_LC1_LU1_NUTS2_24_2022.csv"))

est_toadd = est_toadd[startsWith(x=est_toadd$variable, prefix="SE"),]
est_toadd = est_toadd %>% select(c(variable, contains("Yes")))
# change colnames
colnames(est_toadd)[grep("Yes", colnames(est_toadd))] = sub(".Yes", "",colnames(est_toadd)[grep("Yes", colnames(est_toadd))] )

#est_toadd$key = sub("\\_2nd_phase.*", "", est_toadd$variable)
est_toadd$key = sub("SE.", "", est_toadd$variable)
# to long format
est_toadd= est_toadd %>% select(-variable) %>%  pivot_longer(-key,names_to="NUTS2_24", values_to = "SE") %>% mutate(SE=as.numeric(SE))

# merge
est_LC1_LU1_NUTS2_24_2nd = merge(est_LC1_LU1_NUTS2_24_2nd, est_toadd, by=c("NUTS2_24", "key"), all.x=TRUE)

#new updated variables
est_LC1_LU1_NUTS2_24_2nd$SE = sqrt(est_LC1_LU1_NUTS2_24_2nd$SE.x^2+est_LC1_LU1_NUTS2_24_2nd$SE.y^2)
est_LC1_LU1_NUTS2_24_2nd$'CI.l(95%)' = est_LC1_LU1_NUTS2_24_2nd$Total - 1.96*est_LC1_LU1_NUTS2_24_2nd$SE
est_LC1_LU1_NUTS2_24_2nd$'CI.u(95%)' = est_LC1_LU1_NUTS2_24_2nd$Total + 1.96*est_LC1_LU1_NUTS2_24_2nd$SE
est_LC1_LU1_NUTS2_24_2nd$CV = est_LC1_LU1_NUTS2_24_2nd$SE/est_LC1_LU1_NUTS2_24_2nd$Total
est_LC1_LU1_NUTS2_24_2nd$SE.x <- NULL
est_LC1_LU1_NUTS2_24_2nd$SE.y <- NULL

# from long to wide. 
est_LC1_LU1_NUTS2_24_2nd = est_LC1_LU1_NUTS2_24_2nd %>%  pivot_wider(
  names_from = key,
  values_from = c(Total, SE, 'CI.l(95%)', 'CI.u(95%)', CV), 
  names_sep = "."
) 

est_LC1_LU1_NUTS2_24_2nd=as.data.frame(est_LC1_LU1_NUTS2_24_2nd)
#adjust colnames
colnames(est_LC1_LU1_NUTS2_24_2nd)[grep("Total.Total",colnames(est_LC1_LU1_NUTS2_24_2nd))] = sub("Total.", "", colnames(est_LC1_LU1_NUTS2_24_2nd)[grep("Total.Total",colnames(est_LC1_LU1_NUTS2_24_2nd))])
#adjust rownames
rownames(est_LC1_LU1_NUTS2_24_2nd)=est_LC1_LU1_NUTS2_24_2nd$NUTS2_24



# Transposed ----------------------------------------------
if (length(levels(s$NUTS1_24)) > 1){
  est_LC1_LU1_NUTS1_24_2nd_t <- as.data.frame(t(est_LC1_LU1_NUTS1_24_2nd[,-1]))
  est_LC1_LU1_NUTS1_24_2nd_t$variable <- row.names(est_LC1_LU1_NUTS1_24_2nd_t)
  #est_LC1_LU1_NUTS1_24_2nd_t$variable[1] <- "variable"
}
#-----------------------------------------------------------
if (length(levels(s$NUTS2_24)) > 1){
  est_LC1_LU1_NUTS2_24_2nd_t <- as.data.frame(t(est_LC1_LU1_NUTS2_24_2nd[,-1]))
  est_LC1_LU1_NUTS2_24_2nd_t$variable <- row.names(est_LC1_LU1_NUTS2_24_2nd_t)
  #est_LC1_LU1_NUTS2_24_2nd_t$variable[1] <- "variable"
}

