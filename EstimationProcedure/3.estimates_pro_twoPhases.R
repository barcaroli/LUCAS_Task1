##############################################################
# LUCAS - Calibration and estimation by country 2022
# preparation of inputs for the two-phases calibration step
##############################################################
#----------------------------------------------------------
# Input: survey_2022_cal_wgt_standard.txt
# Output: /estimates2022proTwoPhases
#----------------------------------------------------------
start_time <- Sys.time()
options(stringsAsFactors = TRUE)
library(data.table)
library(ReGenesees)
library(openxlsx)

# Prepare the path for the output
dire <- getwd()
direnew1 <- paste(dire, "/3.estimates2022proTwoPhases", sep = "")
if (dir.exists(direnew1))
  unlink(direnew1,recursive=TRUE)
if (!dir.exists(direnew1))
  dir.create(direnew1)
# 


#------------------------------------------
# Read survey 2022
#------------------------------------------
s2022 <- fread("survey_2022_cal_wgt_standard.txt")

# Fill the missing digits
s2022$SURVEY_LC1 <- as.character(s2022$SURVEY_LC1)
table(s2022$SURVEY_LC1)
s2022$SURVEY_LC1 <- ifelse(nchar(as.character(s2022$SURVEY_LC1)) == 2,
                           paste0(s2022$SURVEY_LC1,"X"),s2022$SURVEY_LC1)
s2022$SURVEY_LC1 <- ifelse(nchar(as.character(s2022$SURVEY_LC1)) == 1,
                           paste0(s2022$SURVEY_LC1,"XX"),s2022$SURVEY_LC1)
table(s2022$SURVEY_LC1)

#------------------------------------------
# First and second phase
#------------------------------------------

s2022$flag_two <- grepl("X",s2022$SURVEY_LC1)
s2022$flag_two <- ifelse(substr(s2022$SURVEY_LC1,1,1)=="B",FALSE,s2022$flag_two)
table(s2022$flag_two)
s2022$flag_two <- ifelse(s2022$SURVEY_LC1 %in% c("A00",
                                                 "A10","A20",
                                                 "B00","B10","B20","B30","B40","B50","B60","B70","B80",
                                                 "C00",
                                                 "C20",
                                                 "D00",
                                                 "E00",
                                                 "F00",
                                                 "G00",
                                                 "G10","G20",
                                                 "H00","H10","H20","H30") |
                         s2022$SURVEY_LU1 %in% c("8","U100","U110",
                                                 "U200","U220",
                                                 "U300","U310","U320","U360",
                                                 "U400"),
                         TRUE,s2022$flag_two)
s2022$flag_two <- ifelse(s2022$flag_two==FALSE,"Yes","No")
options(scipen=100)
addmargins(table(s2022$SURVEY_LC1,s2022$flag_two))
table(s2022$flag_two)
# round(addmargins(prop.table(table(s2022$SURVEY_LC1,s2022$flag_two))),5)

s2022$flag_two <- as.factor(s2022$flag_two)

################################
# some processing
################################
s2022$area <- 1
################################
s2022$SURVEY_LC1 <- factor(s2022$SURVEY_LC1)
s2022$SURVEY_LU1 <- factor(s2022$SURVEY_LU1)
s2022$STRATUM_LUCAS <- as.factor(s2022$STRATUM_LUCAS)
s2022$ones <- 1
s2022$SURVEY_LC1_1 <- as.factor(substr(as.character(s2022$SURVEY_LC1),1,1))
table(s2022$SURVEY_LC1_1, useNA="ifany")
s2022$SURVEY_LC1_2 <- as.factor(substr(as.character(s2022$SURVEY_LC1),1,2))
table(s2022$SURVEY_LC1_2, useNA="ifany")
s2022$SURVEY_LC1_3 <- as.factor(substr(as.character(s2022$SURVEY_LC1),1,3))
table(s2022$SURVEY_LC1_3, useNA="ifany")
s2022$SURVEY_LU1_1 <- as.factor(substr(as.character(s2022$SURVEY_LU1),1,2))
table(s2022$SURVEY_LU1_1, useNA="ifany")
s2022$SURVEY_LU1_2 <- as.factor(substr(as.character(s2022$SURVEY_LU1),1,3))
table(s2022$SURVEY_LU1_2, useNA="ifany")
s2022$SURVEY_LU1_3 <- as.factor(substr(as.character(s2022$SURVEY_LU1),1,4))
table(s2022$SURVEY_LU1_3, useNA="ifany")
###########################################################
s2022$fao_class_name <- as.factor(s2022$fao_class_name)
s2022$settlement <- as.factor(s2022$settlement)
s2022$lue <- as.factor(s2022$lue)
s2022$lud <- as.factor(s2022$lud)

################################################################
# Read areas
# areas <- read.csv(paste0(path_data,"areas_2015_2024.csv"),colClasses = c(rep('character',4), rep('numeric',10)))
# areas$NUTS0 <- substr(areas$NUTS2,1,2)
################################################################


###########################################################################
# Assign the new NUTS24 to the sample
# s2022 <- merge(s2022,master[,c("POINT_ID","NUTS0_24","NUTS1_24","NUTS2_24","NUTS3_24")],by="POINT_ID",all.x=TRUE)
s2022$NUTS0_24 <- as.factor(s2022$NUTS0_24)
s2022$NUTS1_24 <- as.factor(s2022$NUTS1_24)
s2022$NUTS2_24 <- as.factor(s2022$NUTS2_24)
s2022$NUTS3_24 <- as.factor(s2022$NUTS3_24)
a <- s2022[is.na(s2022$NUTS2_24),]

s2022$area <- 1


##########
# LOOP
##########

paesi <- levels(as.factor(s2022$NUTS0_24))
i = which(paesi=="CY")
i
# sintesi=s2022 %>% group_by(NUTS0_24) %>% summarise(nlevel_nuts1=length(unique(NUTS1_24)), nlevel_nuts2=length(unique(NUTS2_24)))

for (i in c(1:length(paesi))) {
  country <- paesi[i]
  cat("\n Country: ",country,"\n")
  s <- s2022[s2022$NUTS0_24==country,c("POINT_ID","fpc","Pop2023","SURVEY_OBS_TYPE",
                      "NUTS0_24","NUTS1_24","NUTS2_24",
                      "SURVEY_LC1","SURVEY_LU1","SURVEY_LC1_1","SURVEY_LU1_1",
                      "SURVEY_LC1_2","SURVEY_LU1_2","SURVEY_LC1_3","SURVEY_LU1_3",
                      "WGT_LUCAS", "STRATUM_LUCAS","settlement","fao_class_name",
                      "lud","lue","settl_pc","area","cal_wgt","flag_two")]
  s<-droplevels(s)
  des <- e.svydesign(data=s, 
                     ids= ~ POINT_ID, 
                     strata= ~ STRATUM_LUCAS, 
                     weights = ~ cal_wgt, 
                     self.rep.str= NULL, 
                     fpc= ~fpc, 
                     check.data= TRUE)
  ls <- find.lon.strata(des)
  # if (!is.null(ls)) des <- collapse.strata(des, block.vars=~NUTS2_24)
  if (!is.null(ls)) des <- collapse.strata(des)

  ##########################   
  # ESTIMATION
  ##########################   
  est_LC1_LU1_NUTS0_24 <- svystatTM(des, ~ area +
                             SURVEY_LC1_1+
                             SURVEY_LU1_1+
                             SURVEY_LC1_2+
                             SURVEY_LU1_2+
                             SURVEY_LC1_3+
                             SURVEY_LU1_3+
                             settlement+
                             settl_pc+
                             fao_class_name+
                             lue+
                             lud,
                           by = ~ flag_two,
                           estimator="Total",
                           vartype=c("se","cv"),
                           conf.int= TRUE, 
                           conf.lev= 0.95)
  est_LC1_LU1_NUTS0_24_t <- as.data.frame(t(est_LC1_LU1_NUTS0_24))
  est_LC1_LU1_NUTS0_24_t$variable <- row.names(est_LC1_LU1_NUTS0_24_t)
  est_LC1_LU1_NUTS0_24_t <- est_LC1_LU1_NUTS0_24_t[-1,]
  if (!"No" %in% colnames(est_LC1_LU1_NUTS0_24_t)) {
    est_LC1_LU1_NUTS0_24_t$No <- 0
  }
  est_LC1_LU1_NUTS0_24_t <- est_LC1_LU1_NUTS0_24_t[,c("No","Yes","variable")]
  
  est_LC1_LU1_NUTS1_24 <- svystatTM(des, ~ area +
                                      SURVEY_LC1_1+
                                      SURVEY_LU1_1+
                                      SURVEY_LC1_2+
                                      SURVEY_LU1_2+
                                      SURVEY_LC1_3+
                                      SURVEY_LU1_3+
                                      settlement+
                                      fao_class_name+
                                      lue+
                                      lud,
                                    by = ~ NUTS1_24 + flag_two,
                                    estimator="Total",
                                    vartype=c("se","cv"),
                                    conf.int= TRUE,
                                    conf.lev= 0.95)
  
  if (length(levels(s$NUTS1_24)) > 1){
    est_LC1_LU1_NUTS1_24_t <- as.data.frame(t(est_LC1_LU1_NUTS1_24))
    est_LC1_LU1_NUTS1_24_t$variable <- row.names(est_LC1_LU1_NUTS1_24_t)
    est_LC1_LU1_NUTS1_24_t <- est_LC1_LU1_NUTS1_24_t[-1,]
    if (!"No" %in% colnames(est_LC1_LU1_NUTS1_24_t)) {
      est_LC1_LU1_NUTS1_24_t$No <- 0
    }
  }
  
  est_LC1_LU1_NUTS2_24 <- svystatTM(des, ~ area +
                                      SURVEY_LC1_1+
                                      SURVEY_LU1_1+
                                      SURVEY_LC1_2+
                                      SURVEY_LU1_2+
                                      SURVEY_LC1_3+
                                      SURVEY_LU1_3+
                                      settlement+
                                      fao_class_name+
                                      lue+
                                      lud,
                                    by = ~ NUTS2_24 + flag_two,
                                    estimator="Total",
                                    vartype=c("se","cv"),
                                    conf.int= TRUE, 
                                    conf.lev= 0.95)
  if (length(levels(s$NUTS2_24)) > 1){
    est_LC1_LU1_NUTS2_24_t <- as.data.frame(t(est_LC1_LU1_NUTS2_24))
    est_LC1_LU1_NUTS2_24_t$variable <- row.names(est_LC1_LU1_NUTS2_24_t)
    est_LC1_LU1_NUTS2_24_t <- est_LC1_LU1_NUTS2_24_t[-1,]
    if (!"No" %in% colnames(est_LC1_LU1_NUTS2_24_t)) {
      est_LC1_LU1_NUTS2_24_t$No <- 0
    }
  }
  

  #---------------------------------------------------------------
  # Write outputs
  #---------------------------------------------------------------
  filename <- paste(country,'_est_LC1_LU1_NUTS0_24_2022.csv',sep="")
  write.table(est_LC1_LU1_NUTS0_24_t,file = file.path(direnew1, filename),sep=",",dec=".",
              row.names=FALSE,col.names=TRUE,quote=FALSE)
  if (length(levels(s$NUTS1_24)) > 1) {
    filename <- paste(country,'_est_LC1_LU1_NUTS1_24_2022.csv',sep="")
    write.table(est_LC1_LU1_NUTS1_24_t,file = file.path(direnew1, filename),sep=",",dec=".",
                row.names=FALSE,col.names=TRUE,quote=FALSE)
  }
  if (length(levels(s$NUTS1_24)) == 1) {
    filename <- paste(country,'_est_LC1_LU1_NUTS1_24_2022.csv',sep="")
    est_LC1_LU1_NUTS1_24_t <- est_LC1_LU1_NUTS0_24_t
    colnames(est_LC1_LU1_NUTS1_24_t) <- c(paste0(country,"0.No"),paste0(country,"0.Yes"),"variable")
    est_LC1_LU1_NUTS1_24_t <- rbind(c("No","Yes","flag_two"),est_LC1_LU1_NUTS1_24_t)
    write.table(est_LC1_LU1_NUTS1_24_t,file = file.path(direnew1, filename),sep=",",dec=".",
                row.names=FALSE,col.names=TRUE,quote=FALSE)
  }
  if (length(levels(s$NUTS2_24)) > 1) {
    filename <- paste(country,'_est_LC1_LU1_NUTS2_24_2022.csv',sep="")
    write.table(est_LC1_LU1_NUTS2_24_t,file = file.path(direnew1, filename),sep=",",dec=".",
                row.names=FALSE,col.names=TRUE,quote=FALSE)
  }
  if (length(levels(s$NUTS2_24)) == 1) {
    filename <- paste(country,'_est_LC1_LU1_NUTS2_24_2022.csv',sep="")
    est_LC1_LU1_NUTS2_24_t <- est_LC1_LU1_NUTS0_24_t
    colnames(est_LC1_LU1_NUTS2_24_t) <- c(paste0(country,"00.No"),paste0(country,"00.Yes"),"variable")
    est_LC1_LU1_NUTS2_24_t <- rbind(c("No","Yes","flag_two"),est_LC1_LU1_NUTS2_24_t)
    write.table(est_LC1_LU1_NUTS2_24_t,file = file.path(direnew1, filename),sep=",",dec=".",
                row.names=FALSE,col.names=TRUE,quote=FALSE)
  }
    
    

}
end_time <- Sys.time() 
execution_time <- end_time - start_time 
print(execution_time)
