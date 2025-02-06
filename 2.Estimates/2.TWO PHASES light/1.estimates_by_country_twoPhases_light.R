##############################################################
# Script to execute first phase estimation
# IN THE SECOND PHASE
# EXCLUDES ONLY ANOMALOUS VALUES (MAINTAINS PI)
##############################################################
start_time <- Sys.time()
options(stringsAsFactors = TRUE)
library(data.table)
library(sf)
library(ReGenesees)
library(openxlsx)
library(dplyr)
library(tidyr)

setwd("D:/Google Drive/LUCAS 2025/Task 1 - ESTIMATES/2.TWO PHASES light")
# setwd("C:\\Users\\UTENTE\\Google Drive\\LUCAS 2025\\Task 1 - ESTIMATES\\2.TWO PHASES light")
path_data <- "D:/Google Drive/LUCAS 2025/2.DATA/"
path_master <- "D:/Google Drive/LUCAS 2025/2.Master/"
# path_data <- "C:\\Users\\UTENTE\\Google Drive/LUCAS 2025/2.DATA/"
#setwd("//pc.istat.it/xendesktop/DaaS/ilaria.bombelli/Desktop/GruppiDiLAvoro/Progetto_LUCAS/Task1/V2.TWO PHASES light")
# setwd("C:/Users/UTENTE/Desktop/Progetto_LUCAS/Task1/V2.TWO PHASES light")
# path_data="C:/Users/UTENTE/Desktop/Progetto_LUCAS/Task1/DATA/"


# Prepare the path for the output
dire <- getwd()
direnew1 <- paste(dire, "/estimates2022", sep = "")
if (dir.exists(direnew1))
  unlink(direnew1,recursive=TRUE)
if (!dir.exists(direnew1))
  dir.create(direnew1)
# 
direnew2 <- paste(dire, "/weights2022/", sep = "")
if (dir.exists(direnew2))
  unlink(direnew2,recursive=TRUE)
if (!dir.exists(direnew2))
  dir.create(direnew2)
# 
direnew3 <- paste(dire, "/Samples", sep = "")
if (dir.exists(direnew3))
  unlink(direnew3,recursive=TRUE)
if (!dir.exists(direnew3))
  dir.create(direnew3)
# 
#------------------------------------------
# Read survey 2022
#------------------------------------------
# Read the survey data calibrated by the standard procedure
s2022 <- fread(paste0(path_data,"Survey_2022_cal_wgt_standard.txt"))
table(s2022$SURVEY_LC1)
table(s2022$SURVEY_LU1)
# Fill the missing digits
s2022$SURVEY_LC1 <- as.character(s2022$SURVEY_LC1)
table(s2022$SURVEY_LC1)
s2022$SURVEY_LC1 <- ifelse(nchar(as.character(s2022$SURVEY_LC1)) == 2,
                           paste0(s2022$SURVEY_LC1,"x"),s2022$SURVEY_LC1)
s2022$SURVEY_LC1 <- ifelse(nchar(as.character(s2022$SURVEY_LC1)) == 1,
                           paste0(s2022$SURVEY_LC1,"xx"),s2022$SURVEY_LC1)
table(s2022$SURVEY_LC1)

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
###########################################################
# Eliminate incomplete LC and LU
# a <- s2022[nchar(as.character(s2022$SURVEY_LC1_2)) < 2,]
# # 688
# b <- s2022[nchar(as.character(s2022$SURVEY_LC1_3)) < 3,]
# # 6190
# c <- s2022[nchar(as.character(s2022$SURVEY_LU1_2)) < 3,]
# d <- s2022[nchar(as.character(s2022$SURVEY_LU1_3)) < 4,]
# s2022 <- s2022[!(nchar(as.character(s2022$SURVEY_LC1_2)) < 2) &
#                  !(nchar(as.character(s2022$SURVEY_LC1_3)) < 3),]
###########################################################
# Calculate fpc
###########################################################
num<-aggregate(s2022$ones,by=list(s2022$STRATUM_LUCAS),FUN=sum)
num$n=num$x
num$x<-NULL
den<-aggregate(s2022$WGT_LUCAS,by=list(s2022$STRATUM_LUCAS),FUN=sum)
den$N=den$x
den$x<-NULL
fpc<-merge(num,den,by="Group.1")
fpc$STRATUM_LUCAS<-fpc$Group.1
fpc$fpc=fpc$n/fpc$N
s2022$fpc <- NULL
s2022<-merge(s2022,fpc[,c("STRATUM_LUCAS","fpc")],by="STRATUM_LUCAS")

################################################################
# Read areas
areas <- read.csv(paste0(path_data,"areas_2015_2024.csv"),colClasses = c(rep('character',4), rep('numeric',10)))
################################################################


#########################################
# MASTER
#########################################
load(paste0(path_master,"master_complete.RData"))

###########################################################################
# Assign the new NUTS24 to the sample
# s2022 <- merge(s2022,master[,c("POINT_ID","NUTS0_24","NUTS1_24","NUTS2_24","NUTS3_24")],by="POINT_ID",all.x=TRUE)
s2022$NUTS0_24 <- as.factor(s2022$NUTS0_24)
s2022$NUTS1_24 <- as.factor(s2022$NUTS1_24)
s2022$NUTS2_24 <- as.factor(s2022$NUTS2_24)
s2022$NUTS3_24 <- as.factor(s2022$NUTS3_24)
a <- s2022[is.na(s2022$NUTS2_24),]
#b <- master[is.na(master$NUTS2_24),]

###########################################################################
# Read population
# pop <- read.csv(paste0(path_data,"EU_population_2009_2023.csv"))
# colnames(pop) <- c("NUTS","Pop2009","Pop2012","Pop2015","Pop2018","Pop2023")
# s2022 <- merge(s2022,pop[,c("NUTS","Pop2023")],by.x="NUTS0_24",by.y="NUTS")


###########################################################
# settl_pc
###########################################################################
table(as.numeric(s2022$settlement))
s2022$settl_pc <- (as.numeric(s2022$settlement)-1) * 1000 / s2022$Pop2023
summary(s2022$settl_pc)

###########################################################################
# exclusion of master points without points in the sample
###########################################################################
# master<-master[master$NUTS0_24 %in% unique(s2022$NUTS0_24),]


###########################################################################
# exclusion of some NUTS2  --> ask for confirm!
###########################################################################
# excl_nuts2 <- c("ES63","ES64","ES70","FR9","PT20","PT30") 
# master <- master[!(master$NUTS2_24 %in% excl_nuts2),]

###########################################################################
# Eliminate Master points with NA in point_area
###########################################################################
# summary(master$point_area)
# a <- master[is.na(master$point_area),]
# master <- master[!is.na(master$point_area),]


###########################################################################
# master processing
###########################################################################
# master$ELEV2 <- ifelse(master$ELEV_DEM < 100, 1,
#                        ifelse(master$ELEV_DEM < 200, 2,
#                               ifelse(master$ELEV < 500, 3,
#                                      ifelse(master$ELEV_DEM < 1000, 4,
#                                             ifelse(master$ELEV_DEM < 1500, 5, 6)))))
table(master$ELEV2,useNA="ifany")
# master$ELEV2 <- as.factor(master$ELEV2)
# 
# master$CLC18_1d<-factor(substr(master$CLC18_vett,1,1))
# master$NUTS0_24 <- factor(master$NUTS0_24)
# master$NUTS1_24 <- factor(master$NUTS1_24)
# master$NUTS2_24 <- factor(master$NUTS2_24)
# master$NUTS3_24 <- factor(master$NUTS3_24)
# master$BCK18_R <- factor(master$BCK18_R)
# master$BCK21_R <- factor(master$BCK21_R)
# master$CLC18_vett<-factor(master$CLC18_vett)
# master$imperviousness <- master$IMD18_10 /100
# master$art_imp <- master$imperviousness * master$point_area
# master$ones<-1


##########
# LOOP
##########

paesi <- levels(as.factor(s2022$NUTS0_24))
i = which(paesi=="AT")
i
# sintesi=s2022 %>% group_by(NUTS0_24) %>% summarise(nlevel_nuts1=length(unique(NUTS1_24)), nlevel_nuts2=length(unique(NUTS2_24)))

for (i in c(1:length(paesi))) {
  country <- paesi[i]
  cat("\n Country: ",country,"\n")
  # seleziono dal master le variabili che possono essere utili
  m <- master[master$NUTS0_24 == country,c("POINT_ID","point_area","NUTS2_24","ELEV2","GRA18_10","FTY18_10","BCK21_R")]
  ##########################################
  # m <- m[!is.na(m$NUTS2_24),]
  # eliminate empty levels in factor variables in m 
  # m<-droplevels(m)
  # select country sample with variables of interest
  s <- s2022[s2022$NUTS0_24==country,c("POINT_ID","fpc","Pop2023","SURVEY_OBS_TYPE",
                                       "NUTS0_24","NUTS1_24","NUTS2_24",
                      "SURVEY_LC1","SURVEY_LU1","SURVEY_LC1_1","SURVEY_LU1_1",
                      "SURVEY_LC1_2","SURVEY_LU1_2","SURVEY_LC1_3","SURVEY_LU1_3",
                      "WGT_LUCAS", "STRATUM_LUCAS","settlement","fao_class_name",
                      "lud","lue","settl_pc","area","cal_wgt")]
  s <- s[!is.na(s$WGT_LUCAS),]
  s <-droplevels(s)
  m$GRA18_10 <- as.factor(m$GRA18_10)
  m$BCK21_R <- as.factor(m$BCK21_R)
  m$FTY18_10 <- as.factor(m$FTY18_10)
  m$ELEV2 <- as.factor(m$ELEV2)
  s <- merge(s,m[,c("POINT_ID","point_area","ELEV2","GRA18_10","FTY18_10","BCK21_R")])
  # align levels of factor variables in s and m
  # levels(s$ELEV2)    <- levels(m$ELEV2)
  # levels(s$NUTS2_24) <- levels(m$NUTS2_24)
  # levels(s$BCK21_R)  <- levels(m$BCK21_R)
  # levels(s$BCK18_R)  <- levels(m$BCK18_R)
  # levels(s$GRA18_10) <- levels(m$GRA18_10)
  # levels(s$FTY18_10) <- levels(m$FTY18_10)
  # levels(s$CLC18_1d) <- levels(m$CLC18_1d)
  cal <- e.svydesign(data=s, 
                     ids= ~ POINT_ID, 
                     strata= ~ STRATUM_LUCAS, 
                     weights = ~ cal_wgt, 
                     self.rep.str= NULL, 
                     fpc= ~fpc, 
                     check.data= TRUE)
  ls <- find.lon.strata(cal)
  #if (!is.null(ls)) cal <- collapse.strata(cal, block.vars=~NUTS2_24)
  #if (!is.null(ls)) cal <- collapse.strata(cal)
  if (!is.null(ls)) {
    cal <-tryCatch({
      cal <- collapse.strata(cal, block.vars = ~NUTS2_24)
    },
    error = function(e) {
      cat("Error:", e$message, "\n")
      cal <- collapse.strata(cal)
      return(cal)
    })
  }
  ###############   
  # CALIBRATION
  ###############
  # areas_country <- areas[substr(areas$NUTS2,1,2)==country,]
  # model1=as.formula("~point_area:NUTS2_24 + point_area:(ELEV2 + BCK21_R + GRA18_10 + FTY18_10) - 1")
  # model2=as.formula("~point_area + point_area:(ELEV2 + BCK21_R + GRA18_10 + FTY18_10) - 1")
  # # model1=as.formula("~point_area + NUTS2_24 + ELEV2 + BCK21_R + GRA18_10 + FTY18_10 - 1")
  # # model2=as.formula("~point_area + ELEV2 + BCK21_R + GRA18_10 + FTY18_10 - 1")
  # 
  # if (length(levels(as.factor(s$NUTS2_24))) > 1) {
  #   popfill <- pop.template(data=des, calmodel = model1)
  #   
  #   # Fill area totals by NUTS2_24
  #   a <- aggregate(m$point_area,by=list(m$NUTS2_24),FUN=sum)
  #   colnames(a) <- c("NUTS2_24","Total")
  #   row.names(a) <- paste0("point_area:NUTS2_24",a$NUTS2_24)
  #   popfill[1,grep("NUTS2_24",names(popfill))] <- a$Total[row.names(a) %in% names(popfill)]
  #   
  #   # Fill area totals by ELEV2 
  #   a <- aggregate(m$point_area,by=list(m$ELEV2),FUN=sum)
  #   colnames(a) <- c("ELEV2","Total")
  #   row.names(a) <- paste0("point_area:ELEV2",a$ELEV2)
  #   popfill[1,grep("ELEV",names(popfill))] <- a$Total[row.names(a) %in% names(popfill)]
  #   
  #   # Fill area totals by BCK21_R 
  #   a <- aggregate(m$point_area,by=list(m$BCK21_R),FUN=sum)
  #   colnames(a) <- c("BCK21_R","Total")
  #   row.names(a) <- paste0("point_area:BCK21_R",a$BCK21_R)
  #   popfill[1,grep("BCK21_R",names(popfill))] <- a$Total[row.names(a) %in% names(popfill)]
  #   
  #   # Fill area totals by GRA18_10 
  #   a <- aggregate(m$point_area,by=list(m$GRA18_10),FUN=sum)
  #   colnames(a) <- c("GRA18_10","Total")
  #   row.names(a) <- paste0("point_area:GRA18_10",a$GRA18_10)
  #   popfill[1,grep("GRA18_10",names(popfill))] <- a$Total[row.names(a) %in% names(popfill)]
  #   
  #   # Fill area totals by FTY18_10 
  #   a <- aggregate(m$point_area,by=list(m$FTY18_10),FUN=sum)
  #   colnames(a) <- c("FTY18_10","Total")
  #   row.names(a) <- paste0("point_area:FTY18_10",a$FTY18_10)
  #   popfill[1,grep("FTY18_10",names(popfill))] <- a$Total[row.names(a) %in% names(popfill)]
  #   cal <- e.calibrate(design=des,
  #                       df.population= popfill,
  #                       calmodel=  model1,
  #                       calfun= 'linear', bounds =   c(0.05,500))
  #   check.cal(cal)
  # }
  # 
  # if (length(levels(as.factor(s$NUTS2_24))) == 1) {
  #   popfill <- pop.template(data=des, calmodel = model2)
  #   
  #   # Fill area total
  #   popfill[1,1] <- areas_country$area2024
  #   
  #   # Fill area totals by ELEV2 
  #   a <- aggregate(m$point_area,by=list(m$ELEV2),FUN=sum)
  #   colnames(a) <- c("ELEV2","Total")
  #   row.names(a) <- paste0("point_area:ELEV2",a$ELEV2)
  #   popfill[1,grep("ELEV",names(popfill))] <- a$Total[row.names(a) %in% names(popfill)]
  #   
  #   # Fill area totals by BCK21_R 
  #   a <- aggregate(m$point_area,by=list(m$BCK21_R),FUN=sum)
  #   colnames(a) <- c("BCK21_R","Total")
  #   row.names(a) <- paste0("point_area:BCK21_R",a$BCK21_R)
  #   popfill[1,grep("BCK21_R",names(popfill))] <- a$Total[row.names(a) %in% names(popfill)]
  #   
  #   # Fill area totals by GRA18_10 
  #   a <- aggregate(m$point_area,by=list(m$GRA18_10),FUN=sum)
  #   colnames(a) <- c("GRA18_10","Total")
  #   row.names(a) <- paste0("point_area:GRA18_10",a$GRA18_10)
  #   popfill[1,grep("GRA18_10",names(popfill))] <- a$Total[row.names(a) %in% names(popfill)]
  #   
  #   # Fill area totals by FTY18_10 
  #   a <- aggregate(m$point_area,by=list(m$FTY18_10),FUN=sum)
  #   colnames(a) <- c("FTY18_10","Total")
  #   row.names(a) <- paste0("point_area:FTY18_10",a$FTY18_10)
  #   popfill[1,grep("FTY18_10",names(popfill))] <- a$Total[row.names(a) %in% names(popfill)]
  #   cal <- e.calibrate(design=des,
  #                       df.population= popfill,
  #                       calmodel=  model2,
  #                       calfun= 'linear', bounds =   c(0.001,500))
  #   check.cal(cal)
  # }
  # UWE(cal)
  # summary(s$WGT_LUCAS)
  # sum(s$WGT_LUCAS*s$point_area)
  # sum(m$ones)
  # sum(m$point_area)
  # cal$prob <- cal$prob/cal$variables$point_area
  # sum(weights(cal))
  # summary(weights(cal))
  # s$cal_wgt <- weights(cal)
  # sum(s$cal_wgt)

  ##########################   
  # write calibrated weights
  ##########################
  # df <- NULL
  # df$POINT_ID <- cal$variables$POINT_ID
  # df$cal_wgt <- weights(cal)
  # df <- as.data.frame(df)
  # filename <- paste(country,"_calibrated_wgts_2022.txt",sep="")
  # write.table(df,file = file.path(direnew2, filename),sep="\t",quote=FALSE,row.names=FALSE,dec=".")
  # filename<- paste(country,"_des.Rdata",sep="")
  # save(des,file=file.path(direnew3, filename))
  # filename<- paste(country,"_cal.Rdata",sep="")
  # save(cal,file=file.path(direnew3, filename))
  ##########################   
  # ESTIMATION
  ##########################   
  est_LC1_LU1 <- svystatTM(cal, ~ area +
                             SURVEY_LC1_1+
                             SURVEY_LU1_1+
                             settlement+
                             settl_pc+
                             fao_class_name+
                             lue+
                             lud,
                           estimator="Total",
                           vartype=c("se","cv"),
                           conf.int= TRUE, 
                           conf.lev= 0.95)
  est_LC1_LU1_NUTS1_24 <- svystatTM(cal, ~ area +
                                      SURVEY_LC1_1+
                                      SURVEY_LU1_1+
                                      settlement+
                                      fao_class_name+
                                      lue+
                                      lud,
                                    by = ~ NUTS1_24,
                                    estimator="Total",
                                    vartype=c("se","cv"),
                                    conf.int= TRUE,
                                    conf.lev= 0.95)
  
  if (length(levels(s$NUTS1_24)) > 1){
    est_LC1_LU1_NUTS1_24_t <- as.data.frame(t(est_LC1_LU1_NUTS1_24[,-1]))
    est_LC1_LU1_NUTS1_24_t$variable <- row.names(est_LC1_LU1_NUTS1_24_t)
    #est_LC1_LU1_NUTS1_24_t$variable[1] <- "variable"
  }
  
  est_LC1_LU1_NUTS2_24 <- svystatTM(cal, ~ area +
                                      SURVEY_LC1_1+
                                      SURVEY_LU1_1+
                                      settlement+
                                      fao_class_name+
                                      lue+
                                      lud,
                                    by = ~ NUTS2_24,
                                    estimator="Total",
                                    vartype=c("se","cv"),
                                    conf.int= TRUE, 
                                    conf.lev= 0.95)
  if (length(levels(s$NUTS2_24)) > 1){
    est_LC1_LU1_NUTS2_24_t <- as.data.frame(t(est_LC1_LU1_NUTS2_24[,-1]))
    est_LC1_LU1_NUTS2_24_t$variable <- row.names(est_LC1_LU1_NUTS2_24_t)
    #est_LC1_LU1_NUTS2_24_t$variable[1] <- "variable"
  }
  
  #---------------------------------------------------------------
  # 2nd PHASE
  #---------------------------------------------------------------
  source("Script/1.estimates_by_country_twoPhases_secondPhase_light.R")
  
  #---------------------------------------------------------------
  # Join outputs and write them
  #---------------------------------------------------------------
  filename <- paste(country,'_est_LC1_LU1_2022.csv',sep="")
  tot1 <- rbind(est_LC1_LU1,est_LC1_LU1_2nd) #ok
  write.svystat(tot1,file = file.path(direnew1, filename),sep=",",dec=".")
  
  # Transposed ----------------------------------------------
  filename <- paste(country,"_est_LC1_LU1_NUTS1_24_2022_t.csv",sep="")
  if (length(levels(s$NUTS1_24)) == 1){
    tot2 = merge(est_LC1_LU1_NUTS1_24,est_LC1_LU1_NUTS1_24_2nd, by="NUTS1_24")
    name=as.character(tot2$NUTS1_24)
    tot2 = tot2 %>% pivot_longer(-NUTS1_24, names_to="variable", values_to = name) %>% select(all_of(name),variable)
    }
  else{
    tot2 <- rbind(est_LC1_LU1_NUTS1_24_t,est_LC1_LU1_NUTS1_24_2nd_t)
    
    #ok for countries with more than one nuts1
  }
  
  write.table(tot2,file = file.path(direnew1, filename),sep=",",dec=".",
              row.names=FALSE,col.names=TRUE,quote=FALSE)
  #-----------------------------------------------------------
  filename <- paste(country,"_est_LC1_LU1_NUTS2_24_2022_t.csv",sep="")
  if (length(levels(s$NUTS2_24)) == 1){
    tot3 = merge(est_LC1_LU1_NUTS2_24,est_LC1_LU1_NUTS2_24_2nd, by="NUTS2_24")
    name=as.character(tot3$NUTS2_24)
    tot3 = tot3 %>% pivot_longer(-NUTS2_24, names_to="variable", values_to = name) %>% select(all_of(name),variable)
  }
  else{
    tot3 <- rbind(est_LC1_LU1_NUTS2_24_t,est_LC1_LU1_NUTS2_24_2nd_t) # ok for countries with more than one nuts2
  }
  write.table(tot3,file = file.path(direnew1, filename),sep=",",dec=".",
              row.names=FALSE,col.names=TRUE,quote=FALSE)
  
}
end_time <- Sys.time() 
execution_time <- end_time - start_time 
print(execution_time)
