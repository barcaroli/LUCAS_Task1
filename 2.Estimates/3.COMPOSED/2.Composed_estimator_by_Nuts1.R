#-------------------------------------------
# Production of estimates 
# using the COMPOSED ESTIMATOR
# One-digit by NUTS1
#-------------------------------------------

path_data <- "D:\\Google Drive\\LUCAS 2025\\2.Data\\"
path_out <- "D:/Google Drive/LUCAS 2025/Task 1 - ESTIMATES/3.COMPOSED\\estimates2022\\"
setwd("D:/Google Drive/LUCAS 2025/Task 1 - ESTIMATES/3.COMPOSED")
options(scipen=100)

dire <- getwd()
direnew1 <- paste(dire, "/estimates2022", sep = "")
# if (dir.exists(direnew1))
#   unlink(direnew1,recursive=TRUE)
if (!dir.exists(direnew1))
  dir.create(direnew1)

# path_data <- "C:\\Users\\UTENTE\\Google Drive\\LUCAS 2025\\2.Data\\"
# path_out <- "C:\\Users\\UTENTE/Google Drive/LUCAS 2025/Task 1 - ESTIMATES/3.COMPOSED\\estimates2022\\"
# setwd("C:\\Users\\UTENTE/Google Drive/LUCAS 2025/Task 1 - ESTIMATES/3.COMPOSED")
library(ReGenesees)
library(data.table)

# ---------------
# Read areas
# ---------------
areas <- read.csv(paste0(path_data,"areas_2015_2024.csv"),colClasses = c(rep('character',4), rep('numeric',10)))


# ---------------
#   Master
# ---------------
load(paste0(path_data,"master_complete.Rdata"))

master$ELEV2 <- ifelse(master$ELEV_DEM < 100, 1, 
                       ifelse(master$ELEV_DEM < 200, 2,
                              ifelse(master$ELEV < 500, 3,
                                     ifelse(master$ELEV_DEM < 1000, 4,
                                            ifelse(master$ELEV_DEM < 1500, 5, 6)))))
table(master$ELEV2,useNA="ifany")
master$ELEV2 <- as.factor(master$ELEV2)

# master$CLC18_1d<-factor(substr(master$CLC18_vett,1,1))
master$NUTS0_24 <- factor(master$NUTS0_24)
master$NUTS2_24 <- factor(master$NUTS2_24)
# master$NUTS3_24 <- factor(master$NUTS3_24)
# master$BCK18_R <- factor(master$BCK18_R)
master$BCK21_R <- factor(master$BCK21_R)
master$GRA18_10 <- factor(master$GRA18_10)
master$FTY18_10 <- factor(master$FTY18_10)
master$ones<-1

#-----------
# LUCAS 2018
#-----------
s2018 <- fread(paste0(path_data,"Survey_2018_cal_wgt.txt"))
s2018 <- s2018[s2018$NUTS0_16 != "UK",]

s2018 <- merge(s2018[,c("POINT_ID","cal_wgt","STRATUM","land_cover","land_use")],
               master[,c("POINT_ID","point_area","ELEV2","NUTS0_24","NUTS2_24","BCK21_R","GRA18_10","FTY18_10")],by="POINT_ID")
s2018$LC2018 <- as.factor(substr(s2018$land_cover,1,1))
s2018$LU2018 <- as.factor(substr(s2018$land_use,1,2))

s2018tot <- s2018
table(s2018tot$LC2018,useNA="ifany")
table(s2018tot$LU2018,useNA="ifany")

#-----------
# LUCAS 2022
#-----------
s2022 <- fread(paste0(path_data,"Survey_2022_cal_wgt_standard.txt"))
s2022 <- s2022[!is.na(s2022$SURVEY_LC1) & !is.na(s2022$SURVEY_LU1),]
s2022 <- s2022[s2022$SURVEY_LC1 != "8" & s2022$SURVEY_LU1 != "8",]

s2022$LC2022 <- as.factor(substr(s2022$SURVEY_LC1,1,1))
s2022$LU2022 <- as.factor(substr(s2022$SURVEY_LU1,1,2))

s2022tot <- merge(s2022[,c("POINT_ID","fpc","WGT_LUCAS","cal_wgt","STRATUM_LUCAS","SURVEY_LC1","SURVEY_LU1","LC2022","LU2022")],
                  master[,c("POINT_ID","point_area","NUTS0_24","NUTS1_24","NUTS2_24","ELEV2","BCK21_R","GRA18_10","FTY18_10")],by="POINT_ID")

#-----------
# Panel
#-----------
s2018_panel_tot <- s2018tot[s2018tot$POINT_ID %in% s2022tot$POINT_ID,]
s2022_panel_tot <- s2022tot[s2022tot$POINT_ID %in% s2018tot$POINT_ID,]
panel_data_tot <-  merge(s2018_panel_tot[,c("POINT_ID",
                                            "land_cover",
                                            "land_use",
                                            "LC2018",
                                            "LU2018",
                                            "STRATUM",
                                            "cal_wgt")],
                         s2022_panel_tot[,c("POINT_ID",
                                            "SURVEY_LC1",
                                            "SURVEY_LU1",
                                            "LC2022",
                                            "LU2022",
                                            "STRATUM_LUCAS",
                                            "fpc",
                                            "point_area",
                                            "WGT_LUCAS",
                                            "NUTS0_24",
                                            "NUTS1_24",
                                            "NUTS2_24",
                                            "ELEV2",
                                            "BCK21_R",
                                            "GRA18_10",
                                            "FTY18_10")])

load("countries.RData")
# countries<-countries[-c(7,17,21,27)]
i="MT"
# which(countries %in% c("CY","MT","EE","LU","LV"))
 
for(i in countries){
  cat("\n#-------------")
  cat("\nCountry: ",i)
  cat("\n#-------------")
  cat("\n")
  m <- master[master$NUTS0_24==i,]
  m <- m[!is.na(m$ELEV2),]
  # selezione paese
  s2018<-s2018tot[s2018tot$NUTS0_24==i,]
  s2022<-s2022tot[s2022tot$NUTS0_24==i,]
  panel_data <-panel_data_tot[panel_data_tot$NUTS0_24==i, ]
  panel_data$pesi_panel<-panel_data[,c("WGT_LUCAS")]
  
  ##################################################################
  # ReGenesees on the Panel
  ##################################################################
  panel_data$stratum <- as.factor(panel_data$STRATUM_LUCAS)
  #panel_data$W<-panel_data$pesi_panel/sum(panel_data$pesi_panel)
  panel_data$W<-panel_data$pesi_panel
  panel_data$STRATUM_LUCAS <- as.factor(panel_data$STRATUM_LUCAS)
  panel_data <- droplevels(panel_data)
  panel_data$LC <- as.factor(substr(panel_data$land_cover,1,1))
  levels(panel_data$LC) <- LETTERS[1:8]
  table(panel_data$LC)
  panel_data$LU <- as.factor(substr(panel_data$land_use,1,2))
  levels(panel_data$LU) <- c("U1","U2","U3","U4")
  table(panel_data$LU)
  
  des_panel <- e.svydesign(data=panel_data, 
                           fpc=~fpc, 
                           ids= ~ POINT_ID, 
                           strata= ~ STRATUM_LUCAS, 
                           weights= ~ W, 
                           self.rep.str= NULL, 
                           check.data= TRUE)
  ls <- find.lon.strata(des_panel)
  print(ls)
  if (!is.null(ls)) {
    des_panel <-tryCatch({
      des_panel <- collapse.strata(des_panel, block.vars = ~NUTS2_24)
    },
    error = function(e) {
      cat("Error:", e$message, "\n")
      des_panel <- collapse.strata(des_panel)
      return(des_panel)
    })
  }
  
  #---------------------------
  # calibration
  #---------------------------
  m <- droplevels(m)
  s2022 <- droplevels(s2022)
  #---------------------------
  # Check feasibility calmodel
  #---------------------------
  calib_vars <- NULL
  a <- table(m$ELEV2)
  b <- table(panel_data$ELEV2)
  if (length(a[a>0]) == length(b[b>0]) & length(b[b>0]) > 1) calib_vars <- "ELEV2"
  
  a <- table(m$BCK21_R)
  b <- table(panel_data$BCK21_R)
  if (length(a[a>0]) == length(b[b>0]) & length(b[b>0]) > 1) calib_vars <- c(calib_vars,"BCK21_R")
  
  a <- table(m$GRA18_10)
  b <- table(panel_data$GRA18_10)
  if (length(a[a>0] == length(b[b>0])) & length(b[b>0]) > 1) calib_vars <- c(calib_vars,"GRA18_10")
  
  a <- table(m$FTY18_10)
  b <- table(panel_data$FTY18_10)
  if (length(a[a>0] == length(b[b>0])) & length(b[b>0]) > 1) calib_vars <- c(calib_vars,"FTY18_10")
  
  print(calib_vars)
  
  if (length(calib_vars) > 0) {
    stringa <- paste0(
      "poptemp <- pop.template(data=panel_data, calmodel = ~ point_area + point_area:(",
      paste(calib_vars, collapse = " + "),
      ") - 1)"
    )
    print(stringa)
    eval(parse(text=stringa))
    poptot1 <- fill.template(universe=m, template= poptemp)
    poptot1[1] <- sum(areas$area2024[substr(areas$NUTS2,1,2)==i],na.rm=TRUE)
    stringa <- paste0(
      "cal <- e.calibrate(design=des_panel,
        df.population= poptot1,calmodel = ~ point_area + point_area:(",
      paste(calib_vars, collapse = " + "),
      ") - 1, calfun= 'linear', bounds =   c(0.01,100))"
    )
    print(stringa)
    eval(parse(text=stringa))
  }
  if (length(calib_vars) == 0) {
    poptemp <- pop.template(data=panel_data, calmodel = ~ point_area - 1)
    poptot1 <- fill.template(universe=m, template= poptemp)
    poptot1[1] <- sum(areas$area2024[substr(areas$NUTS2,1,2)==i],na.rm=TRUE)
    cal <- e.calibrate(design=des_panel,
                       df.population= poptot1,
                       calmodel = ~ point_area - 1, 
                       calfun= 'linear', bounds =   c(0.01,Inf))
  }
  check.cal(cal)
  cal$prob <- cal$prob/cal$variables$point_area
  sum(weights(cal))
  sum(m$point_area)
    # Estimation
    a <- table(panel_data$LC)
    b <- table(panel_data$LU)
    Y1r <- svystat(cal, y = ~LC+LU, by = ~NUTS1_24, estimator="Total", conf.int=TRUE)
    Y2r <- svystat(cal, y = ~LC+LU, by = ~NUTS1_24, estimator="Mean", conf.int=TRUE)
    if (nrow(Y2r) < 12) {
      nuts <- levels(as.factor(s2022$NUTS1_24))
      class <- paste0(nuts,":",c("LCA","LCB","LCC","LCD","LCE","LCF","LCG","LCH","LUU1","LUU2","LUU3","LUU4"))
      template <- as.data.frame(list(name = class,dummy = rep(NA,12)))
      Y2 <- merge(template,Y2r,all.x=TRUE)
      Y2$dummy <- NULL
      Y2$Y[is.na(Y2$Y)] <- 0
      Y2$SE[is.na(Y2$SE)] <- 0
      Y2$VAR[is.na(Y2$VAR)] <- 0
      Y2$CV[is.na(Y2$CV)] <- 0
      Y2r <- Y2
    }
    Y2r
    
    ########################################################################
    # ReGenesees on the new observations
    ########################################################################
    new_data <- s2022[!(s2022$POINT_ID %in% panel_data$POINT_ID),]
    new_data$LC <- as.factor(substr(new_data$SURVEY_LC1,1,1))
    levels(new_data$LC) <- LETTERS[1:8]
    table(new_data$LC)
    new_data$LU <- as.factor(substr(new_data$SURVEY_LU1,1,2))
    levels(new_data$LU) <- c("U1","U2","U3","U4")
    table(new_data$LU)
    new_data$stratum <- as.factor(new_data$STRATUM_LUCAS)
    # new_data$LC_pred <- as.factor(new_data$LC_pred)
    #new_data$W<-new_data$WGT_LUCAS/sum(new_data$WGT_LUCAS)
    new_data$W<-new_data$WGT_LUCAS
    # table(new_data$LC_pred,useNA = "ifany")
    
    des_new <- e.svydesign(data=new_data, 
                           fpc=~fpc, 
                           ids= ~ POINT_ID, 
                           strata= ~ stratum, 
                           weights= ~ W, 
                           self.rep.str= NULL, 
                           check.data= TRUE)
    ls <- find.lon.strata(des_new)
    print(ls)
    if (!is.null(ls)) {
      des_new <-tryCatch({
        des_new <- collapse.strata(des_new, block.vars = ~NUTS2_24)
      },
      error = function(e) {
        cat("Error:", e$message, "\n")
        des_new <- collapse.strata(des_new)
        return(des_new)
      })
    }
    
    #---------------------------
    # calibration
    #---------------------------
    new_data <- droplevels(new_data)
    #---------------------------
    # Check feasibility calmodel
    #---------------------------
    calib_vars <- NULL
    a <- table(m$ELEV2)
    a
    b <- table(new_data$ELEV2)
    b
    if (length(a[a>0]) == length(b[b>0]) & length(b[b>0]) > 1) calib_vars <- "ELEV2"
    
    a <- table(m$BCK21_R)
    a
    b <- table(new_data$BCK21_R)
    b
    if (length(a[a>0]) == length(b[b>0]) & length(b[b>0]) > 1) calib_vars <- c(calib_vars,"BCK21_R")
    
    a <- table(m$GRA18_10)
    a
    b <- table(new_data$GRA18_10)
    b
    if (length(a[a>0] == length(b[b>0])) & length(b[b>0]) > 1) calib_vars <- c(calib_vars,"GRA18_10")
    
    a <- table(m$FTY18_10)
    a
    b <- table(new_data$FTY18_10)
    b
    if (length(a[a>0] == length(b[b>0])) & length(b[b>0]) > 1) calib_vars <- c(calib_vars,"FTY18_10")
    
    print(calib_vars)
    
    if (length(calib_vars) > 0) {
      stringa <- paste0(
        "poptemp <- pop.template(data=new_data, calmodel = ~ point_area + point_area:(",
        paste(calib_vars, collapse = " + "),
        ") - 1)"
      )
      print(stringa)
      eval(parse(text=stringa))
      poptot1 <- fill.template(universe=m, template= poptemp)
      poptot1[1] <- sum(areas$area2024[substr(areas$NUTS2,1,2)==i],na.rm=TRUE)
      stringa <- paste0(
        "cal_new <- e.calibrate(design=des_new,df.population= poptot1,
        calmodel = ~ point_area + point_area:(",
        paste(calib_vars, collapse = " + "),
        ") - 1, calfun= 'linear', bounds =   c(0.01,100))"
      )
      print(stringa)
      eval(parse(text=stringa))
    }
    if (length(calib_vars) == 0) {
      poptemp <- pop.template(data=new_data, calmodel = ~ point_area - 1)
      poptot1 <- fill.template(universe=m, template= poptemp)
      poptot1[1] <- sum(areas$area2024[substr(areas$NUTS2,1,2)==i],na.rm=TRUE)
      cal_new <- e.calibrate(design=des_new,
                         df.population= poptot1,
                         calmodel = ~ point_area - 1, 
                         calfun= 'linear', bounds =   c(0.01,100))
    }
    check.cal(cal_new)
    
    a <- table(new_data$LC)
    b <- table(new_data$LU)
    

    if (length(a) == 8 & (length(b) == 4)) {
      y1s <- svystat(cal_new, y = ~LC2022+LU2022, by = ~NUTS1_24, estimator="Total", conf.int=TRUE)
      y2s <- svystat(cal_new, y = ~LC2022+LU2022, by = ~NUTS1_24, estimator="Mean", conf.int=TRUE)
      y2s$fi <- y2s$SE^2 / (Y2r$SE^2 + y2s$SE^2)
      y2s$Y2star = y2s$fi * Y2r$Y + (1 - y2s$fi) * y2s$Y
      y2s$V_Y2star = y2s$fi^2 * (Y2r$SE^2) + (1 - y2s$fi)^2 * (y2s$SE^2)
      y2s$cv <- sqrt(y2s$V_Y2star) / y2s$Y2star
      Y2star <- y2s[,c(1,7,8,9)]
    }     
 

    if (!(length(a) == 8 & (length(b) == 4))) {
      Y2star <- as.data.frame(list(name=Y2r$name,
                                   Y2star=Y2r$Y,
                                   V_Y2star=Y2r$SE,
                                   cv=Y2r$CV))
    }

    nuts <- levels(as.factor(s2022$NUTS1_24))
    variables <- c(paste0("SURVEY_LC1_1",LETTERS[1:8]),paste0("SURVEY_LU1_1",c("U1","U2","U3","U4")))
    area <- sum(areas[substr(areas$NUTS2,1,2)==i,"area2024"],na.rm=TRUE)
    est <- NULL
    estim <- NULL
    for (k in nuts) {
      estim$Variable <- paste0("Total.",variables)
      Total <- Y2star[grep(k,Y2star$name),c("Y2star")] * area
      Total[1:8] <- Total[1:8] * (area / sum(Total[1:8]) )
      Total[9:12] <- Total[9:12] * (area / sum(Total[9:12]) )
      eval(parse(text=paste0("estim$",k," <- Total")))
      estim <- as.data.frame(estim)
    }
    est <- rbind(est,estim)
    estim <- NULL
    for (k in nuts) {
      estim$Variable <- paste0("SE.Total.",variables)
      SE <- sqrt(Y2star[grep(k,Y2star$name),c("V_Y2star")] * nrow(s2022)^2)
      eval(parse(text=paste0("estim$",k," <- SE")))
      estim <- as.data.frame(estim)
    }
    est <- rbind(est,estim)
    estim <- NULL
    for (k in nuts) {
      estim$Variable <- paste0("CI.l(95%).Total.",variables)
      CI_l <- (Y2star[grep(k,Y2star$name),c("Y2star")] * area) - 1.96 * sqrt(Y2star[grep(k,Y2star$name),c("V_Y2star")] * nrow(s2022)^2)
      eval(parse(text=paste0("estim$",k," <- CI_l")))
      estim <- as.data.frame(estim)
    }
    est <- rbind(est,estim)
    estim <- NULL
    for (k in nuts) {
      estim$Variable <- paste0("CI.u(95%).Total.",variables)
      CI_u <- (Y2star[grep(k,Y2star$name),c("Y2star")] * area) + 1.96 * sqrt(Y2star[grep(k,Y2star$name),c("V_Y2star")] * nrow(s2022)^2)
      eval(parse(text=paste0("estim$",k," <- CI_u")))
      estim <- as.data.frame(estim)
    }
    est <- rbind(est,estim)
    estim <- NULL
    for (k in nuts) {
      estim$Variable <- paste0("CV.Total.",variables)
      CV <- sqrt(Y2star[grep(k,Y2star$name),c("V_Y2star")] * nrow(s2022)^2) /  (Y2star[grep(k,Y2star$name),c("Y2star")] * area)
      eval(parse(text=paste0("estim$",k," <- CV")))
      estim <- as.data.frame(estim)
    }
    est <- rbind(est,estim)
    print(est)
    filename <- paste(path_out,i,'_est_LC1_LU1_NUTS1_24_2022.csv',sep="")
    write.table(est,filename,sep=",",dec=".",quote=F,row.names=FALSE)

}





