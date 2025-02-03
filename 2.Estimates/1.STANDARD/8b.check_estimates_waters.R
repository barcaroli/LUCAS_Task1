#######################################################################
# LUCAS - Check of estimates variations for Land Cover G and H
#######################################################################
library(xlsx)
# setwd("C:\\Users\\Giulio\\Google Drive\\LUCAS 2025\\Task 1 - ESTIMATES\\1.STANDARD\\anomalies")
setwd("D:/Google Drive/LUCAS 2025/Task 1 - ESTIMATES/1.STANDARD/anomalies")

filename = "waters_anomalies.xlsx"
unlink(filename)

variable = c("SURVEY_LC1_1G","SURVEY_LC1_1H",
             "SURVEY_LC1_2G1","SURVEY_LC1_2G2","SURVEY_LC1_2G5","SURVEY_LC1_2H1",
             "SURVEY_LC1_3G10","SURVEY_LC1_3G11","SURVEY_LC1_3G20","SURVEY_LC1_3G21",
             "SURVEY_LC1_3G50","SURVEY_LC1_3H11","SURVEY_LC1_3H12")
path_data <- "D:/Google Drive/LUCAS 2025/2.DATA/"
# load(paste0(path_data,"Master_con_hrl_e_NUTS24.Rdata"))
load(paste0(path_data,"countries.Rdata"))


for (w in (1:length(variable))) {
  anom <- NULL
  anom$country <- NA
  anom$Variable <- NA
  anom$Area2009 <- NA
  anom$Area2009_CI_l <- NA
  anom$Area2009_CI_u <- NA
  anom$Area2012 <- NA
  anom$Area2012_CI_l <- NA
  anom$Area2012_CI_u <- NA
  anom$Area2015 <- NA
  anom$Area2015_CI_l <- NA
  anom$Area2015_CI_u <- NA
  anom$Area2018 <- NA
  anom$Area2018_CI_l <- NA
  anom$Area2018_CI_u <- NA
  anom$Area2022 <- NA
  anom$Area2022_CI_l <- NA
  anom$Area2022_CI_u <- NA
  anom$signal
  cat("\n *** Variable: ",variable[w],"\n")
  # w <- which(variable == "SURVEY_LC1_2G2")
  i <- which(countries == "CY")
  ncountries <- 0
  k = 0
  for (i in (1:length(countries))) {
    cat("\n Country: ",countries[i],"\n")
    setwd("D:/Google Drive/LUCAS 2025/Task 1 - ESTIMATES/1.STANDARD/allyears_estimates")
    st <- paste("df <- read.csv('",countries[i],"_est_all.csv',dec='.')",sep="")
    eval(parse(text=st))
    if (ncol(df) == 26) {
      cat("\n Country (26): ",countries[i],"\n")
      ncountries <- ncountries + 1
      if (nrow(df[df$Variable==variable[w],]) > 0) {
        if (!is.na(df[df$Variable==variable[w],c("Area_2022")]) & !is.na(df[df$Variable==variable[w],c("Area_2018")]) ) {
          if (df[df$Variable==variable[w],c("Area_2022")] < df[df$Variable==variable[w],c("Area_2018")]  ) {
            k = k+ 1
            sw2022 <- 1
            sw2018 <- 1
            anom$country[k] <- countries[i]
            anom$Variable[k] <- variable[w]
            anom$Area2009[k] <- df[df$Variable==variable[w],c("Area_2009")]
            anom$Area2009_CI_l[k] <- df[df$Variable==variable[w],c("CI_lower")]
            anom$Area2009_CI_u[k] <- df[df$Variable==variable[w],c("CI_upper")]
            # anom$Area2009_SE[k] <- df[df$Variable==variable[w],c("Std_error")],2)
            # anom$Area2009_CV[k] <- df[df$Variable==variable[w],c("CV")],3)
            anom$Area2012[k] <- df[df$Variable==variable[w],c("Area_2012")]
            anom$Area2012_CI_l[k] <- df[df$Variable==variable[w],c("CI_lower.1")]
            anom$Area2012_CI_u[k] <- df[df$Variable==variable[w],c("CI_upper.1")]
            anom$Area2015[k] <- df[df$Variable==variable[w],c("Area_2015")]
            anom$Area2015_CI_l[k] <- df[df$Variable==variable[w],c("CI_lower.2")]
            anom$Area2015_CI_u[k] <- df[df$Variable==variable[w],c("CI_upper.2")]
            # anom$Area2015_SE[k] <- df[df$Variable==variable[w],c("Std_error.2")],2)
            # anom$Area2015_CV[k] <- df[df$Variable==variable[w],c("CV.2")],3)
            anom$Area2018[k] <- df[df$Variable==variable[w],c("Area_2018")]
            anom$Area2018_CI_l[k] <- df[df$Variable==variable[w],c("CI_lower.3")]
            anom$Area2018_CI_u[k] <- df[df$Variable==variable[w],c("CI_upper.3")]
            # anom$Area2018_SE[k] <- df[df$Variable==variable[w],c("Std_error.3")],2)
            # anom$Area2018_CV[k] <- df[df$Variable==variable[w],c("CV.3")],3)
            anom$Area2022[k] <- df[df$Variable==variable[w],c("Area_2022")]
            anom$Area2022_CI_l[k] <- df[df$Variable==variable[w],c("CI_lower.4")]
            anom$Area2022_CI_u[k] <- df[df$Variable==variable[w],c("CI_upper.4")]
            # anom$Area2018_SE[k] <- df[df$Variable==variable[w],c("Std_error.3")],2)
            # anom$Area2018_CV[k] <- df[df$Variable==variable[w],c("CV.3")],3)
            anom$signal[k] <- "Area 2018 > Area 2022"
            if (df[df$Variable==variable[w],c("Area_2018")] > df[df$Variable==variable[w],c("CI_upper.4")]) anom$signal[k] <- "Area 2018 external to 2022 C.I."
            if (df[df$Variable==variable[w],c("CI_lower.3")] > df[df$Variable==variable[w],c("CI_upper.4")]) anom$signal[k] <- "C.I. 2018 non overlapping C.I. 2022"
          }
        }
      #   if (!is.na(df[df$Variable==variable[w],c("Area_2015")]) & !is.na(df[df$Variable==variable[w],c("Area_2012")]) ) {
      #     if (df[df$Variable==variable[w],c("Area_2015")] < df[df$Variable==variable[w],c("Area_2012")]  ) {
      #       k = k+ 1
      #       sw2015 <- 1
      #       anom$country[k] <- countries[i]
      #       anom$Variable[k] <- variable[w]
      #       anom$Area2009[k] <- df[df$Variable==variable[w],c("Area_2009")]
      #       anom$Area2009_CI_l[k] <- df[df$Variable==variable[w],c("CI_lower")]
      #       anom$Area2009_CI_u[k] <- df[df$Variable==variable[w],c("CI_upper")]
      #       # anom$Area2009_SE[k] <- df[df$Variable==variable[w],c("Std_error")],2)
      #       # anom$Area2009_CV[k] <- df[df$Variable==variable[w],c("CV")],3)
      #       anom$Area2012[k] <- df[df$Variable==variable[w],c("Area_2012")]
      #       anom$Area2012_CI_l[k] <- df[df$Variable==variable[w],c("CI_lower.1")]
      #       anom$Area2012_CI_u[k] <- df[df$Variable==variable[w],c("CI_upper.1")]
      #       # anom$Area2012_SE[k] <- df[df$Variable==variable[w],c("Std_error.1")],2)
      #       # anom$Area2012_CV[k] <- df[df$Variable==variable[w],c("CV.1")],3)
      #       anom$Area2015[k] <- df[df$Variable==variable[w],c("Area_2015")]
      #       anom$Area2015_CI_l[k] <- df[df$Variable==variable[w],c("CI_lower.2")]
      #       anom$Area2015_CI_u[k] <- df[df$Variable==variable[w],c("CI_upper.2")]
      #       # anom$Area2015_SE[k] <- df[df$Variable==variable[w],c("Std_error.2")],2)
      #       # anom$Area2015_CV[k] <- df[df$Variable==variable[w],c("CV.2")],3)
      #       anom$signal[k] <- "Area 2012 > Area 2015"
      #       if (df[df$Variable==variable[w],c("Area_2012")] > df[df$Variable==variable[w],c("CI_upper.2")]) anom$signal[k] <- "Area 2012 external to 2015 C.I."
      #       if (df[df$Variable==variable[w],c("CI_lower.1")] > df[df$Variable==variable[w],c("CI_upper.2")]) anom$signal[k] <- "C.I. 2012 non overlapping C.I. 2015"
      #       anom$Area2018[k] <- df[df$Variable==variable[w],c("Area_2018")]
      #       anom$Area2018_CI_l[k] <- df[df$Variable==variable[w],c("CI_lower.3")]
      #       anom$Area2018_CI_u[k] <- df[df$Variable==variable[w],c("CI_upper.3")]
      #       # anom$Area2018_SE[k] <- df[df$Variable==variable[w],c("Std_error.2")],2)
      #       # anom$Area2018_CV[k] <- df[df$Variable==variable[w],c("CV.2")],3)
      #     }
      #   }
      #   if (!is.na(df[df$Variable==variable[w],c("Area_2012")]) & !is.na(df[df$Variable==variable[w],c("Area_2009")]) ) {
      #     if (df[df$Variable==variable[w],c("Area_2012")] < df[df$Variable==variable[w],c("Area_2009")]  ) {
      #       k = k+ 1
      #       sw2012 <- 1
      #       anom$country[k] <- countries[i]
      #       anom$Variable[k] <- variable[w]
      #       anom$Area2009[k] <- df[df$Variable==variable[w],c("Area_2009")]
      #       anom$Area2009_CI_l[k] <- df[df$Variable==variable[w],c("CI_lower")]
      #       anom$Area2009_CI_u[k] <- df[df$Variable==variable[w],c("CI_upper")]
      #       # anom$Area2009_SE[k] <- df[df$Variable==variable[w],c("Std_error")],2)
      #       # anom$Area2009_CV[k] <- df[df$Variable==variable[w],c("CV")],3)
      #       anom$Area2012[k] <- df[df$Variable==variable[w],c("Area_2012")]
      #       anom$Area2012_CI_l[k] <- df[df$Variable==variable[w],c("CI_lower.1")]
      #       anom$Area2012_CI_u[k] <- df[df$Variable==variable[w],c("CI_upper.1")]
      #       # anom$Area2012_SE[k] <- df[df$Variable==variable[w],c("Std_error.1")],2)
      #       # anom$Area2012_CV[k] <- df[df$Variable==variable[w],c("CV.1")],3)
      #       anom$signal[k] <- "Area 2009 > Area 2012"
      #       if (df[df$Variable==variable[w],c("Area_2009")] > df[df$Variable==variable[w],c("CI_upper.1")]) anom$signal[k] <- "Area 2009 external to 2012 C.I."
      #       if (df[df$Variable==variable[w],c("CI_lower")] > df[df$Variable==variable[w],c("CI_upper.1")]) anom$signal[k] <- "C.I. 2009 non overlapping C.I. 2012"
      #       anom$Area2015[k] <- df[df$Variable==variable[w],c("Area_2015")]
      #       anom$Area2015_CI_l[k] <- df[df$Variable==variable[w],c("CI_lower.2")]
      #       anom$Area2015_CI_u[k] <- df[df$Variable==variable[w],c("CI_upper.2")]
      #       # anom$Area2015_SE[k] <- df[df$Variable==variable[w],c("Std_error.1")],2)
      #       # anom$Area2015_CV[k] <- df[df$Variable==variable[w],c("CV.1")],3)
      #       anom$Area2018[k] <- df[df$Variable==variable[w],c("Area_2018")]
      #       anom$Area2018_CI_l[k] <- df[df$Variable==variable[w],c("CI_lower.3")]
      #       anom$Area2018_CI_u[k] <- df[df$Variable==variable[w],c("CI_upper.3")]
      #       # anom$Area2018_SE[k] <- df[df$Variable==variable[w],c("Std_error.2")],2)
      #       # anom$Area2018_CV[k] <- df[df$Variable==variable[w],c("CV.2")],3)
      #       
      #     }
      #   }
      }
    }
    if (ncol(df) == 21) {
      ncountries <- ncountries + 1
      cat("\n Country (21): ",countries[i],"\n")
      if (nrow(df[df$Variable==variable[w],]) > 0) {
        if (!is.na(df[df$Variable==variable[w],c("Area_2022")]) & !is.na(df[df$Variable==variable[w],c("Area_2018")]) ) {
          if (df[df$Variable==variable[w],c("Area_2022")] < df[df$Variable==variable[w],c("Area_2018")]  ) {
            k = k+ 1
            sw2022 <- 1
            sw2018 <- 1
            anom$country[k] <- countries[i]
            anom$Variable[k] <- variable[w]
            anom$Area2009[k] <- NA
            anom$Area2009_CI_l[k] <- NA
            anom$Area2009_CI_u[k] <- NA
            # anom$Area2009_SE[k] <- df[df$Variable==variable[w],c("Std_error")],2)
            # anom$Area2009_CV[k] <- df[df$Variable==variable[w],c("CV")],3)
            anom$Area2012[k] <- df[df$Variable==variable[w],c("Area_2012")]
            anom$Area2012_CI_l[k] <- df[df$Variable==variable[w],c("CI_lower")]
            anom$Area2012_CI_u[k] <- df[df$Variable==variable[w],c("CI_upper")]
            anom$Area2015[k] <- df[df$Variable==variable[w],c("Area_2015")]
            anom$Area2015_CI_l[k] <- df[df$Variable==variable[w],c("CI_lower.1")]
            anom$Area2015_CI_u[k] <- df[df$Variable==variable[w],c("CI_upper.1")]
            # anom$Area2015_SE[k] <- df[df$Variable==variable[w],c("Std_error.2")],2)
            # anom$Area2015_CV[k] <- df[df$Variable==variable[w],c("CV.2")],3)
            anom$Area2018[k] <- df[df$Variable==variable[w],c("Area_2018")]
            anom$Area2018_CI_l[k] <- df[df$Variable==variable[w],c("CI_lower.2")]
            anom$Area2018_CI_u[k] <- df[df$Variable==variable[w],c("CI_upper.3")]
            # anom$Area2018_SE[k] <- df[df$Variable==variable[w],c("Std_error.3")],2)
            # anom$Area2018_CV[k] <- df[df$Variable==variable[w],c("CV.3")],3)
            anom$Area2022[k] <- df[df$Variable==variable[w],c("Area_2022")]
            anom$Area2022_CI_l[k] <- df[df$Variable==variable[w],c("CI_lower.3")]
            anom$Area2022_CI_u[k] <- df[df$Variable==variable[w],c("CI_upper.3")]
            # anom$Area2018_SE[k] <- df[df$Variable==variable[w],c("Std_error.3")],2)
            # anom$Area2018_CV[k] <- df[df$Variable==variable[w],c("CV.3")],3)
            anom$signal[k] <- "Area 2018 > Area 2022"
            if (df[df$Variable==variable[w],c("Area_2018")] > df[df$Variable==variable[w],c("CI_upper.3")]) anom$signal[k] <- "Area 2018 external to 2022 C.I."
            if (df[df$Variable==variable[w],c("CI_lower.2")] > df[df$Variable==variable[w],c("CI_upper.3")]) anom$signal[k] <- "C.I. 2018 non overlapping C.I. 2022"
          }
        }
        # if (!is.na(df[df$Variable==variable[w],c("Area_2015")]) & !is.na(df[df$Variable==variable[w],c("Area_2012")]) ) {
        #   if (df[df$Variable==variable[w],c("Area_2015")] < df[df$Variable==variable[w],c("Area_2012")]  ) {
        #     k = k+ 1
        #     sw2015 <- 1
        #     anom$country[k] <- countries[i]
        #     anom$Variable[k] <- variable[w]
        #     anom$Area2009[k] <- df[df$Variable==variable[w],c("Area_2009")]
        #     anom$Area2009_CI_l[k] <- df[df$Variable==variable[w],c("CI_lower")]
        #     anom$Area2009_CI_u[k] <- df[df$Variable==variable[w],c("CI_upper")]
        #     # anom$Area2009_SE[k] <- df[df$Variable==variable[w],c("Std_error")],2)
        #     # anom$Area2009_CV[k] <- df[df$Variable==variable[w],c("CV")],3)
        #     anom$Area2012[k] <- df[df$Variable==variable[w],c("Area_2012")]
        #     anom$Area2012_CI_l[k] <- df[df$Variable==variable[w],c("CI_lower.1")]
        #     anom$Area2012_CI_u[k] <- df[df$Variable==variable[w],c("CI_upper.1")]
        #     # anom$Area2012_SE[k] <- df[df$Variable==variable[w],c("Std_error.1")],2)
        #     # anom$Area2012_CV[k] <- df[df$Variable==variable[w],c("CV.1")],3)
        #     anom$Area2015[k] <- df[df$Variable==variable[w],c("Area_2015")]
        #     anom$Area2015_CI_l[k] <- df[df$Variable==variable[w],c("CI_lower.2")]
        #     anom$Area2015_CI_u[k] <- df[df$Variable==variable[w],c("CI_upper.2")]
        #     # anom$Area2015_SE[k] <- df[df$Variable==variable[w],c("Std_error.2")],2)
        #     # anom$Area2015_CV[k] <- df[df$Variable==variable[w],c("CV.2")],3)
        #     anom$signal[k] <- "Area 2012 > Area 2015"
        #     if (df[df$Variable==variable[w],c("Area_2012")] > df[df$Variable==variable[w],c("CI_upper.2")]) anom$signal[k] <- "Area 2012 external to 2015 C.I."
        #     if (df[df$Variable==variable[w],c("CI_lower.1")] > df[df$Variable==variable[w],c("CI_upper.2")]) anom$signal[k] <- "C.I. 2012 non overlapping C.I. 2015"
        #     anom$Area2018[k] <- df[df$Variable==variable[w],c("Area_2018")]
        #     anom$Area2018_CI_l[k] <- df[df$Variable==variable[w],c("CI_lower.3")]
        #     anom$Area2018_CI_u[k] <- df[df$Variable==variable[w],c("CI_upper.3")]
        #     # anom$Area2018_SE[k] <- df[df$Variable==variable[w],c("Std_error.2")],2)
        #     # anom$Area2018_CV[k] <- df[df$Variable==variable[w],c("CV.2")],3)
        #   }
        # }
        # if (!is.na(df[df$Variable==variable[w],c("Area_2012")]) & !is.na(df[df$Variable==variable[w],c("Area_2009")]) ) {
        #   if (df[df$Variable==variable[w],c("Area_2012")] < df[df$Variable==variable[w],c("Area_2009")]  ) {
        #     k = k+ 1
        #     sw2012 <- 1
        #     anom$country[k] <- countries[i]
        #     anom$Variable[k] <- variable[w]
        #     anom$Area2009[k] <- df[df$Variable==variable[w],c("Area_2009")]
        #     anom$Area2009_CI_l[k] <- df[df$Variable==variable[w],c("CI_lower")]
        #     anom$Area2009_CI_u[k] <- df[df$Variable==variable[w],c("CI_upper")]
        #     # anom$Area2009_SE[k] <- df[df$Variable==variable[w],c("Std_error")],2)
        #     # anom$Area2009_CV[k] <- df[df$Variable==variable[w],c("CV")],3)
        #     anom$Area2012[k] <- df[df$Variable==variable[w],c("Area_2012")]
        #     anom$Area2012_CI_l[k] <- df[df$Variable==variable[w],c("CI_lower.1")]
        #     anom$Area2012_CI_u[k] <- df[df$Variable==variable[w],c("CI_upper.1")]
        #     # anom$Area2012_SE[k] <- df[df$Variable==variable[w],c("Std_error.1")],2)
        #     # anom$Area2012_CV[k] <- df[df$Variable==variable[w],c("CV.1")],3)
        #     anom$signal[k] <- "Area 2009 > Area 2012"
        #     if (df[df$Variable==variable[w],c("Area_2009")] > df[df$Variable==variable[w],c("CI_upper.1")]) anom$signal[k] <- "Area 2009 external to 2012 C.I."
        #     if (df[df$Variable==variable[w],c("CI_lower")] > df[df$Variable==variable[w],c("CI_upper.1")]) anom$signal[k] <- "C.I. 2009 non overlapping C.I. 2012"
        #     anom$Area2015[k] <- df[df$Variable==variable[w],c("Area_2015")]
        #     anom$Area2015_CI_l[k] <- df[df$Variable==variable[w],c("CI_lower.2")]
        #     anom$Area2015_CI_u[k] <- df[df$Variable==variable[w],c("CI_upper.2")]
        #     # anom$Area2015_SE[k] <- df[df$Variable==variable[w],c("Std_error.1")],2)
        #     # anom$Area2015_CV[k] <- df[df$Variable==variable[w],c("CV.1")],3)
        #     anom$Area2018[k] <- df[df$Variable==variable[w],c("Area_2018")]
        #     anom$Area2018_CI_l[k] <- df[df$Variable==variable[w],c("CI_lower.3")]
        #     anom$Area2018_CI_u[k] <- df[df$Variable==variable[w],c("CI_upper.3")]
        #     # anom$Area2018_SE[k] <- df[df$Variable==variable[w],c("Std_error.2")],2)
        #     # anom$Area2018_CV[k] <- df[df$Variable==variable[w],c("CV.2")],3)
        # 
        #   }
        # }
      }
    }
    if (ncol(df) == 16) {
      ncountries <- ncountries + 1
      cat("\n Country (16): ",countries[i],"\n")
      if (nrow(df[df$Variable==variable[w],]) > 0) {
        if (!is.na(df[df$Variable==variable[w],c("Area_2022")]) & !is.na(df[df$Variable==variable[w],c("Area_2018")]) ) {
          if (df[df$Variable==variable[w],c("Area_2022")] < df[df$Variable==variable[w],c("Area_2018")]  ) {
            k = k+ 1
            anom$country[k] <- countries[i]
            anom$Variable[k] <- variable[w]
            anom$Area2009[k] <- NA
            anom$Area2009_CI_l[k] <- NA
            anom$Area2009_CI_u[k] <- NA
            anom$Area2012[k] <- NA
            anom$Area2012_CI_l[k] <- NA
            anom$Area2012_CI_u[k] <- NA
            anom$Area2015[k] <- df[df$Variable==variable[w],c("Area_2015")]
            anom$Area2015_CI_l[k] <- df[df$Variable==variable[w],c("CI_lower")]
            anom$Area2015_CI_u[k] <- df[df$Variable==variable[w],c("CI_upper")]
            # anom$Area2015_SE[k] <- df[df$Variable==variable[w],c("Std_error.1")],2)
            # anom$Area2015_CV[k] <- df[df$Variable==variable[w],c("CV.1")],3)
            anom$Area2018[k] <- df[df$Variable==variable[w],c("Area_2018")]
            anom$Area2018_CI_l[k] <- df[df$Variable==variable[w],c("CI_lower.1")]
            anom$Area2018_CI_u[k] <- df[df$Variable==variable[w],c("CI_upper.1")]
            # anom$Area2018_SE[k] <- df[df$Variable==variable[w],c("Std_error.2")],2)
            # anom$Area2018_CV[k] <- df[df$Variable==variable[w],c("CV.2")],3)
            anom$Area2022[k] <- df[df$Variable==variable[w],c("Area_2022")]
            anom$Area2022_CI_l[k] <- df[df$Variable==variable[w],c("CI_lower.2")]
            anom$Area2022_CI_u[k] <- df[df$Variable==variable[w],c("CI_upper.2")]
            # anom$Area2018_SE[k] <- df[df$Variable==variable[w],c("Std_error.3")],2)
            # anom$Area2018_CV[k] <- df[df$Variable==variable[w],c("CV.3")],3)
            anom$signal[k] <- "Area 2018 > Area 2022"
            if (df[df$Variable==variable[w],c("Area_2018")] > df[df$Variable==variable[w],c("CI_upper.2")]) anom$signal[k] <- "Area 2018 external to 2022 C.I."
            if (df[df$Variable==variable[w],c("CI_lower.1")] > df[df$Variable==variable[w],c("CI_upper.2")]) anom$signal[k] <- "C.I. 2018 non overlapping C.I. 2022"
          }
        }
        # if (!is.na(df[df$Variable==variable[w],c("Area_2015")]) & !is.na(df[df$Variable==variable[w],c("Area_2012")]) ) {
        #   if (df[df$Variable==variable[w],c("Area_2015")] < df[df$Variable==variable[w],c("Area_2012")]  ) {
        #     k = k+ 1
        #     anom$country[k] <- countries[i]
        #     anom$Variable[k] <- variable[w]
        #     anom$Area2009[k] <- NA
        #     anom$Area2009_CI_l[k] <- NA
        #     anom$Area2009_CI_u[k] <- NA
        #     anom$Area2012[k] <- df[df$Variable==variable[w],c("Area_2012")]
        #     anom$Area2012_CI_l[k] <- df[df$Variable==variable[w],c("CI_lower")]
        #     anom$Area2012_CI_u[k] <- df[df$Variable==variable[w],c("CI_upper")]
        #     # anom$Area2012_SE[k] <- df[df$Variable==variable[w],c("Std_error")],2)
        #     # anom$Area2012_CV[k] <- df[df$Variable==variable[w],c("CV")],3)
        #     anom$Area2015[k] <- df[df$Variable==variable[w],c("Area_2015")]
        #     anom$Area2015_CI_l[k] <- df[df$Variable==variable[w],c("CI_lower.1")]
        #     anom$Area2015_CI_u[k] <- df[df$Variable==variable[w],c("CI_upper.1")]
        #     # anom$Area2015_SE[k] <- df[df$Variable==variable[w],c("Std_error.1")],2)
        #     # anom$Area2015_CV[k] <- df[df$Variable==variable[w],c("CV.1")],3)
        #     anom$Area2018[k] <- df[df$Variable==variable[w],c("Area_2018")]
        #     anom$Area2018_CI_l[k] <- df[df$Variable==variable[w],c("CI_lower.2")]
        #     anom$Area2018_CI_u[k] <- df[df$Variable==variable[w],c("CI_upper.2")]
        #     anom$signal[k] <- "Area 2012 > Area 2015"
        #     if (df[df$Variable==variable[w],c("Area_2012")] > df[df$Variable==variable[w],c("CI_upper.1")]) anom$signal[k] <- "Area 2012 external to 2015 C.I."
        #     if (df[df$Variable==variable[w],c("CI_lower")] > df[df$Variable==variable[w],c("CI_upper.1")]) anom$signal[k] <- "C.I. 2012 non overlapping C.I. 2015"
        #   }
        # }
      }
    }
  #   if (ncol(df) == 11) {
  #     if (nrow(df[df$Variable==variable[w],]) > 0) {
  #       if (!is.na(df[df$Variable==variable[w],c("Area_2018")]) & !is.na(df[df$Variable==variable[w],c("Area_2015")]) ) {
  #         if (df[df$Variable==variable[w],c("Area_2018")] < df[df$Variable==variable[w],c("Area_2015")]  ) {
  #           k = k+ 1
  #           anom$country[k] <- countries[i]
  #           anom$Variable[k] <- variable[w]
  #           anom$Area2015[k] <- df[df$Variable==variable[w],c("Area_2015")]
  #           anom$Area2015_CI_l[k] <- df[df$Variable==variable[w],c("CI_lower")]
  #           anom$Area2015_CI_u[k] <- df[df$Variable==variable[w],c("CI_upper")]
  #           # anom$Area2015_SE[k] <- df[df$Variable==variable[w],c("Std_error")],2)
  #           # anom$Area2015_CV[k] <- df[df$Variable==variable[w],c("CV")],3)
  #           anom$Area2018[k] <- df[df$Variable==variable[w],c("Area_2018")]
  #           anom$Area2018_CI_l[k] <- df[df$Variable==variable[w],c("CI_lower.1")]
  #           anom$Area2018_CI_u[k] <- df[df$Variable==variable[w],c("CI_upper.1")]
  #           # anom$Area2018_SE[k] <- df[df$Variable==variable[w],c("Std_error.1")],2)
  #           # anom$Area2018_CV[k] <- df[df$Variable==variable[w],c("CV.1")],3)
  #           anom$signal[k] <- "Area 2015 > Area 2018"
  #           if (df[df$Variable==variable[w],c("Area_2015")] > df[df$Variable==variable[w],c("CI_upper.1")]) anom$signal[k] <- "Area 2015 external to 2018 C.I."
  #           if (df[df$Variable==variable[w],c("CI_lower")] > df[df$Variable==variable[w],c("CI_upper.1")]) anom$signal[k] <- "C.I. 2015 non overlapping C.I. 2018"
  #         }
  #       }
  #     }
  }
  setwd("D:/Google Drive/LUCAS 2025/Task 1 - ESTIMATES/1.STANDARD/anomalies")
  anom <- as.data.frame(anom)
  if (!(nrow(anom) == 1 && is.na(anom$country))) {
    # anom <- anom[,c(1,3,6,9,12,15,18)]
    write.xlsx(anom, filename, sheetName = variable[w], 
               col.names = TRUE, row.names = FALSE, append = TRUE)
  }
}


