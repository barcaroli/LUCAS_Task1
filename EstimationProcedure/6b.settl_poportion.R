###########################################################################
# LUCAS - Produce settl_porportion (2009 / 2022) estimates for each country
###########################################################################
#----------------------------------------------------------
# Input: /6.allyears_estimates/
# Output: /6.allyears_estimates/
#----------------------------------------------------------

load("countries.RData")

# Estimates comparison 
for (i in c(1:27)) {
  country <- countries[i]
  filename <- paste0("./6.allyears_estimates/",country,"_est_all.csv")
  df <- read.csv(filename)
  cat("\nCountry:",country)
  
  df <- df[!df$Variable=="settl_proportion",]
  
  
  if (ncol(df) == 26)  {
    df$Area_2009[df$Variable=="area"] <- round(df$Area_2009[df$Variable=="settlement0"] + df$Area_2009[df$Variable=="settlement1"])
    df$Area_2012[df$Variable=="area"] <- round(df$Area_2012[df$Variable=="settlement0"] + df$Area_2012[df$Variable=="settlement1"])
    df$Area_2015[df$Variable=="area"] <- round(df$Area_2015[df$Variable=="settlement0"] + df$Area_2015[df$Variable=="settlement1"])
    df$Area_2018[df$Variable=="area"] <- round(df$Area_2018[df$Variable=="settlement0"] + df$Area_2018[df$Variable=="settlement1"])
    
    settl_proportion_2009 <- df$Area_2009[df$Variable=="settlement1"] / df$Area_2009[df$Variable=="area"]
    settl_proportion_SE_2009 <- sqrt( (1 / df$Area_2009[df$Variable=="area"]^2 )  * (df$Std_error[df$Variable=="settlement1"])^2 )
    settl_proportion_CV_2009 <- settl_proportion_SE_2009 / settl_proportion_2009
    settl_proportion_CI.l_2009 <- settl_proportion_2009 - 1.96 * settl_proportion_SE_2009
    settl_proportion_CI.u_2009 <- settl_proportion_2009 + 1.96 * settl_proportion_SE_2009
    
    settl_proportion_2012 <- df$Area_2012[df$Variable=="settlement1"] / df$Area_2012[df$Variable=="area"]
    settl_proportion_SE_2012 <- sqrt( (1 / df$Area_2012[df$Variable=="area"]^2 )  * (df$Std_error.1[df$Variable=="settlement1"])^2 )
    settl_proportion_CV_2012 <- settl_proportion_SE_2012 / settl_proportion_2012
    settl_proportion_CI.l_2012 <- settl_proportion_2012 - 1.96 * settl_proportion_SE_2012
    settl_proportion_CI.u_2012 <- settl_proportion_2012 + 1.96 * settl_proportion_SE_2012
    
    settl_proportion_2015 <- df$Area_2015[df$Variable=="settlement1"] / df$Area_2015[df$Variable=="area"]
    settl_proportion_SE_2015 <- sqrt( (1 / df$Area_2015[df$Variable=="area"]^2 )  * (df$Std_error.2[df$Variable=="settlement1"])^2 )
    settl_proportion_CV_2015 <- settl_proportion_SE_2015 / settl_proportion_2015
    settl_proportion_CI.l_2015 <- settl_proportion_2015 - 1.96 * settl_proportion_SE_2015
    settl_proportion_CI.u_2015 <- settl_proportion_2015 + 1.96 * settl_proportion_SE_2015
    
    settl_proportion_2018 <- df$Area_2018[df$Variable=="settlement1"] / df$Area_2018[df$Variable=="area"]
    settl_proportion_SE_2018 <- sqrt( (1 / df$Area_2018[df$Variable=="area"]^2 )  * (df$Std_error.3[df$Variable=="settlement1"])^2 )
    settl_proportion_CV_2018 <- settl_proportion_SE_2018 / settl_proportion_2018
    settl_proportion_CI.l_2018 <- settl_proportion_2018 - 1.96 * settl_proportion_SE_2018
    settl_proportion_CI.u_2018 <- settl_proportion_2018 + 1.96 * settl_proportion_SE_2018
    
    settl_proportion_2022 <- df$Area_2022[df$Variable=="settlement1"] / df$Area_2022[df$Variable=="area"]
    settl_proportion_SE_2022 <- sqrt( (1 / df$Area_2022[df$Variable=="area"]^2 )  * (df$Std_error.4[df$Variable=="settlement1"])^2 )
    settl_proportion_CV_2022 <- settl_proportion_SE_2022 / settl_proportion_2022
    settl_proportion_CI.l_2022 <- settl_proportion_2022 - 1.96 * settl_proportion_SE_2022
    settl_proportion_CI.u_2022 <- settl_proportion_2022 + 1.96 * settl_proportion_SE_2022
    
    rec <- c("settl_proportion",
             settl_proportion_2009,
             settl_proportion_SE_2009,
             settl_proportion_CI.l_2009,
             settl_proportion_CI.u_2009,
             settl_proportion_CV_2009,
             settl_proportion_2012,
             settl_proportion_SE_2012,
             settl_proportion_CI.l_2012,
             settl_proportion_CI.u_2012,
             settl_proportion_CV_2012,
             settl_proportion_2015,
             settl_proportion_SE_2015,
             settl_proportion_CI.l_2015,
             settl_proportion_CI.u_2015,
             settl_proportion_CV_2015,
             settl_proportion_2018,
             settl_proportion_SE_2018,
             settl_proportion_CI.l_2018,
             settl_proportion_CI.u_2018,
             settl_proportion_CV_2018,
             settl_proportion_2022,
             settl_proportion_SE_2022,
             settl_proportion_CI.l_2022,
             settl_proportion_CI.u_2022,
             settl_proportion_CV_2022)
    df <- rbind(df,rec)
    df[, 2:26] <- lapply(df[, 2:26], as.numeric)
    df <- df[order(df$Variable),]
  }
  
  if (ncol(df) == 21)  {
    df$Area_2012[df$Variable=="area"] <- round(df$Area_2012[df$Variable=="settlement0"] + df$Area_2012[df$Variable=="settlement1"])
    df$Area_2015[df$Variable=="area"] <- round(df$Area_2015[df$Variable=="settlement0"] + df$Area_2015[df$Variable=="settlement1"])
    df$Area_2018[df$Variable=="area"] <- round(df$Area_2018[df$Variable=="settlement0"] + df$Area_2018[df$Variable=="settlement1"])
 
    settl_proportion_2012 <- df$Area_2012[df$Variable=="settlement1"] / df$Area_2012[df$Variable=="area"]
    settl_proportion_SE_2012 <- sqrt( (1 / df$Area_2012[df$Variable=="area"]^2 )  * (df$Std_error[df$Variable=="settlement1"])^2 )
    settl_proportion_CV_2012 <- settl_proportion_SE_2012 / settl_proportion_2012
    settl_proportion_CI.l_2012 <- settl_proportion_2012 - 1.96 * settl_proportion_SE_2012
    settl_proportion_CI.u_2012 <- settl_proportion_2012 + 1.96 * settl_proportion_SE_2012
    
    settl_proportion_2015 <- df$Area_2015[df$Variable=="settlement1"] / df$Area_2015[df$Variable=="area"]
    settl_proportion_SE_2015 <- sqrt( (1 / df$Area_2015[df$Variable=="area"]^2 )  * (df$Std_error.1[df$Variable=="settlement1"])^2 )
    settl_proportion_CV_2015 <- settl_proportion_SE_2015 / settl_proportion_2015
    settl_proportion_CI.l_2015 <- settl_proportion_2015 - 1.96 * settl_proportion_SE_2015
    settl_proportion_CI.u_2015 <- settl_proportion_2015 + 1.96 * settl_proportion_SE_2015
    
    settl_proportion_2018 <- df$Area_2018[df$Variable=="settlement1"] / df$Area_2018[df$Variable=="area"]
    settl_proportion_SE_2018 <- sqrt( (1 / df$Area_2018[df$Variable=="area"]^2 )  * (df$Std_error.2[df$Variable=="settlement1"])^2 )
    settl_proportion_CV_2018 <- settl_proportion_SE_2018 / settl_proportion_2018
    settl_proportion_CI.l_2018 <- settl_proportion_2018 - 1.96 * settl_proportion_SE_2018
    settl_proportion_CI.u_2018 <- settl_proportion_2018 + 1.96 * settl_proportion_SE_2018
    
    settl_proportion_2022 <- df$Area_2022[df$Variable=="settlement1"] / df$Area_2022[df$Variable=="area"]
    settl_proportion_SE_2022 <- sqrt( (1 / df$Area_2022[df$Variable=="area"]^2 )  * (df$Std_error.3[df$Variable=="settlement1"])^2 )
    settl_proportion_CV_2022 <- settl_proportion_SE_2022 / settl_proportion_2022
    settl_proportion_CI.l_2022 <- settl_proportion_2022 - 1.96 * settl_proportion_SE_2022
    settl_proportion_CI.u_2022 <- settl_proportion_2022 + 1.96 * settl_proportion_SE_2022
    
    rec <- c("settl_proportion",
             settl_proportion_2012,
             settl_proportion_SE_2012,
             settl_proportion_CI.l_2012,
             settl_proportion_CI.u_2012,
             settl_proportion_CV_2012,
             settl_proportion_2015,
             settl_proportion_SE_2015,
             settl_proportion_CI.l_2015,
             settl_proportion_CI.u_2015,
             settl_proportion_CV_2015,
             settl_proportion_2018,
             settl_proportion_SE_2018,
             settl_proportion_CI.l_2018,
             settl_proportion_CI.u_2018,
             settl_proportion_CV_2018,
             settl_proportion_2022,
             settl_proportion_SE_2022,
             settl_proportion_CI.l_2022,
             settl_proportion_CI.u_2022,
             settl_proportion_CV_2022)
    df <- rbind(df,rec)
    df[, 2:21] <- lapply(df[, 2:21], as.numeric)
    df <- df[order(df$Variable),]
  }

  if (ncol(df) == 16)  {
    df$Area_2015[df$Variable=="area"] <- round(df$Area_2015[df$Variable=="settlement0"] + df$Area_2015[df$Variable=="settlement1"])
    df$Area_2018[df$Variable=="area"] <- round(df$Area_2018[df$Variable=="settlement0"] + df$Area_2018[df$Variable=="settlement1"])
    
    settl_proportion_2015 <- df$Area_2015[df$Variable=="settlement1"] / df$Area_2015[df$Variable=="area"]
    settl_proportion_SE_2015 <- sqrt( (1 / df$Area_2015[df$Variable=="area"]^2 )  * (df$Std_error[df$Variable=="settlement1"])^2 )
    settl_proportion_CV_2015 <- settl_proportion_SE_2015 / settl_proportion_2015
    settl_proportion_CI.l_2015 <- settl_proportion_2015 - 1.96 * settl_proportion_SE_2015
    settl_proportion_CI.u_2015 <- settl_proportion_2015 + 1.96 * settl_proportion_SE_2015
    
    settl_proportion_2018 <- df$Area_2018[df$Variable=="settlement1"] / df$Area_2018[df$Variable=="area"]
    settl_proportion_SE_2018 <- sqrt( (1 / df$Area_2018[df$Variable=="area"]^2 )  * (df$Std_error.1[df$Variable=="settlement1"])^2 )
    settl_proportion_CV_2018 <- settl_proportion_SE_2018 / settl_proportion_2018
    settl_proportion_CI.l_2018 <- settl_proportion_2018 - 1.96 * settl_proportion_SE_2018
    settl_proportion_CI.u_2018 <- settl_proportion_2018 + 1.96 * settl_proportion_SE_2018
    
    settl_proportion_2022 <- df$Area_2022[df$Variable=="settlement1"] / df$Area_2022[df$Variable=="area"]
    settl_proportion_SE_2022 <- sqrt( (1 / df$Area_2022[df$Variable=="area"]^2 )  * (df$Std_error.2[df$Variable=="settlement1"])^2 )
    settl_proportion_CV_2022 <- settl_proportion_SE_2022 / settl_proportion_2022
    settl_proportion_CI.l_2022 <- settl_proportion_2022 - 1.96 * settl_proportion_SE_2022
    settl_proportion_CI.u_2022 <- settl_proportion_2022 + 1.96 * settl_proportion_SE_2022
    
    rec <- c("settl_proportion",
             settl_proportion_2015,
             settl_proportion_SE_2015,
             settl_proportion_CI.l_2015,
             settl_proportion_CI.u_2015,
             settl_proportion_CV_2015,
             settl_proportion_2018,
             settl_proportion_SE_2018,
             settl_proportion_CI.l_2018,
             settl_proportion_CI.u_2018,
             settl_proportion_CV_2018,
             settl_proportion_2022,
             settl_proportion_SE_2022,
             settl_proportion_CI.l_2022,
             settl_proportion_CI.u_2022,
             settl_proportion_CV_2022)
    df <- rbind(df,rec)
    df[, 2:16] <- lapply(df[, 2:16], as.numeric)
    df <- df[order(df$Variable),]
  }
  
  write.table(df,filename,sep=",",row.names=FALSE,quote=F)
}

