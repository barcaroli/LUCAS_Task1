#---------------------------------
# Script to plot LUCAS
# EU 22 countries LC and LU estimates
# 009 - 2012 - 2015 - 2018 -2022
#---------------------------------
plotCI <- function(df,
                   y,
                   ci_l,
                   ci_u,
                   variable,
                   domain=NULL,
                   ylim=NULL,
                   specs=NULL) {
  eval(parse(text=paste0("y_vals <- df$",y)))
  eval(parse(text=paste0("ci_l <- df$",ci_l)))
  eval(parse(text=paste0("ci_u <- df$",ci_u)))
  plot(y_vals,
       ylim = c(min(ci_l),max(ci_u)),
       type="b",
       ylab = "Estimate",
       xlab = variable,
       xaxt = 'n',
       col="blue")
  title(paste0(y," by ",variable," - ",domain," ",specs),cex.main = 1,font.main= 1,col.main="blue")
  axis(side = 1, at = c(1:nrow(df)), 
       labels = c("2009","2012","2015","2018","2022"), las=1, cex.axis=0.7)
  x_points <- 1:nrow(df)
  polygon(
    x = c(x_points,rev(x_points)),
    y = c(ci_l[1:nrow(df)],rev(ci_u[1:nrow(df)])), 
    col = rgb(0, 0, 1, 0.2), border = NA)
}


library(xlsx)
setwd("D:/Google Drive/LUCAS 2025/Task 1 - ESTIMATES/2.TWO PHASES light/EU_estimates")
# eu <- read.xlsx("Europe_estimates.xlsx",sheetIndex = 1,colClasses = rep("numeric",5))
eu <- read.xlsx("Europe_estimates.xlsx",sheetIndex = 1)

#-----------------------------
# FAO, settlement, lue, lud
#-----------------------------

variables <- c("fao_class_name0",
               "fao_class_name1",
               "fao_class_name2",
               "fao_class_name3",
               "lud0",
               "lud1",
               "lue0",
               "lue1",
               "settl_pc",
               "settlement0",
               "settlement1")
eu1 <- eu[eu$Variable %in% variables,]

years <- c("2009","2012","2015","2018","2022")

par(mfrow=c(2,2))
for (i in (1:nrow(eu1))) {
  var <- eu1$Variable[i]
  df <- NULL
  df$Year <- years
  df$var <- c(eu1$Area2009[i],eu1$Area2012[i],eu1$Area2015[i],eu1$Area2018[i],eu1$Area2022[i])
  df$ci_l <- c(eu1$CI.l_2009[i],eu1$CI.l_2012[i],eu1$CI.l_2015[i],eu1$CI.l_2018[i],eu1$CI.l_2022[i])
  df$ci_u <- c(eu1$CI.u_2009[i],eu1$CI.u_2012[i],eu1$CI.u_2015[i],eu1$CI.u_2018[i],eu1$CI.u_2022[i])
  df <- as.data.frame(df)
  colnames(df)[2] <- var
  df
  plotCI(df=df,
         y=var,
         ci_l="ci_l",
         ci_u="ci_u",
         variable="Year",
         domain=NULL,
         ylim=NULL,
         specs=NULL)
}
par(mfrow=c(1,1)) 

#-----------------------------
# LC 1 digit
#-----------------------------

variables <- grep("LC1_1",eu$Variable)
eu1 <- eu[variables,]

par(mfrow=c(2,2))
for (i in (1:nrow(eu1))) {
  var <- eu1$Variable[i]
  df <- NULL
  df$Year <- years
  df$var <- c(eu1$Area2009[i],eu1$Area2012[i],eu1$Area2015[i],eu1$Area2018[i],eu1$Area2022[i])
  df$ci_l <- c(eu1$CI.l_2009[i],eu1$CI.l_2012[i],eu1$CI.l_2015[i],eu1$CI.l_2018[i],eu1$CI.l_2022[i])
  df$ci_u <- c(eu1$CI.u_2009[i],eu1$CI.u_2012[i],eu1$CI.u_2015[i],eu1$CI.u_2018[i],eu1$CI.u_2022[i])
  df <- as.data.frame(df)
  colnames(df)[2] <- var
  df
  plotCI(df=df,
         y=var,
         ci_l="ci_l",
         ci_u="ci_u",
         variable="Year",
         domain=NULL,
         ylim=NULL,
         specs=NULL)
}
par(mfrow=c(1,1))  

#-----------------------------
# LU 1 digit
#-----------------------------

variables <- grep("LU1_1",eu$Variable)
eu1 <- eu[variables,]

par(mfrow=c(2,2))
for (i in (1:nrow(eu1))) {
  var <- eu1$Variable[i]
  df <- NULL
  df$Year <- years
  df$var <- c(eu1$Area2009[i],eu1$Area2012[i],eu1$Area2015[i],eu1$Area2018[i],eu1$Area2022[i])
  df$ci_l <- c(eu1$CI.l_2009[i],eu1$CI.l_2012[i],eu1$CI.l_2015[i],eu1$CI.l_2018[i],eu1$CI.l_2022[i])
  df$ci_u <- c(eu1$CI.u_2009[i],eu1$CI.u_2012[i],eu1$CI.u_2015[i],eu1$CI.u_2018[i],eu1$CI.u_2022[i])
  df <- as.data.frame(df)
  colnames(df)[2] <- var
  df
  plotCI(df=df,
         y=var,
         ci_l="ci_l",
         ci_u="ci_u",
         variable="Year",
         domain=NULL,
         ylim=NULL,
         specs=NULL)
}
par(mfrow=c(1,1))  

#-----------------------------
# LC 2 digits
#-----------------------------

variables <- grep("LC1_2",eu$Variable)
eu1 <- eu[variables,]
eu1 <- eu1[complete.cases(eu1),]

par(mfrow=c(2,2))
for (i in (1:nrow(eu1))) {
  var <- eu1$Variable[i]
  df <- NULL
  df$Year <- years
  df$var <- c(eu1$Area2009[i],eu1$Area2012[i],eu1$Area2015[i],eu1$Area2018[i],eu1$Area2022[i])
  df$ci_l <- c(eu1$CI.l_2009[i],eu1$CI.l_2012[i],eu1$CI.l_2015[i],eu1$CI.l_2018[i],eu1$CI.l_2022[i])
  df$ci_u <- c(eu1$CI.u_2009[i],eu1$CI.u_2012[i],eu1$CI.u_2015[i],eu1$CI.u_2018[i],eu1$CI.u_2022[i])
  df <- as.data.frame(df)
  colnames(df)[2] <- var
  df
  plotCI(df=df,
         y=var,
         ci_l="ci_l",
         ci_u="ci_u",
         variable="Year",
         domain=NULL,
         ylim=NULL,
         specs=NULL)
}
par(mfrow=c(1,1))  

#-----------------------------
# LU 2 digits
#-----------------------------

variables <- grep("LU1_2",eu$Variable)
eu1 <- eu[variables,]
eu1 <- eu1[complete.cases(eu1),]

par(mfrow=c(2,2))
for (i in (1:nrow(eu1))) {
  var <- eu1$Variable[i]
  df <- NULL
  df$Year <- years
  df$var <- c(eu1$Area2009[i],eu1$Area2012[i],eu1$Area2015[i],eu1$Area2018[i],eu1$Area2022[i])
  df$ci_l <- c(eu1$CI.l_2009[i],eu1$CI.l_2012[i],eu1$CI.l_2015[i],eu1$CI.l_2018[i],eu1$CI.l_2022[i])
  df$ci_u <- c(eu1$CI.u_2009[i],eu1$CI.u_2012[i],eu1$CI.u_2015[i],eu1$CI.u_2018[i],eu1$CI.u_2022[i])
  df <- as.data.frame(df)
  colnames(df)[2] <- var
  df
  plotCI(df=df,
         y=var,
         ci_l="ci_l",
         ci_u="ci_u",
         variable="Year",
         domain=NULL,
         ylim=NULL,
         specs=NULL)
}
par(mfrow=c(1,1)) 

#-----------------------------
# LC 3 digits
#-----------------------------

variables <- grep("LC1_3",eu$Variable)
eu1 <- eu[variables,]
eu1 <- eu1[complete.cases(eu1),]

par(mfrow=c(2,2))
for (i in (1:nrow(eu1))) {
  var <- eu1$Variable[i]
  df <- NULL
  df$Year <- years
  df$var <- c(eu1$Area2009[i],eu1$Area2012[i],eu1$Area2015[i],eu1$Area2018[i],eu1$Area2022[i])
  df$ci_l <- c(eu1$CI.l_2009[i],eu1$CI.l_2012[i],eu1$CI.l_2015[i],eu1$CI.l_2018[i],eu1$CI.l_2022[i])
  df$ci_u <- c(eu1$CI.u_2009[i],eu1$CI.u_2012[i],eu1$CI.u_2015[i],eu1$CI.u_2018[i],eu1$CI.u_2022[i])
  df <- as.data.frame(df)
  colnames(df)[2] <- var
  df
  plotCI(df=df,
         y=var,
         ci_l="ci_l",
         ci_u="ci_u",
         variable="Year",
         domain=NULL,
         ylim=NULL,
         specs=NULL)
}
par(mfrow=c(1,1))  

#-----------------------------
# LU 3 digits
#-----------------------------

variables <- grep("LU1_3",eu$Variable)
eu1 <- eu[variables,]
eu1 <- eu1[complete.cases(eu1),]

par(mfrow=c(2,2))
for (i in (1:nrow(eu1))) {
  var <- eu1$Variable[i]
  df <- NULL
  df$Year <- years
  df$var <- c(eu1$Area2009[i],eu1$Area2012[i],eu1$Area2015[i],eu1$Area2018[i],eu1$Area2022[i])
  df$ci_l <- c(eu1$CI.l_2009[i],eu1$CI.l_2012[i],eu1$CI.l_2015[i],eu1$CI.l_2018[i],eu1$CI.l_2022[i])
  df$ci_u <- c(eu1$CI.u_2009[i],eu1$CI.u_2012[i],eu1$CI.u_2015[i],eu1$CI.u_2018[i],eu1$CI.u_2022[i])
  df <- as.data.frame(df)
  colnames(df)[2] <- var
  df
  plotCI(df=df,
         y=var,
         ci_l="ci_l",
         ci_u="ci_u",
         variable="Year",
         domain=NULL,
         ylim=NULL,
         specs=NULL)
}
par(mfrow=c(1,1)) 
