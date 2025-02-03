#---------------------------------
# Script to plot LUCAS
# EU 27 countries LC and LU estimates
# 2015 - 2018 -2022
# comparing standard and two-phases
# estimation procedures
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
       # ylim = c(min(ci_l),max(ci_u)),
       ylim = ylim,
       type="b",
       ylab = "Estimate",
       xlab = variable,
       xaxt = 'n',
       col="blue")
  title(paste0(y," by ",variable," - ",domain," ",specs),cex.main = 1,font.main= 1,col.main="blue")
  axis(side = 1, at = c(1:nrow(df)), 
       labels = c("2015","2018","2022"), las=1, cex.axis=0.7)
  x_points <- 1:nrow(df)
  polygon(
    x = c(x_points,rev(x_points)),
    y = c(ci_l[1:nrow(df)],rev(ci_u[1:nrow(df)])), 
    col = rgb(0, 0, 1, 0.2), border = NA)
}

library(xlsx)
setwd("D:/Google Drive/LUCAS 2025/Task 1 - ESTIMATES/1.STANDARD/EU_estimates")
# setwd("C:\\Users\\UTENTE\\Google Drive/LUCAS 2025/Task 1 - ESTIMATES/1.STANDARD/EU_estimates")
# eu <- read.xlsx("Europe_estimates.xlsx",sheetIndex = 1,colClasses = rep("numeric",5))
eu_stn <- read.xlsx("Europe_estimates.xlsx",sheetIndex = 3)

library(xlsx)
setwd("D:/Google Drive/LUCAS 2025/Task 1 - ESTIMATES/2.TWO PHASES light/EU_estimates")
# setwd("C:\\Users\\UTENTE\\Google Drive/LUCAS 2025/Task 1 - ESTIMATES/2.TWO PHASES light/EU_estimates")
# eu <- read.xlsx("Europe_estimates.xlsx",sheetIndex = 1,colClasses = rep("numeric",5))
eu_two <- read.xlsx("Europe_estimates.xlsx",sheetIndex = 3)

years <- c("2015","2018","2022")

#-----------------------------
# LC 2 digits
#-----------------------------

variables1 <- grep("LC1_2",eu_stn$Variable)
eu1 <- eu_stn[variables1,]
eu1 <- eu1[complete.cases(eu1),]
variables2 <- grep("LC1_2",eu_two$Variable)
eu2 <- eu_two[variables2,]
eu2 <- eu2[complete.cases(eu2),]

i = which(eu1$Variable == "LC1_2G5")

par(mfrow=c(2,2))
for (i in (1:nrow(eu1))) {
  var <- eu1$Variable[i]
  # cat("\nVariable: ",var)
  df <- eu1[eu1$Variable == var,]
  df1 <- NULL
  df1$Year <- years
  df1$var <- c(df$Area2015,df$Area2018,df$Area2022)
  df1$ci_l <- c(df$CI.l_2015,df$CI.l_2018,df$CI.l_2022)
  df1$ci_u <- c(df$CI.u_2015,df$CI.u_2018,df$CI.u_2022)
  df1 <- as.data.frame(df1)
  colnames(df1)[2] <- var
  df1
  df <- eu2[eu2$Variable == var,]
  if (nrow(df) > 0) {
    df2 <- NULL
    df2$Year <- years
    df2$var <- c(df$Area2015,df$Area2018,df$Area2022)
    df2$ci_l <- c(df$CI.l_2015,df$CI.l_2018,df$CI.l_2022)
    df2$ci_u <- c(df$CI.u_2015,df$CI.u_2018,df$CI.u_2022)
    df2 <- as.data.frame(df2)
    colnames(df2)[2] <- var
    df2
    ylim = c(min(df1$ci_l,df2$ci_l),max(df1$ci_u,df2$ci_u))
    plotCI(df=df1,
           y=var,
           ci_l="ci_l",
           ci_u="ci_u",
           variable="Year",
           domain=NULL,
           ylim=ylim,
           specs="Standard")
  
    plotCI(df=df2,
           y=var,
           ci_l="ci_l",
           ci_u="ci_u",
           variable="Year",
           domain=NULL,
           ylim=ylim,
           specs="Two-phases")
  }
}
par(mfrow=c(1,1))  

#-----------------------------
# LU 2 digits
#-----------------------------

variables1 <- grep("LU1_2",eu_stn$Variable)
eu1 <- eu_stn[variables1,]
eu1 <- eu1[complete.cases(eu1),]
variables2 <- grep("LU1_2",eu_two$Variable)
eu2 <- eu_two[variables2,]
eu2 <- eu2[complete.cases(eu2),]

par(mfrow=c(2,2))
for (i in (1:nrow(eu1))) {
  var <- eu1$Variable[i]
  # cat("\nVariable: ",var)
  df <- eu1[eu1$Variable == var,]
  df1 <- NULL
  df1$Year <- years
  df1$var <- c(df$Area2015,df$Area2018,df$Area2022)
  df1$ci_l <- c(df$CI.l_2015,df$CI.l_2018,df$CI.l_2022)
  df1$ci_u <- c(df$CI.u_2015,df$CI.u_2018,df$CI.u_2022)
  df1 <- as.data.frame(df1)
  colnames(df1)[2] <- var
  df1
  df <- eu2[eu2$Variable == var,]
  if (nrow(df) > 0) {
    df2 <- NULL
    df2$Year <- years
    df2$var <- c(df$Area2015,df$Area2018,df$Area2022)
    df2$ci_l <- c(df$CI.l_2015,df$CI.l_2018,df$CI.l_2022)
    df2$ci_u <- c(df$CI.u_2015,df$CI.u_2018,df$CI.u_2022)
    df2 <- as.data.frame(df2)
    colnames(df2)[2] <- var
    df2
    ylim = c(min(df1$ci_l,df2$ci_l),max(df1$ci_u,df2$ci_u))
    plotCI(df=df1,
           y=var,
           ci_l="ci_l",
           ci_u="ci_u",
           variable="Year",
           domain=NULL,
           ylim=ylim,
           specs="Standard")
    
    plotCI(df=df2,
           y=var,
           ci_l="ci_l",
           ci_u="ci_u",
           variable="Year",
           domain=NULL,
           ylim=ylim,
           specs="Two-phases")
  }
}
par(mfrow=c(1,1))  
#-----------------------------
# LC 3 digits
#-----------------------------

variables1 <- grep("LC1_3",eu_stn$Variable)
eu1 <- eu_stn[variables1,]
eu1 <- eu1[complete.cases(eu1),]
variables2 <- grep("LC1_3",eu_two$Variable)
eu2 <- eu_two[variables2,]
eu2 <- eu2[complete.cases(eu2),]

par(mfrow=c(2,2))
for (i in (1:nrow(eu1))) {
  var <- eu1$Variable[i]
  # cat("\nVariable: ",var)
  df <- eu1[eu1$Variable == var,]
  df1 <- NULL
  df1$Year <- years
  df1$var <- c(df$Area2015,df$Area2018,df$Area2022)
  df1$ci_l <- c(df$CI.l_2015,df$CI.l_2018,df$CI.l_2022)
  df1$ci_u <- c(df$CI.u_2015,df$CI.u_2018,df$CI.u_2022)
  df1 <- as.data.frame(df1)
  colnames(df1)[2] <- var
  df1
  df <- eu2[eu2$Variable == var,]
  if (nrow(df) > 0) {
    df2 <- NULL
    df2$Year <- years
    df2$var <- c(df$Area2015,df$Area2018,df$Area2022)
    df2$ci_l <- c(df$CI.l_2015,df$CI.l_2018,df$CI.l_2022)
    df2$ci_u <- c(df$CI.u_2015,df$CI.u_2018,df$CI.u_2022)
    df2 <- as.data.frame(df2)
    colnames(df2)[2] <- var
    df2
    ylim = c(min(df1$ci_l,df2$ci_l),max(df1$ci_u,df2$ci_u))
    plotCI(df=df1,
           y=var,
           ci_l="ci_l",
           ci_u="ci_u",
           variable="Year",
           domain=NULL,
           ylim=ylim,
           specs="Standard")
    plotCI(df=df2,
           y=var,
           ci_l="ci_l",
           ci_u="ci_u",
           variable="Year",
           domain=NULL,
           ylim=ylim,
           specs="Two-phases")
  }
}
par(mfrow=c(1,1))  

#-----------------------------
# LU 3 digits
#-----------------------------

variables1 <- grep("LU1_3",eu_stn$Variable)
eu1 <- eu_stn[variables1,]
eu1 <- eu1[complete.cases(eu1),]
variables2 <- grep("LU1_3",eu_two$Variable)
eu2 <- eu_two[variables2,]
eu2 <- eu2[complete.cases(eu2),]

par(mfrow=c(2,2))
for (i in (1:nrow(eu1))) {
  var <- eu1$Variable[i]
  # cat("\nVariable: ",var)
  df <- eu1[eu1$Variable == var,]
  df1 <- NULL
  df1$Year <- years
  df1$var <- c(df$Area2015,df$Area2018,df$Area2022)
  df1$ci_l <- c(df$CI.l_2015,df$CI.l_2018,df$CI.l_2022)
  df1$ci_u <- c(df$CI.u_2015,df$CI.u_2018,df$CI.u_2022)
  df1 <- as.data.frame(df1)
  colnames(df1)[2] <- var
  df1
  df <- eu2[eu2$Variable == var,]
  if (nrow(df) > 0) {
    df2 <- NULL
    df2$Year <- years
    df2$var <- c(df$Area2015,df$Area2018,df$Area2022)
    df2$ci_l <- c(df$CI.l_2015,df$CI.l_2018,df$CI.l_2022)
    df2$ci_u <- c(df$CI.u_2015,df$CI.u_2018,df$CI.u_2022)
    df2 <- as.data.frame(df2)
    colnames(df2)[2] <- var
    df2
    ylim = c(min(df1$ci_l,df2$ci_l),max(df1$ci_u,df2$ci_u))
    plotCI(df=df1,
           y=var,
           ci_l="ci_l",
           ci_u="ci_u",
           variable="Year",
           domain=NULL,
           ylim=ylim,
           specs="Standard")
    plotCI(df=df2,
           y=var,
           ci_l="ci_l",
           ci_u="ci_u",
           variable="Year",
           domain=NULL,
           ylim=ylim,
           specs="Two-phases")
  }
}
par(mfrow=c(1,1)) 
