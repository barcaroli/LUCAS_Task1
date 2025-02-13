######################################################################
# LUCAS - Produce concatenate (2009 / 2022) estimates for each country
# Standard estimation
######################################################################
dire <- getwd()
direnew1 <- paste(dire, "\\2b.allyears_estimates\\", sep = "")
if (dir.exists(direnew1))
  unlink(direnew1,recursive=TRUE)
if (!dir.exists(direnew1)) 
  dir.create(direnew1)

load("countries.RData")

countries

# Estimates comparison 
for (i in c(1:27)) {
  country <- countries[i]
  cat("\nCountry:",country)
  filename1 <- paste("./estimates2009/",country,"_est_LC1_LU1_2009.csv",sep="")
  filename2 <- paste("./estimates2012/",country,"_est_LC1_LU1_2012.csv",sep="")
  filename3 <- paste("./estimates2015/",country,"_est_LC1_LU1_2015.csv",sep="")
  filename4 <- paste("./estimates2018/",country,"_est_LC1_LU1_2018.csv",sep="")
  filename5 <- paste("./2.StandardEstimates2022/",country,"_est_LC1_LU1_2022.csv",sep="")
  
  a <- dir(path = paste0("\\.estimates2009\\"),pattern = country)
  if (length(a) > 0) {
    est1 <- read.csv(filename1,dec=".")
    est1$Variable <- as.character(est1$Variable)
  }
  if (length(a) == 0) est1 <- NULL
  
  a <- dir(path = paste0("\\estimates2012\\"),pattern = country)
  if (length(a) > 0) {
    est2 <- read.csv(filename2,dec=".")
    est2$Variable <- as.character(est2$Variable)
  }
  if (length(a) == 0) est2 <- NULL
  
  est3 <- read.csv(filename3,dec=".")
  est3$Variable <- as.character(est3$Variable)
  
  est4 <- read.csv(filename4,dec=".")
  est4$Variable <- as.character(est4$Variable)
  
  est5 <- read.csv(filename5,dec=".")
  est5$Variable <- as.character(est5$Variable)
  

  est <- NULL
  
  if (!is.null(est1) & !is.null(est2)) {
    est <- merge(est1,est2,by=c("Variable"),all.x=TRUE,all.y=TRUE)
    est <- merge(est,est3,by=c("Variable"),all.x=TRUE,all.y=TRUE)
    ind <- 4
  }
  if (is.null(est1) & !is.null(est2)) {
    est <- est2
    est <- merge(est,est3,by=c("Variable"),all.x=TRUE,all.y=TRUE)
    ind <- 3
  }
  if (is.null(est1) & is.null(est2)) {
    est <- est3
    ind <- 2
  }
  est <- merge(est,est4,by=c("Variable"),all.x=TRUE,all.y=TRUE)
  est <- merge(est,est5,by=c("Variable"),all.x=TRUE,all.y=TRUE)
  if (ind == 4) {
    colnames(est) <- c("Variable","Area_2009","Std_error","CI_lower","CI_upper","CV",
                     "Area_2012","Std_error","CI_lower","CI_upper","CV",
                     "Area_2015","Std_error","CI_lower","CI_upper","CV",
                     "Area_2018","Std_error","CI_lower","CI_upper","CV",
                     "Area_2022","Std_error","CI_lower","CI_upper","CV"
                     )
  }
  if (ind == 3) {
    colnames(est) <- c("Variable",
                       "Area_2012","Std_error","CI_lower","CI_upper","CV",
                       "Area_2015","Std_error","CI_lower","CI_upper","CV",
                       "Area_2018","Std_error","CI_lower","CI_upper","CV",
                       "Area_2022","Std_error","CI_lower","CI_upper","CV"
    )
  }
  if (ind == 2) {
    colnames(est) <- c("Variable",
                       "Area_2015","Std_error","CI_lower","CI_upper","CV",
                       "Area_2018","Std_error","CI_lower","CI_upper","CV",
                       "Area_2022","Std_error","CI_lower","CI_upper","CV"
    )
  }
  filename6 <- paste(direnew1,country,"_est_all.csv",sep="")
  write.table(est,filename6,sep=",",row.names=FALSE,quote=F)
}
