## ----setup, include=FALSE-----------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
setwd("D:/Google Drive/LUCAS 2025/Task 1 - ESTIMATES/3.COMPOSED")
load("countries.RData")


## ----echo=FALSE---------------------------------------------------------------------------
## ----setup, include=FALSE----------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
setwd("D:/Google Drive/LUCAS 2025/Task 1 - ESTIMATES/3.COMPOSED")
load("countries.RData")
countries <- countries[!countries %in% c("CY","MT","LU","LV")]


## ----echo=FALSE--------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------
path_data <- "D:\\Google Drive\\LUCAS 2025\\Task 1 - ESTIMATES\\1.STANDARD\\estimates2022\\"
cv2022lc <- matrix(rep(NA,216),nrow=length(countries),ncol=8)
cv2022lc <- as.data.frame(cv2022lc)
row.names(cv2022lc) <- countries
colnames(cv2022lc) <- paste0("cv_",c(LETTERS[1:8]))
cv2022lu <- matrix(rep(NA,108),nrow=length(countries),ncol=4)
cv2022lu <- as.data.frame(cv2022lu)
colnames(cv2022lu) <- paste0("cv_",c("U1","U2","U3","U4"))
row.names(cv2022lu) <- countries
k <- 0
for (i in (countries)) {
  # cat("\n",i)
  # a <- read.csv(paste0("./estimates2022cal/",countries[i],"_est_LC1_LU1_2022.csv"))
  a <- read.csv(paste0(path_data,i,"_est_LC1_LU1_2022.csv"))
  a <- a[-1,]
  k <- k + 1
  cv2022lc[k,] <- a[c(1:8),ncol(a)]
  cv2022lu[k,] <- a[c(9:12),ncol(a)]
}
#-----------------------------------------------------------------------------
# Estimates 2022 composed estimator
ris <- read.csv("ris_fpc_all_countries_new.csv",dec=".")

# Artificial ---------------------------------------------------------
par(mfrow=c(1,2))
plot(as.numeric(ris$cv_A)/100,cex=1,col="red",ylab="CV",xlab="Countries",pch="*",ylim=c(0,max(c(ris$cv_A/100,c(cv2022lc[ ,1])),na.rm=TRUE)))
abline(a=0.025,b=0,col="red")
text(as.numeric(ris$cv_A)/100, labels = ris$country, pos = 4, cex=0.6)
title("2022 Composed estimator - Artificial",cex.main=0.7)

plot(cv2022lc[ ,1], xlab = "Countries", ylab = "CV",pch="*",col="red",ylim=c(0,max(c(ris$cv_A/100,c(cv2022lc[ ,1])),na.rm=TRUE)))
abline(a=0.025,b=0,col="red")
text(cv2022lc[ ,1], labels = row.names(cv2022lc), pos = 4, cex=0.6)
title("2022 Current estimator - Artificial",cex.main=0.7)


# Cropland ---------------------------------------------------------
par(mfrow=c(1,2))
plot(as.numeric(ris$cv_B)/100,cex=1,col="red",ylab="CV",xlab="Countries",pch="*",ylim=c(0,max(c(ris$cv_B/100,c(cv2022lc[ ,2])),na.rm=TRUE)))
abline(a=0.025,b=0,col="red")
text(as.numeric(ris$cv_B)/100, labels = ris$country, pos = 4, cex=0.6)
title("2022 Composed estimator - Cropland",cex.main=0.7)

plot(cv2022lc[ ,2], xlab = "Countries", ylab = "CV",pch="*",col="red",ylim=c(0,max(c(ris$cv_B/100,c(cv2022lc[ ,2])),na.rm=TRUE)))
abline(a=0.025,b=0,col="red")
text(cv2022lc[ ,2], labels = row.names(cv2022lc), pos = 4, cex=0.6)
title("2022 Current estimator - Cropland",cex.main=0.7)

# Woodland ---------------------------------------------------------

par(mfrow=c(1,2))
plot(as.numeric(ris$cv_C)/100,cex=1,col="red",ylab="CV",xlab="Countries",pch="*",ylim=c(0,max(c(ris$cv_C/100,c(cv2022lc[ ,3])),na.rm=TRUE)))
abline(a=0.025,b=0,col="red")
text(as.numeric(ris$cv_C)/100, labels = ris$country, pos = 4, cex=0.6)
title("2022 Composed estimator - Woodland",cex.main=0.7)

plot(cv2022lc[ ,3], xlab = "Countries", ylab = "CV",pch="*",col="red",ylim=c(0,max(c(ris$cv_C/100,c(cv2022lc[ ,3])),na.rm=TRUE)))
abline(a=0.025,b=0,col="red")
text(cv2022lc[ ,3], labels = row.names(cv2022lc), pos = 4, cex=0.6)
title("2022 Current estimator - Woodland",cex.main=0.7)


# Shrubland ---------------------------------------------------------

par(mfrow=c(1,2))
plot(as.numeric(ris$cv_D)/100,cex=1,col="red",ylab="CV",xlab="Countries",pch="*",ylim=c(0,max(c(ris$cv_D/100,c(cv2022lc[ ,4])),na.rm=TRUE)))
abline(a=0.025,b=0,col="red")
text(as.numeric(ris$cv_D)/100, labels = ris$country, pos = 4, cex=0.6)
title("2022 Composed estimator - Shrubland",cex.main=0.7)

plot(cv2022lc[ ,4], xlab = "Countries", ylab = "CV",pch="*",col="red",ylim=c(0,max(c(ris$cv_D/100,c(cv2022lc[ ,4])),na.rm=TRUE)))
abline(a=0.025,b=0,col="red")
text(cv2022lc[ ,4], labels = row.names(cv2022lc), pos = 4, cex=0.6)
title("2022 Current estimator - Shrubland",cex.main=0.7)


# Grassland ---------------------------------------------------------

par(mfrow=c(1,2))
plot(as.numeric(ris$cv_E)/100,cex=1,col="red",ylab="CV",xlab="Countries",pch="*",ylim=c(0,max(c(ris$cv_E/100,c(cv2022lc[ ,5])),na.rm=TRUE)))
abline(a=0.025,b=0,col="red")
text(as.numeric(ris$cv_E)/100, labels = ris$country, pos = 4, cex=0.6)
title("2022 Composed estimator - Grassland",cex.main=0.7)

plot(cv2022lc[ ,5], xlab = "Countries", ylab = "CV",pch="*",col="red",ylim=c(0,max(c(ris$cv_E/100,c(cv2022lc[ ,5])),na.rm=TRUE)))
abline(a=0.025,b=0,col="red")
text(cv2022lc[ ,5], labels = row.names(cv2022lc), pos = 4, cex=0.6)
title("2022 Current estimator - Grassland",cex.main=0.7)


# Bareland ---------------------------------------------------------

par(mfrow=c(1,2))
plot(as.numeric(ris$cv_F)/100,cex=1,col="red",ylab="CV",xlab="Countries",pch="*",ylim=c(0,max(c(ris$cv_F/100,c(cv2022lc[ ,6])),na.rm=TRUE)))
abline(a=0.025,b=0,col="red")
text(as.numeric(ris$cv_F)/100, labels = ris$country, pos = 4, cex=0.6)
title("2022 Composed estimator - Bareland",cex.main=0.7)

plot(cv2022lc[ ,6], xlab = "Countries", ylab = "CV",pch="*",col="red",ylim=c(0,max(c(ris$cv_F/100,c(cv2022lc[ ,6])),na.rm=TRUE)))
abline(a=0.025,b=0,col="red")
text(cv2022lc[ ,6], labels = row.names(cv2022lc), pos = 4, cex=0.6)
title("2022 Current estimator - Bareland",cex.main=0.7)

# Water areas ---------------------------------------------------------

par(mfrow=c(1,2))
plot(as.numeric(ris$cv_G)/100,cex=1,col="red",ylab="CV",xlab="Countries",pch="*",ylim=c(0,max(c(ris$cv_G/100,c(cv2022lc[ ,7])),na.rm=TRUE)))
abline(a=0.025,b=0,col="red")
text(as.numeric(ris$cv_G)/100, labels = ris$country, pos = 4, cex=0.6)
title("2022 Composed estimator - Water",cex.main=0.7)

plot(cv2022lc[ ,7], xlab = "Countries", ylab = "CV",pch="*",col="red",ylim=c(0,max(c(ris$cv_G/100,c(cv2022lc[ ,7])),na.rm=TRUE)))
abline(a=0.025,b=0,col="red")
text(cv2022lc[ ,7], labels = row.names(cv2022lc), pos = 4, cex=0.6)
title("2022 Current estimator - Water",cex.main=0.7)

# Wetlands ---------------------------------------------------------

par(mfrow=c(1,2))
plot(as.numeric(ris$cv_H)/100,cex=1,col="red",ylab="CV",xlab="Countries",pch="*",ylim=c(0,max(c(ris$cv_H/100,c(cv2022lc[ ,8])),na.rm=TRUE)))
abline(a=0.025,b=0,col="red")
text(as.numeric(ris$cv_H)/100, labels = ris$country, pos = 4, cex=0.6)
title("2022 Composed estimator - Wetlands",cex.main=0.7)

plot(cv2022lc[ ,8], xlab = "Countries", ylab = "CV",pch="*",col="red",ylim=c(0,max(c(ris$cv_H/100,c(cv2022lc[ ,8])),na.rm=TRUE)))
abline(a=0.025,b=0,col="red")
text(cv2022lc[ ,8], labels = row.names(cv2022lc), pos = 4, cex=0.6)
title("2022 Current estimator - Wetlands",cex.main=0.7)

# U1 ---------------------------------------------------------

par(mfrow=c(1,2))
plot(as.numeric(ris$cv_U1)/100,cex=1,col="red",ylab="CV",xlab="Countries",pch="*",ylim=c(0,max(c(ris$cv_U1/100,c(cv2022lu[ ,1])),na.rm=TRUE)))
abline(a=0.025,b=0,col="red")
text(as.numeric(ris$cv_U1)/100, labels = ris$country, pos = 4, cex=0.6)
title("2022 Composed estimator - U1",cex.main=0.7)

plot(cv2022lu[ ,1], xlab = "Countries", ylab = "CV",pch="*",col="red",ylim=c(0,max(c(ris$cv_U1/100,c(cv2022lu[ ,1])),na.rm=TRUE)))
abline(a=0.025,b=0,col="red")
text(cv2022lu[ ,1], labels = row.names(cv2022lu), pos = 4, cex=0.6)
title("2022 Current estimator - U1",cex.main=0.7)

# U2 ---------------------------------------------------------

par(mfrow=c(1,2))
plot(as.numeric(ris$cv_U2)/100,cex=1,col="red",ylab="CV",xlab="Countries",pch="*",ylim=c(0,max(c(ris$cv_U2/100,c(cv2022lu[ ,2])),na.rm=TRUE)))
abline(a=0.025,b=0,col="red")
text(as.numeric(ris$cv_U2)/100, labels = ris$country, pos = 4, cex=0.6)
title("2022 Composed estimator - U2",cex.main=0.7)

plot(cv2022lu[ ,2], xlab = "Countries", ylab = "CV",pch="*",col="red",ylim=c(0,max(c(ris$cv_U2/100,c(cv2022lu[ ,2])),na.rm=TRUE)))
abline(a=0.025,b=0,col="red")
text(cv2022lu[ ,2], labels = row.names(cv2022lu), pos = 4, cex=0.6)
title("2022 Current estimator - U2",cex.main=0.7)

# U3 ---------------------------------------------------------

par(mfrow=c(1,2))
plot(as.numeric(ris$cv_U3)/100,cex=1,col="red",ylab="CV",xlab="Countries",pch="*",ylim=c(0,max(c(ris$cv_U3/100,c(cv2022lu[ ,3])),na.rm=TRUE)))
abline(a=0.025,b=0,col="red")
text(as.numeric(ris$cv_U3)/100, labels = ris$country, pos = 4, cex=0.6)
title("2022 Composed estimator - U3",cex.main=0.7)

plot(cv2022lu[ ,3], xlab = "Countries", ylab = "CV",pch="*",col="red",ylim=c(0,max(c(ris$cv_U3/100,c(cv2022lu[ ,3])),na.rm=TRUE)))
abline(a=0.025,b=0,col="red")
text(cv2022lu[ ,3], labels = row.names(cv2022lu), pos = 4, cex=0.6)
title("2022 Current estimator - U3",cex.main=0.7)

# U4 ---------------------------------------------------------

par(mfrow=c(1,2))
plot(as.numeric(ris$cv_U4)/100,cex=1,col="red",ylab="CV",xlab="Countries",pch="*",ylim=c(0,max(c(ris$cv_U4/100,c(cv2022lu[ ,4])),na.rm=TRUE)))
abline(a=0.025,b=0,col="red")
text(as.numeric(ris$cv_U4)/100, labels = ris$country, pos = 4, cex=0.6)
title("2022 Composed estimator - U4",cex.main=0.7)

plot(cv2022lu[ ,4], xlab = "Countries", ylab = "CV",pch="*",col="red",ylim=c(0,max(c(ris$cv_U4/100,c(cv2022lu[ ,4])),na.rm=TRUE)))
abline(a=0.025,b=0,col="red")
text(cv2022lu[ ,4], labels = row.names(cv2022lu), pos = 4, cex=0.6)
title("2022 Current estimator - U4",cex.main=0.7)

#---------------------------------------------------------
# Box plots
par(mfrow=c(1,2))
par(cex.axis = 0.6)
# Artificial
a <- cbind(ris$cv_A,cv2022lc$cv_A)
a <- as.data.frame(a)
a$V1 <- a$V1 / 100
boxplot(a$V1, a$V2, names = c("Composed", "Current"),
        ylab = "CVs",
        col = c("orange", "green"))
title("Land Cover Artificial",cex.main=0.7)
abline(h = 0.025, col = "blue", lwd = 2)

# Cropland
a <- cbind(ris$cv_B,cv2022lc$cv_B)
a <- as.data.frame(a)
a$V1 <- a$V1 / 100
boxplot(a$V1, a$V2, names = c("Composed", "Current"),
        ylab = "CVs",
        col = c("orange", "green"))
abline(h = 0.025, col = "blue", lwd = 2)
title("Land Cover Cropland",cex.main=0.7)

# Woodland
a <- cbind(ris$cv_C,cv2022lc$cv_C)
a <- as.data.frame(a)
a$V1 <- a$V1 / 100
boxplot(a$V1, a$V2, names = c("Composed", "Current"),
        ylab = "CVs",
        col = c("orange", "green"))
title("Land Cover Woodland",cex.main=0.7)
abline(h = 0.025, col = "blue", lwd = 2)

# Bareland
a <- cbind(ris$cv_D,cv2022lc$cv_D)
a <- as.data.frame(a)
a$V1 <- a$V1 / 100
boxplot(a$V1, a$V2, names = c("Composed", "Current"),
        ylab = "CVs",
        col = c("orange", "green"))
title("Land Cover Bareland",cex.main=0.7)
abline(h = 0.025, col = "blue", lwd = 2)

# Grassland
a <- cbind(ris$cv_E,cv2022lc$cv_E)
a <- as.data.frame(a)
a$V1 <- a$V1 / 100
boxplot(a$V1, a$V2, names = c("Composed", "Current"),
        ylab = "CVs",
        col = c("orange", "green"))
title("Land Cover Grassland",cex.main=0.7)
abline(h = 0.025, col = "blue", lwd = 2)

# Shrubland
a <- cbind(ris$cv_F,cv2022lc$cv_F)
a <- as.data.frame(a)
a$V1 <- a$V1 / 100
boxplot(a$V1, a$V2, names = c("Composed", "Current"),
        ylab = "CVs",
        col = c("orange", "green"))
abline(h = 0.025, col = "blue", lwd = 2)

# Water
a <- cbind(ris$cv_G,cv2022lc$cv_G)
a <- as.data.frame(a)
a$V1 <- a$V1 / 100
boxplot(a$V1, a$V2, names = c("Composed", "Current"),
        ylab = "CVs",
        col = c("orange", "green"), cex.names = 0.8)
title("Land Cover Water",cex.main=0.7)
abline(h = 0.025, col = "blue", lwd = 2)

# Wetland
a <- cbind(ris$cv_H,cv2022lc$cv_H)
a <- as.data.frame(a)
a$V1 <- a$V1 / 100
boxplot(a$V1, a$V2, names = c("Composed", "Current"),
        ylab = "CVs",
        col = c("orange", "green"), cex.names = 0.8)
title("Land Cover Wetland",cex.main=0.7)
abline(h = 0.025, col = "blue", lwd = 2)

# U1
a <- cbind(ris$cv_U1,cv2022lu$cv_U1)
a <- as.data.frame(a)
a$V1 <- a$V1 / 100
boxplot(a$V1, a$V2, names = c("Composed", "Current"),
        ylab = "CVs",
        col = c("orange", "green"), cex.names = 0.8)
title("Land Use U1",cex.main=0.7)
abline(h = 0.025, col = "blue", lwd = 2)

# U2
a <- cbind(ris$cv_U2,cv2022lu$cv_U2)
a <- as.data.frame(a)
a$V1 <- a$V1 / 100
boxplot(a$V1, a$V2, names = c("Composed", "Current"),
        ylab = "CVs",
        col = c("orange", "green"), cex.names = 0.8)
title("Land Use U2",cex.main=0.7)
abline(h = 0.025, col = "blue", lwd = 2)

# U3
a <- cbind(ris$cv_U3,cv2022lu$cv_U3)
a <- as.data.frame(a)
a$V1 <- a$V1 / 100
boxplot(a$V1, a$V2, names = c("Composed", "Current"),
        ylab = "CVs",
        col = c("orange", "green"), cex.names = 0.8)
title("Land Use U3",cex.main=0.7)
abline(h = 0.025, col = "blue", lwd = 2)

# U4
a <- cbind(ris$cv_U4,cv2022lu$cv_U4)
a <- as.data.frame(a)
a$V1 <- a$V1 / 100
boxplot(a$V1, a$V2, names = c("Composed", "Current"),
        ylab = "CVs",
        col = c("orange", "green"), cex.names = 0.8)
title("Land Use U4",cex.main=0.7)
abline(h = 0.025, col = "blue", lwd = 2)

