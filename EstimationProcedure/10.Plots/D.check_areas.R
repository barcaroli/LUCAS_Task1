#---------------------------------
# Script to check the areas of
# EU 27 countries LC and LU estimates
#---------------------------------
# Input: ./9.Tables_country/all_countries_estimates.xlsx
#---------------------------------

library(dplyr)
library(readxl)
library(stringr)

# read area file:
areas = read.csv("../areas_2015_2024.csv")
load("../countries.RData")


check=data.frame(matrix(nrow=length(countries), ncol=6))
colnames(check)=c("Country", "LC-2digits", "LC-3digits","LU-2digits", "LU-3digits", "area")
check$Country=countries


# for each country
for (j in 1:length(countries)){
  country=countries[j]
  print(country)
  estimates=read_xlsx("../9.Tables_country/all_countries_estimates.xlsx", sheet = j)
  
  # select only current estimates: 
  estimates=estimates[,c(1,((ncol(estimates)-4):ncol(estimates)))]
  colnames(estimates)=c("Variable", "Area", "Std_error", "CI_lower", "CI_upper", "CV" )
  
  # select only area variable
  areas_estimates=estimates  %>% filter(grepl(pattern="area", Variable)) 
  
  # select only country's most updated areas: 
  areas_c= areas %>% filter(str_detect(string=NUTS2, pattern=paste0("^", country))) %>% select(area2024)
  
  # select only SURVEY_LC1, SURVEY_LU1
  estimates= estimates %>% filter(grepl(pattern="SURVEY_LC1|SURVEY_LU1", Variable)) 
  
  # comparison between LC1 and LC2, LC3: 
  # Land Cover
  LC=estimates$Variable[startsWith(prefix="SURVEY_LC1", estimates$Variable)]
  LC1digit=LC[nchar(LC)==13]
  LC2digits=LC[nchar(LC)==14]
  LC3digits=LC[nchar(LC)==15]
  check_boolean_LC2d=vector(length=length(LC1digit))
  check_boolean_LC3d=vector(length=length(LC1digit))
  
  
  for (i in 1:length(LC1digit)){
    macro=LC1digit[i]
    print(macro)
    letter=substr(macro,13,13)
    digits2=LC2digits[which(startsWith(prefix=paste(substr(macro,1,11), "2", letter, sep=""), LC2digits))]
    print(paste(digits2, collapse = "+"))
    #print(sum(estimates$Area[estimates$Variable %in% digits2], na.rm=TRUE))
    #print(estimates$Area[estimates$Variable %in% macro])
    print(paste("Check:", round(sum(estimates$Area[estimates$Variable %in% digits2], na.rm=TRUE),6)==
          round(estimates$Area[estimates$Variable %in% macro],6)))
    check_boolean_LC2d[i]=round(sum(estimates$Area[estimates$Variable %in% digits2], na.rm=TRUE),6)==
      round(estimates$Area[estimates$Variable %in% macro],6)
    
    
    digits3=LC3digits[which(startsWith(prefix=paste(substr(macro,1,11), "3", letter, sep=""), LC3digits))]
    print(paste(digits3, collapse = "+"))
    #print(sum(estimates$Area[estimates$Variable %in% digits3], na.rm=TRUE))
    #print(estimates$Area[estimates$Variable %in% macro])
    print(paste("Check:", round(sum(estimates$Area[estimates$Variable %in% digits3], na.rm=TRUE),6)==
                  round(estimates$Area[estimates$Variable %in% macro],6)))
    check_boolean_LC3d[i]=round(sum(estimates$Area[estimates$Variable %in% digits3], na.rm=TRUE),6)==
      round(estimates$Area[estimates$Variable %in% macro],6)
  }
  
  print(paste(country, "- Check LC 2 digits:", all(check_boolean_LC2d)))
  print(paste(country, "- Check LC 3 digits:", all(check_boolean_LC3d)))
  
  check[j,2:3]=c(all(check_boolean_LC2d),all(check_boolean_LC3d))
  
  # comparison between LU1 and LU2, LU3: 
  # Land Use
  LU=estimates$Variable[startsWith(prefix="SURVEY_LU1", estimates$Variable)]
  LU1digit=LU[nchar(LU)==14]
  LU2digits=LU[nchar(LU)==15]
  LU3digits=LU[nchar(LU)==16]
  check_boolean_LU2d=vector(length=length(LU1digit))
  check_boolean_LU3d=vector(length=length(LU1digit))
  
  
  for (i in 1:length(LU1digit)){
    macro=LU1digit[i]
    print(macro)
    letter=substr(macro,13,14)
    digits2=LU2digits[which(startsWith(prefix=paste(substr(macro,1,11), "2", letter, sep=""), LU2digits))]
    print(paste(digits2, collapse = "+"))
    #print(sum(estimates$Area[estimates$Variable %in% digits2], na.rm=TRUE))
    #print(estimates$Area[estimates$Variable %in% macro])
    print(paste("Check:", round(sum(estimates$Area[estimates$Variable %in% digits2], na.rm=TRUE),6)==
                  round(estimates$Area[estimates$Variable %in% macro],6)))
    check_boolean_LU2d[i]=round(sum(estimates$Area[estimates$Variable %in% digits2], na.rm=TRUE),6)==
      round(estimates$Area[estimates$Variable %in% macro],6)
    
    
    digits3=LU3digits[which(startsWith(prefix=paste(substr(macro,1,11), "3", letter, sep=""), LU3digits))]
    print(paste(digits3, collapse = "+"))
    #print(sum(estimates$Area[estimates$Variable %in% digits3], na.rm=TRUE))
    #print(estimates$Area[estimates$Variable %in% macro])
    print(paste("Check:", round(sum(estimates$Area[estimates$Variable %in% digits3], na.rm=TRUE),6)==
                  round(estimates$Area[estimates$Variable %in% macro],6)))
    check_boolean_LU3d[i]=round(sum(estimates$Area[estimates$Variable %in% digits3], na.rm=TRUE),6)==
      round(estimates$Area[estimates$Variable %in% macro],6)
  }
  
  print(paste(country, "- Check LU 2 digits:",all(check_boolean_LU2d)))
  print(paste(country, "- Check LU 3 digits:",all(check_boolean_LU3d)))
  
  check[j,4:5]=c(all(check_boolean_LU2d),all(check_boolean_LU3d))
  
  # comparison between areas
  check_boolean_areas=round(areas_estimates$Area, 6)==round(sum(areas_c, na.rm=TRUE),6)
  print(paste(country, "- Check areas:",check_boolean_areas ))
  check[j,6]=check_boolean_areas
}
