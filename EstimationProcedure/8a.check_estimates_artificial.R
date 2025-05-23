#######################################################################
# LUCAS - Check of estimates variations for Land Cover A and settlement
#######################################################################
# Input:  ./6.allyears_estimates/
# Output: ./8.anomalies/artificial_anomalies.xlsx
#######################################################################
library(xlsx)
library(flextable)
library(officer)

dire <- getwd()
direnew1 <- paste(dire, "./8.anomalies/", sep = "")
if (!dir.exists(direnew1))
  dir.create(direnew1)

filename = "./8.anomalies/artificial_anomalies.xlsx"
unlink(filename)

variable = c("SURVEY_LC1_1A",
             "SURVEY_LC1_2A1","SURVEY_LC1_2A2","SURVEY_LC1_2A3","SURVEY_LC1_3A11",
             "SURVEY_LC1_3A12","SURVEY_LC1_3A13","SURVEY_LC1_3A21","SURVEY_LC1_3A22",
             "SURVEY_LC1_3A30","settlement1","settl_pc")

load("countries.Rdata")

doc <- read_docx()


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
  i <- which(countries == "CY")
  ncountries <- 0
  k = 0
  for (i in (1:length(countries))) {
    cat("\n Country: ",countries[i],"\n")
    st <- paste("df <- read.csv('./6.allyears_estimates/",countries[i],"_est_all.csv',dec='.')",sep="")
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
            anom$Area2018_CI_u[k] <- df[df$Variable==variable[w],c("CI_upper.2")]
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
      }
    }
  }
  anom <- as.data.frame(anom)
  if (!(nrow(anom) == 1 && is.na(anom$country))) {
    # anom <- anom[,c(1,3,6,9,12,15,18)]
    write.xlsx(anom, filename, sheetName = variable[w], 
               col.names = TRUE, row.names = FALSE, append = TRUE)
    anom$Area2018 <- round(anom$Area2018,2)
    anom$Area2018_CI_l <- round(anom$Area2018_CI_l,2)
    anom$Area2018_CI_u <- round(anom$Area2018_CI_u,2)
    anom$Area2022 <- round(anom$Area2022,2)
    anom$Area2022_CI_l <- round(anom$Area2022_CI_l,2)
    anom$Area2022_CI_u <- round(anom$Area2022_CI_u,2)
    ft <- flextable(anom[, c("country", 
                             "Area2018", "Area2018_CI_l","Area2018_CI_u",
                             "Area2022", "Area2022_CI_l","Area2022_CI_u",
                             "signal")])
    ft <- set_caption(ft, caption = paste0("Anomalies for variable ",variable[w]))
    ft <- fontsize(ft, size = 8, part = "all")
    ft <- set_table_properties(ft, layout = "fixed", width = 0.5)
    ft <- autofit(ft)
    ft
    doc <- body_add_flextable(doc, value = ft)
    print(doc, target = "./8.anomalies/anomalies_artificial.docx")
  }
}


