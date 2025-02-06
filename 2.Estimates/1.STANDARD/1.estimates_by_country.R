##############################################################
# LUCAS - Calibration and estimation by country 2022
##############################################################
start_time <- Sys.time()
options(stringsAsFactors = TRUE)
library(data.table)
library(sf)
library(ReGenesees)
library(openxlsx)

setwd("D:/Google Drive/LUCAS 2025/Task 1 - ESTIMATES/1.STANDARD")
path_data <- "D:/Google Drive/LUCAS 2025/2.DATA/"
path_master <- "D:/Google Drive/LUCAS 2025/2.Master/"

# setwd("C:\\Users\\UTENTE\\Google Drive/LUCAS 2025/Task 1 - ESTIMATES/1.STANDARD")
# path_data <- "C:\\Users\\UTENTE\\Google Drive/LUCAS 2025/2.DATA/"
# Prepare the path for the output
dire <- getwd()
direnew1 <- paste(dire, "/estimates2022", sep = "")
if (dir.exists(direnew1))
  unlink(direnew1,recursive=TRUE)
if (!dir.exists(direnew1))
  dir.create(direnew1)
# 
direnew2 <- paste(dire, "/weights2022", sep = "")
if (dir.exists(direnew2))
  unlink(direnew2,recursive=TRUE)
if (!dir.exists(direnew2))
  dir.create(direnew2)

#------------------------------------------
# Read survey 2022
#------------------------------------------
s2022 <- fread(paste0(path_data,"LUCAS22_corrected_complete.csv"))
table(s2022$fao_class_name)/nrow(s2022)
#remove rows with NA in LC, LU
s2022=s2022[!(is.na(s2022$SURVEY_LC1) | is.na(s2022$SURVEY_LU1)), ]
a <- s2022[s2022$SURVEY_LC1 == "8", ]
s2022=s2022[s2022$SURVEY_LC1 != "8", ]
s2022=s2022[s2022$SURVEY_LU1 != "8", ]
#remove rows with NA in LC, LU
s2022=s2022[!(is.na(s2022$SURVEY_LC1) | is.na(s2022$SURVEY_LU1)), ]
# write.table(s2022,file=paste0(path_data,"LUCAS22_corrected_complete.csv"),quote=F,row.names=F,sep=",")
# Fill the missing digits
s2022$SURVEY_LC1 <- as.character(s2022$SURVEY_LC1)
table(s2022$SURVEY_LC1)
s2022$SURVEY_LC1 <- ifelse(nchar(as.character(s2022$SURVEY_LC1)) == 2,
                           paste0(s2022$SURVEY_LC1,"x"),s2022$SURVEY_LC1)
s2022$SURVEY_LC1 <- ifelse(nchar(as.character(s2022$SURVEY_LC1)) == 1,
                           paste0(s2022$SURVEY_LC1,"xx"),s2022$SURVEY_LC1)
table(s2022$SURVEY_LC1)
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
# Eliminate incomplete or missing LC and LU
a <- s2022[nchar(as.character(s2022$SURVEY_LC1_2)) < 2,]
# 688
b <- s2022[nchar(as.character(s2022$SURVEY_LC1_3)) < 3,]
# 6190
c <- s2022[nchar(as.character(s2022$SURVEY_LU1_2)) < 3,]
d <- s2022[nchar(as.character(s2022$SURVEY_LU1_3)) < 4,]
s2022 <- s2022[!(nchar(as.character(s2022$SURVEY_LC1_2)) < 2) &
                 !(nchar(as.character(s2022$SURVEY_LC1_3)) < 3),]
table(s2022$SURVEY_LC1,useNA="ifany")
table(s2022$SURVEY_LU1,useNA="ifany")
###########################################################
# Calculate fpc
###########################################################
# num<-aggregate(s2022$ones,by=list(s2022$STRATUM_LUCAS),FUN=sum)
# num$n=num$x
# num$x<-NULL
# den<-aggregate(s2022$WGT_LUCAS,by=list(s2022$STRATUM_LUCAS),FUN=sum)
# den$N=den$x
# den$x<-NULL
# fpc<-merge(num,den,by="Group.1")
# fpc$STRATUM_LUCAS<-fpc$Group.1
# fpc$fpc=fpc$n/fpc$N
# s2022<-merge(s2022,fpc[,c("STRATUM_LUCAS","fpc")],by="STRATUM_LUCAS")

################################################################
# Read areas
areas <- read.csv(paste0(path_data,"areas_2015_2024.csv"),colClasses = c(rep('character',4), rep('numeric',10)))
################################################################


#########################################
# MASTER
#########################################
# load(paste0(path_data,"Master_con_hrl_e_NUTS24.RData"))
# master<-Master_con_hrl_e_NUTS24
# rm(Master_con_hrl_e_NUTS24)
load(paste0(path_master,"master_complete.RData"))

table(master$NUTS0_24,useNA="ifany")
# c <- master[master$NUTS0_24=="",]
# table(c$NUTS0_24,useNA="ifany")
################################
# geovisualize master points
################################
# library(sf)
# library(mapview)
# c2 <- st_as_sf(c,coords=c("Y_LAEA","X_LAEA"))
# st_crs(c2) <- "Coordinate Reference System:
#   User input: +proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs 
#   wkt:
# PROJCRS['unknown',
#     BASEGEOGCRS['unknown',
#         DATUM['Unknown based on GRS 1980 ellipsoid',
#             ELLIPSOID['GRS 1980',6378137,298.257222101,
#                 LENGTHUNIT['metre',1],
#                 ID['EPSG',7019]]],
#         PRIMEM['Greenwich',0,
#             ANGLEUNIT['degree',0.0174532925199433],
#             ID['EPSG',8901]]],
#     CONVERSION['unknown',
# METHOD['Lambert Azimuthal Equal Area',
#        ID['EPSG',9820]],
# PARAMETER['Latitude of natural origin',52,
#           ANGLEUNIT['degree',0.0174532925199433],
#           ID['EPSG',8801]],
# PARAMETER['Longitude of natural origin',10,
#           ANGLEUNIT['degree',0.0174532925199433],
#           ID['EPSG',8802]],
# PARAMETER['False easting',4321000,
#           LENGTHUNIT['metre',1],
#           ID['EPSG',8806]],
# PARAMETER['False northing',321000,
#             ID['EPSG',8807]]],
#     CS[Cartesian,2],
#         AXIS['(E)',east,
#             ORDER[1],
#             LENGTHUNIT['metre',1,
#                 ID['EPSG',9001]]],
#         AXIS['(N)',north,
#             ORDER[2],
#             LENGTHUNIT['metre',1,
#                 ID['EPSG',9001]]]]"
# library(mapview)
# mapview(c2, map.types = c("OpenStreetMap"))

###########################################################################
# Assign the new NUTS24 to the sample
# s2022 <- merge(s2022,master[,c("POINT_ID","NUTS0_24","NUTS1_24","NUTS2_24","NUTS3_24")],by="POINT_ID",all.x=TRUE)
s2022$NUTS0_24 <- as.factor(s2022$NUTS0_24)
s2022$NUTS1_24 <- as.factor(s2022$NUTS1_24)
s2022$NUTS2_24 <- as.factor(s2022$NUTS2_24)
# a <- s2022[is.na(s2022$NUTS2_24),]
# b <- master[is.na(master$NUTS2_24),]

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
table(master$ELEV2,useNA="ifany")
# master$ELEV2 <- ifelse(master$ELEV_DEM < 100, 1, 
#                        ifelse(master$ELEV_DEM < 200, 2,
#                               ifelse(master$ELEV < 500, 3,
#                                      ifelse(master$ELEV_DEM < 1000, 4,
#                                             ifelse(master$ELEV_DEM < 1500, 5, 6)))))
master$ELEV2 <- as.factor(master$ELEV2)

master$CLC18_1d<-factor(substr(master$CLC18_vett,1,1))
master$NUTS0_24 <- factor(master$NUTS0_24)
master$NUTS1_24 <- factor(master$NUTS1_24)
master$NUTS2_24 <- factor(master$NUTS2_24)
master$NUTS3_24 <- factor(master$NUTS3_24)
master$BCK18_R <- factor(master$BCK18_R)
master$BCK21_R <- factor(master$BCK21_R)
master$CLC18_vett<-factor(master$CLC18_vett)
master$imperviousness <- master$IMD18_10 /100
master$art_imp <- master$imperviousness * master$point_area
master$ones<-1


##########
# LOOP
##########

paesi <- levels(as.factor(s2022$NUTS0_24))
i = which(paesi=="AT")
i

for (i in c(1:length(paesi))) {
    country <- paesi[i]
    cat("\n Country: ",country,"\n")
    # seleziono dal master le vaariabili che possono essere utili
    m <- master[master$NUTS0_24 == country,c("POINT_ID","point_area","ones","NUTS0_24","NUTS1_24","NUTS2_24","NUTS3_24",
                                             "ELEV2","N2K_SITETYPE",
                                             "IMP18_10_cl", "IBU18_10","GRA18_10","FTY18_10",
                                             "CLC18_1d","BCK18_R","BCK21_R")]
    
    # m <- master[master$NUTS0_24 == country,c("POINT_ID","GRA18_10","FTY18_10","BCK21_R")]
    ##########################################
    m <- m[!is.na(m$NUTS2_24),]
    # m$geometry<-NULL
    # eliminate empty levels in factor variables in m 
    m<-droplevels(m)
    # select country sample with variables of interest
    s <- merge(s2022[,c("POINT_ID","fpc","Pop2023",
                        "SURVEY_LC1","SURVEY_LU1","SURVEY_LC1_1","SURVEY_LU1_1",
                        "SURVEY_LC1_2","SURVEY_LU1_2","SURVEY_LC1_3","SURVEY_LU1_3",
                        "WGT_LUCAS", "STRATUM_LUCAS","settlement","fao_class_name",
                        "lud","lue","settl_pc","area")], 
               m,
               by=c("POINT_ID"))  
    s <- s[!is.na(s$WGT_LUCAS),]
    s<-droplevels(s)
    # align levels of factor variables in s and m
    levels(s$ELEV2)    <- levels(m$ELEV2)
    levels(s$NUTS2_24) <- levels(m$NUTS2_24)
    levels(s$BCK21_R)  <- levels(m$BCK21_R)
    levels(s$BCK18_R)  <- levels(m$BCK18_R)
    levels(s$GRA18_10) <- levels(m$GRA18_10)
    levels(s$FTY18_10) <- levels(m$FTY18_10R)
    levels(s$CLC18_1d) <- levels(m$CLC18_1d)
    ###############   
    # CALIBRATION
    ###############
    s <- s[order(s$NUTS2_24,s$STRATUM_LUCAS),]
    if (length(levels(s$NUTS2_24)) > 1) {
        # design
        des <- e.svydesign(data=s, 
                           ids= ~ POINT_ID, 
                           strata= ~ STRATUM_LUCAS, 
                           weights = ~ WGT_LUCAS, 
                           self.rep.str= NULL, 
                           fpc= ~fpc, 
                           check.data= TRUE)
        ls <- find.lon.strata(des)
        if (!is.null(ls)) des <- collapse.strata(des, block.vars = ~NUTS2_24)
        
        levels(des$variables$BCK21_R)<-levels(m$BCK21_R)
        levels(des$variables$GRA18_10)<-levels(m$GRA18_10)
        levels(des$variables$FTY18_10)<-levels(m$FTY18_10)
        
        
        # known totals
        poptemp <- pop.template(data=des,
                                calmodel=   ~
                                  point_area:NUTS2_24 +
                                  point_area:(
                                    ELEV2 +
                                      BCK21_R +
                                      GRA18_10 +
                                      FTY18_10) - 1,
                                )

        # fill template with the master
        popfill <- fill.template(universe=m, template= poptemp)
        ######################################################
        # and adjust with areas:
        area_totals <- areas$area2024[substr(areas$NUTS2,1,2)==country]
        area_totals <- area_totals[!is.na(area_totals)]
        popfill[substr(colnames(popfill),1,15)=="point_area:NUTS"] <- area_totals
        sum(popfill[substr(colnames(popfill),1,15)=="point_area:NUTS"])
        ######################################################
        # calibration
        cal <- e.calibrate(design=des,
                           df.population= popfill,
                           calmodel=   ~
                             point_area:NUTS2_24 +
                             point_area:(
                               ELEV2 +
                                 BCK21_R +
                                 GRA18_10 +
                                 FTY18_10) - 1,
                           calfun= "linear",
                           bounds =   c(0.01,50)
        )
        
      }
 
    if (length(levels(s$NUTS2_24)) == 1) { 
       # design
       des <- e.svydesign(data=s, 
                         ids= ~ POINT_ID, 
                         strata= ~ STRATUM_LUCAS, 
                         weights = ~ WGT_LUCAS, 
                         self.rep.str= NULL, 
                         fpc= ~fpc, 
                         check.data= TRUE)
      ls <- find.lon.strata(des)
      if (!is.null(ls)) des <- collapse.strata(des)
      
      levels(des$variables$BCK21_R)<-levels(m$BCK21_R)
      levels(des$variables$GRA18_10)<-levels(m$GRA18_10)
      levels(des$variables$FTY18_10)<-levels(m$FTY18_10)
      # known totals for all countries except EE, LU, LV
      if (!i %in% c(8,18,19)) {
          poptemp <- pop.template(data=des,
                                  calmodel=   ~ 
                                    point_area +
                                    point_area:(
                                      ELEV2+
                                        BCK21_R+
                                        GRA18_10+
                                        FTY18_10) - 1)
          # fill with master
          
          
          popfill <- fill.template(universe=m, template= poptemp)
          popfill[1] <- sum(areas$area2024[substr(areas$NUTS2,1,2)==country],na.rm=TRUE)
          
          
          # calibration
          cal <- e.calibrate(design=des, 
                             df.population= popfill, 
                             calmodel=   ~ 
                               point_area +
                               point_area:(
                                 ELEV2+
                                   BCK21_R+
                                   GRA18_10+
                                   FTY18_10) - 1,
                             calfun= "linear",
                             bounds =   c(0.01,50)
          )
      }
          # known totals for EE, LU, LV
      if (i %in% c(8,18,19)) {
        poptemp <- pop.template(data=des,
                                calmodel=   ~ 
                                  point_area - 1)
        # fill with the master
        popfill <- fill.template(universe=m, template= poptemp)
        popfill[1] <- sum(areas$area2024[substr(areas$NUTS2,1,2)==country],na.rm=TRUE)
        # calibration
        cal <- e.calibrate(design=des, 
                           df.population= popfill, 
                           calmodel=   ~ 
                             point_area - 1,
                           calfun= "linear",
                           bounds =   c(0.01,50)
         )
        }
      }
      check.cal(cal)
      UWE(cal)
      summary(s$WGT_LUCAS)
      sum(s$WGT_LUCAS)
      sum(m$ones)
      sum(m$point_area)
      cal$prob <- cal$prob/cal$variables$point_area
      sum(weights(cal))
      summary(weights(cal))
      
      ##########################   
      # write calibrated weights
      ##########################
      df <- NULL
      df$POINT_ID <- cal$variables$POINT_ID
      df$cal_wgt <- weights(cal)
      df <- as.data.frame(df)
      filename <- paste(country,"_calibrated_wgts_2022.txt",sep="")
      write.table(df,file = file.path(direnew2, filename),sep="\t",quote=FALSE,row.names=FALSE,dec=".")
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
                                 SURVEY_LC1_2+
                                 SURVEY_LU1_2+
                                 SURVEY_LC1_3+
                                 SURVEY_LU1_3+
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
                                          SURVEY_LC1_2+
                                          SURVEY_LU1_2+
                                          SURVEY_LC1_3+
                                          SURVEY_LU1_3+
                                        settlement+
                                        fao_class_name+
                                        lue+
                                        lud,
                                        by = ~ NUTS1_24,
                                        estimator="Total",
                                        vartype=c("se","cv"),
                                        conf.int= TRUE,
                                        conf.lev= 0.95)
      
      est_LC1_LU1_NUTS2_24 <- svystatTM(cal, ~ area +
                                          SURVEY_LC1_1+
                                          SURVEY_LU1_1+
                                          SURVEY_LC1_2+
                                          SURVEY_LU1_2+
                                          SURVEY_LC1_3+
                                          SURVEY_LU1_3+
                                        settlement+
                                        fao_class_name+
                                        lue+
                                        lud,
                                        by = ~ NUTS2_24,
                                        estimator="Total",
                                        vartype=c("se","cv"),
                                        conf.int= TRUE, 
                                        conf.lev= 0.95)
      
      filename <- paste(country,'_est_LC1_LU1_2022.csv',sep="")
      write.svystat(est_LC1_LU1,file = file.path(direnew1, filename),sep=",",dec=".")
      
      # Transposed ----------------------------------------------
      est_LC1_LU1_NUTS1_24_t <- as.data.frame(t(est_LC1_LU1_NUTS1_24))
      filename <- paste(country,"_est_LC1_LU1_NUTS1_24_2022_t.csv",sep="")
      est_LC1_LU1_NUTS1_24_t$variable <- row.names(est_LC1_LU1_NUTS1_24_t)
      est_LC1_LU1_NUTS1_24_t$variable[1] <- "variable"
      write.table(est_LC1_LU1_NUTS1_24_t,file = file.path(direnew1, filename),sep=",",dec=".",
                 row.names=FALSE,col.names=FALSE,quote=FALSE)
      #-----------------------------------------------------------
      est_LC1_LU1_NUTS2_24_t <- as.data.frame(t(est_LC1_LU1_NUTS2_24))
      filename <- paste(country,"_est_LC1_LU1_NUTS2_24_2022_t.csv",sep="")
      est_LC1_LU1_NUTS2_24_t$variable <- row.names(est_LC1_LU1_NUTS2_24_t)
      est_LC1_LU1_NUTS2_24_t$variable[1] <- "variable"
      write.table(est_LC1_LU1_NUTS2_24_t,file = file.path(direnew1, filename),sep=",",dec=".",
                  row.names=FALSE,col.names=FALSE,quote=FALSE)
}
end_time <- Sys.time() 
execution_time <- end_time - start_time 
print(execution_time)
