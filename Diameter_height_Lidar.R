rm(list=ls())
################################### Load in libraries ###############
library(dplyr)
library(tidyverse)
library(minpack.lm)
library(nlme)
library(lme4)
##########Bring in the data#########################
data_merch<-read.csv("D:\\Height_diameter\\Diameter_Merch_height\\Training.csv")
summary(data_merch)

data_height<-read.csv("D:\\Height_diameter\\Clean_plots1\\Clean2_round2.csv")
summary(data_height)

database<-read.csv("S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\AOI_plots\\Predicted_species\\Species.csv")

############## convert into feet###############################
database$height<- (database$height)*3.28084
summary(database)
############################Subset needed data################################

############### Subset the database#############################
data<-subset(database,species_pred=="Pondo")
str(data)
################################################
##########cleaning and subsetting height data for ponderosa pine SSeirra###############
clean_pp_Height<-subset(data_height,Tree_Speci=="PP"& Region=="NSierra")

################## David Hann model for Height Ponderosa pine####################################

ctrl <- nls.control(maxiter =150,tol=1.49e-08,
                    minFactor =1/2048,
                    printEval = T)


nls17_Height <- nlsLM(dbh ~((log(height-1.5) - b1) / b2)^(1/b3),
                     #start = c(b1 =6.58172, b2 =-7.58311,b3=-0.41703),#df,wf,PP
                     start = c(b1 =6.58172,b2 =-7.58311 ,b3=-0.41703),# WF_NSIERR
                     data=clean_pp_Height,
                     control=ctrl)

summary(nls17_Height)
coef(nls17_Height)
predict_dbh<-predict(nls17_Height,data)
data$dbh<-predict_dbh
summary(data)
plot(data$height~data$dbh)
##########cleaning for ponderosa pine SSierra###############

data3a<-subset(data_merch,Tree_Speci=="PP" & Region=="NSierra"  & Merch_Heig>=1)
summary(data3a)

plot(data3a$height~data3a$ Merch_Heig )


data3<-data3a %>%
  group_by(Region,PLOT_ID,Tree_Numbe,Tree_Speci,Name)%>%
  summarise(dbh=mean(dbh), height=mean(height), ratio=mean(Ratio), 
            elev=mean(elev),  Merch_Heig=mean(Merch_Heig))



model<-lm(Merch_Heig~height, data=data3)
summary(model)
plot(model)

Merch<-predict(model,data)
data$Merch<-Merch
summary(data)
write.csv(data,file="S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\AOI_plots\\Predicted_species\\PP.csv")
#####################################################################################

######################### For douglas fir####################################

############### Subset the database#############################
data5<-subset(database,species_pred=="Doug")
################################################
##########cleaning and subsetting height data for ponderosa pine SSeirra###############
clean_DF_Height<-subset(data_height,Tree_Speci=="DF"& Region=="NSierra")



#########################################################################


################## David Hann model for Height douglas fir####################################

ctrl <- nls.control(maxiter =150,tol=1.49e-08,
                    minFactor =1/2048,
                    printEval = T)


nls17_Height <- nlsLM(dbh ~((log(height-1.5) - b1) / b2)^(1/b3),
                     #start = c(b1 =6.58172, b2 =-7.58311,b3=-0.41703),#df,wf,PP
                     start = c(b1 =6.58172  , b2 =-7.58311 ,b3=-0.41703),# WF_NSIERRA
                     data=clean_DF_Height,
                     control=ctrl)

summary(nls17_Height)
coef(nls17_Height)
predicted_dbh<-predict(nls17_Height,data5)
data5$dbh<-predicted_dbh
summary(data5)
##########cleaning for douglas fir SSierra####################################################

data4a<-subset(data_merch,Tree_Speci=="DF"& Region=="NSierra"  & Merch_Heig>=1)
summary(data4a)
data_df<-data4a %>%
  group_by(Region,PLOT_ID,Tree_Numbe,Tree_Speci,Name)%>%
  summarise(dbh=mean(dbh), height=mean(height), ratio=mean(Ratio), 
            elev=mean(elev),  Merch_Heig=mean(Merch_Heig))



model_df<-lm(Merch_Heig~height, data=data_df)
summary(model_df)
plot(model_df)

Merch<-predict(model_df,data5)
data5$Merch<-Merch
write.csv(data5,file="S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\AOI_plots\\Predicted_species\\DF.csv")
#####################################################################################

############################ For white fir#####################################################
########################################################################

############### Subset the database#############################
data6<-subset(database,species_pred=="Whitefir")
################################################
##########cleaning and subsetting height data for ponderosa pine SSeirra###############
clean_WF_Height<-subset(data_height,Tree_Speci=="WF"& Region=="NSierra")


################## David Hann model for Height White fir####################################

ctrl <- nls.control(maxiter =150,tol=1.49e-08,
                    minFactor =1/2048,
                    printEval = T)


nls17_Height <- nlsLM(dbh ~((log(height - 1.5) - b1) / b2)^(1/b3),
                     #start = c(b1 =6.58172, b2 =-7.58311,b3=-0.41703),#df,wf,PP
                     start = c(b1 =6.58172  , b2 =-7.58311 ,b3=-0.41703),# WF_NSIERRA
                     data=clean_WF_Height,
                     control=ctrl)

summary(nls17_Height)
coef(nls17_Height)
predict_dbh<-predict(nls17_Height,data6)
data6$dbh<-predict_dbh
summary(data6)
##########cleaning for wite fir SSierra####################################################

data5a<-subset(data_merch,Tree_Speci=="WF"& Region=="NSierra"  & Merch_Heig>=1)
summary(data5a)
data_wf<-data5a %>%
  group_by(Region,PLOT_ID,Tree_Numbe,Tree_Speci,Name)%>%
  summarise(dbh=mean(dbh), height=mean(height), ratio=mean(Ratio), 
            elev=mean(elev),  Merch_Heig=mean(Merch_Heig))



model_wf<-lm(Merch_Heig~height, data=data_wf)
summary(model_wf)
plot(model_wf)

Merch<-predict(model_wf,data6)
data6$Merch<-Merch
write.csv(data6,file="S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\AOI_plots\\Predicted_species\\WF.csv")
#####################################################################################
############################ For Incense_cedar#####################################################
########################################################################

############### Subset the database#############################
data7<-subset(database,species_pred=="Cedar")
################################################
##########cleaning and subsetting height data for###############
clean_IC_Height<-subset(data_height,Tree_Speci=="IC"& Region=="NSierra")




################## David Hann model ####################################

ctrl <- nls.control(maxiter =150,tol=1.49e-08,
                    minFactor =1/2048,
                    printEval = T)


nls17_Height <- nlsLM(dbh ~((log(height - 1.5) - b1) / b2)^(1/b3),
                     #start = c(b1 =6.58172, b2 =-7.58311,b3=-0.41703),#df,wf,PP
                     start = c(b1 =6.58172  , b2 =-7.58311 ,b3=-0.41703),# WF_NSIERRA
                     
                     data=clean_IC_Height,
                     control=ctrl)

summary(nls17_Height)
coef(nls17_Height)
predict_dbh<-predict(nls17_Height,data7)
data7$dbh<-predict_dbh
summary(data7)
##########cleaning for ####################################################

data6a<-subset(data_merch,Tree_Speci=="IC"& Region=="NSierra"  & Merch_Heig>=1)
summary(data6a)
data_ic<-data6a %>%
  group_by(Region,PLOT_ID,Tree_Numbe,Tree_Speci,Name)%>%
  summarise(dbh=mean(dbh), height=mean(height), ratio=mean(Ratio), 
            elev=mean(elev),  Merch_Heig=mean(Merch_Heig))


plot(data_ic$Merch_Heig~data_ic$height)

model_ic<-lm(Merch_Heig~height, data=data_ic)
summary(model_ic)
plot(model_ic)

Merch<-predict(model_ic,data7)
data7$Merch<-Merch
write.csv(data7,file="S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\AOI_plots\\Predicted_species\\IC.csv")
#####################################################################################
#####################################################################################
############################ For Si=ugar pine#####################################################
########################################################################

############### Subset the database#############################
data8<-subset(database,Species=="SP")
################################################
##########cleaning and subsetting height data for ponderosa pine SSeirra###############
clean_SP_Height<-subset(data_height,Tree_Speci=="SP"& Region=="NSierra"& dbh>=6.5 & ratio>=3)

plot(clean_SP_Height$height~clean_SP_Height$dbh)
clean_SP_Height$elev<- round(clean_SP_Height$elev/100)*100
summary(clean_SP_Height)

################################# elevation for whitefir ###############
#clean_WF_Height$elev[clean_WF_Height$elev < 3400] <- "3300"			

################################################################
clean_SP_Height$elev[clean_SP_Height$elev >6800] <- "6900"
aggregate(dbh ~ elev, data = clean_SP_Height, FUN = function(x) c(mean = mean(x), sd = sd(x), 
                                                                  count=length(x)))
#########################################################################


################## David Hann model for Height White fir####################################

ctrl <- nls.control(maxiter =150,tol=1.49e-08,
                    minFactor =1/2048,
                    printEval = T)


nls17_Height <- nlme(height ~ 4.5 + exp((b1+u1)+ b2*(dbh^b3)),
                     #start = c(b1 =6.58172, b2 =-7.58311,b3=-0.41703),#df,wf,PP
                     start = c(b1 =6.58172  , b2 =-7.58311 ,b3=-0.41703),# WF_NSIERRA
                     fixed=b1+b2+b3~1,random=u1~1|elev,
                     data=clean_SP_Height,
                     control=ctrl)

summary(nls17_Height)
coef(nls17_Height)
height<-predict(nls17_Height,data8)
data8$height<-height

##########cleaning for wite fir SSierra####################################################

data7a<-subset(data_merch,Tree_Speci=="SP"& Region=="NSierra"  & Merch_Heig>=1 &
                 dbh>=6.5 & Ratio>=3.5 & dbh<60 & height<=200)
summary(data7a)
data_sp<-data7a %>%
  group_by(Region,PLOT_ID,Tree_Numbe,Tree_Speci,Name)%>%
  summarise(dbh=mean(dbh), height=mean(height), ratio=mean(Ratio), 
            elev=mean(elev),  Merch_Heig=mean(Merch_Heig))

data_sp$elev<- round(data_sp$elev/100)*100
summary(data_sp)
#data_wf$elev[data_wf$elev>7200] <- 7300
#data_wf$elev[data_wf$elev<2300] <- 2200

aggregate(dbh ~ elev, data = data_sp, FUN = function(x) c(mean = mean(x), sd = sd(x), 
                                                          count=length(x)))

plot(data_sp$Merch_Heig~data_sp$height)

model_sp<-lm(Merch_Heig~height, data=data_sp)
summary(model_sp)
plot(model_sp)

Merch<-predict(model_sp,data8)
data8$Merch<-Merch
write.csv(data8,file="D:\\Height_diameter\\Diameter_Merch_height\\Merch_Height_SP_lm.csv")
#####################################################################################

