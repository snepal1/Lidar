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

database<-read.csv("D:\\Height_diameter\\Diameter_Merch_height\\test.csv")

############################Subset needed data################################

############### Subset the database#############################
data<-subset(database,Species=="PP")
################################################
##########cleaning and subsetting height data for ponderosa pine SSeirra###############
clean_pp_Height<-subset(data_height,Tree_Speci=="PP"& Region=="NSierra")
################################################################

################## David Hann model for Height Ponderosa pine####################################

ctrl <- nls.control(maxiter =150,tol=1.49e-08,
                    minFactor =1/2048,
                    printEval = T)


nls17_Height <- nlsLM(height ~ 4.5 + exp((b1)+ b2*(dbh^b3)),
                     #start = c(b1 =6.58172, b2 =-7.58311,b3=-0.41703),#df,wf,PP
                     start = c(b1 =6.58172  , b2 =-7.58311 ,b3=-0.41703),# WF_NSIERRA
                     data=clean_pp_Height,
                     control=ctrl)

summary(nls17_Height)
coef(nls17_Height)
height<-predict(nls17_Height,data)
data$height<-height
write.csv(data, file="S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\New folder\\DBH-Height\\PP_predicted.csv")

######################### For douglas fir####################################

############### Subset the database#############################
data5<-subset(database,Species=="DF")
################################################
##########cleaning and subsetting height data for ponderosa pine SSeirra###############
clean_DF_Height<-subset(data_height,Tree_Speci=="DF"& Region=="NSierra")

################## David Hann model for Height douglas fir####################################

ctrl <- nls.control(maxiter =150,tol=1.49e-08,
                    minFactor =1/2048,
                    printEval = T)


nls17_Height <- nlsLM(height ~ 4.5 + exp((b1)+ b2*(dbh^b3)),
                     #start = c(b1 =6.58172, b2 =-7.58311,b3=-0.41703),#df,wf,PP
                     start = c(b1 =6.58172  , b2 =-7.58311 ,b3=-0.41703),# WF_NSIERRA
                     
                     data=clean_DF_Height,
                     control=ctrl)

summary(nls17_Height)
coef(nls17_Height)
height<-predict(nls17_Height,data5)
data5$height<-height

write.csv(data5, file="S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\New folder\\DBH-Height\\DF_predicted.csv")



############### Subset the database#############################
data6<-subset(database,Species=="WF")
################################################
##########cleaning and subsetting height data for ponderosa pine SSeirra###############
clean_WF_Height<-subset(data_height,Tree_Speci=="WF"& Region=="NSierra")


################## David Hann model for Height White fir####################################

ctrl <- nls.control(maxiter =150,tol=1.49e-08,
                    minFactor =1/2048,
                    printEval = T)


nls17_Height <- nlsLM(height ~ 4.5 + exp((b1)+ b2*(dbh^b3)),
                     #start = c(b1 =6.58172, b2 =-7.58311,b3=-0.41703),#df,wf,PP
                     start = c(b1 =6.58172  , b2 =-7.58311 ,b3=-0.41703),# WF_NSIERRA
                     #fixed=b1+b2+b3~1,random=u1~1|elev,
                     data=clean_WF_Height,
                     control=ctrl)

summary(nls17_Height)
coef(nls17_Height)
height<-predict(nls17_Height,data6)
data6$height<-height

write.csv(data6, file="S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\New folder\\DBH-Height\\WF_predicted.csv")


#####
############################ For Incense_cedar#####################################################
########################################################################

############### Subset the database#############################
data7<-subset(database,Species=="IC")
################################################
##########cleaning and subsetting height data for###############
clean_IC_Height<-subset(data_height,Tree_Speci=="IC"& Region=="NSierra")

##########################################################


################## David Hann model ####################################

ctrl <- nls.control(maxiter =150,tol=1.49e-08,
                    minFactor =1/2048,
                    printEval = T)


nls17_Height <- nlsLM(height ~ 4.5 + exp((b1)+ b2*(dbh^b3)),
                     #start = c(b1 =6.58172, b2 =-7.58311,b3=-0.41703),#df,wf,PP
                     start = c(b1 =6.58172  , b2 =-7.58311 ,b3=-0.41703),# WF_NSIERRA
                     #fixed=b1+b2+b3~1,random=u1~1|elev,
                     data=clean_IC_Height,
                     control=ctrl)

summary(nls17_Height)
coef(nls17_Height)
height<-predict(nls17_Height,data7)
data7$height<-height

write.csv(data7, file="S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\New folder\\DBH-Height\\IC_predicted.csv")
######################################################################################