
############# for mixed effect models
rm(list=ls())
####################
library(nlme)
library(MASS)
library(lme4)
library(jtools)
library(car)
library(optimx)
library(nlme)
library(dplyr)

#setwd("D:\\2015_DEM\\Model_data_squares")

setwd("S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\AOI_plots\\12m_bigdata")
#a<-read.csv("PLOT_Metrics1 - Copy.csv", header=T)

a<-read.csv("Biomass_merge.csv", header=T)
str(a)

subset_data1 <- a[a$Scribner >0, ]
summary(subset_data1$Scribner)
a1 <-subset_data1%>% 
  group_by(LINK) %>%
  summarize(across(1:72, mean, na.rm = TRUE))
str(a1)
summary(a1$Scribner)
sum(a1$Scribner)

write.csv(a1, file= "S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\AOI_plots\\12m_bigdata\\Scribner.csv")

a1<-read.csv("Scribner.csv", header=T)
sd(a1$Scribner)
hist(a1$Scribner)
#Training<-read.csv("S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\AOI_plots\\12m_bigdata\\Training.csv")
#Testing<-read.csv("S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\AOI_plots\\Predicting_area\\CHM1.6\\Merge.csv")
Testing<-read.csv("S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\AOI_plots\\Predicting_area\\CHM1.5\\Merge.csv")
summary(Testing)

subset_data <- Testing[Testing$zmax >=4, ]
str(subset_data)
str(subset_data)
b <- Testing %>% 
  group_by(FID) %>%
  summarize(across(1:58, mean, na.rm = TRUE))
str(b)


#a$PLOT.ID<- as.factor(a$PLOT.ID)

#str(a)
#a[c(2:67)] <- lapply(a[c(2:67)], function(x) c(scale(x)))

#a[c(3:65)] <- lapply(a[c(3:65)], function(x) c(scale(x)))
#str(a)


#### Start with first 5 quantile heights and fit the model (top-down approach)###########  .
str(a1)

Candiddate_0 <- glm(Scribner ~zmax+
                     zpcum6+ipcumzq10+ipcumzq30+
                      ipcumzq90+p1th+p2th+p3th
                    ,family = Gamma(link = "identity"),data = a1)


Candiddate_0 <- lmer(Scribner ~zmax+
                      zpcum6+ipcumzq10+ipcumzq30+
                      ipcumzq90+p1th+p2th+p3th
                    ,family = Gamma(link = "identity"),data = a1)

vif(Candiddate_0)
summary(Candiddate_0 )

plot(Candiddate_0)
b$predicted<-predict(Candiddate_0,b, type="response")

write.csv(b, file="S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\AOI_plots\\Predicting_area\\CHM1.6\\Predicted_scribner.csv")

b1<- b[b$predicted >0, ]
sd(b1$predicted)
summary(b1$predicted)
hist(b1$predicted)
par(mfrow=c(2,1))
my.breaks <- seq(0,150000, by=2000)
#ALL
hist(a$Scribner,breaks=my.breaks,xaxt = "n",col="grey", main=NULL,adj=NULL,ylab ="Frequency", xlab =NULL, ylim=c(0,400),cex.lab=1.5, cex.axis=1.65)
mtext(text="a) Ground",font=3,cex=1.25,adj=0,line=0)
abline(v = mean(a$Scribner),
       col = "black",
       lwd = 2)
abline(v = median(a$Scribner),
       col = "black",lty=2,
       lwd = 2)
#PP
hist(b1$predicted,breaks=my.breaks,col="grey", main=NULL,adj=NULL,ylab ="Frequency",xlab="Scribner",xlim=c(0,120000), ylim=c(0,300),cex.lab=1.5, cex.axis=1.65)
mtext(text="b) LiDAr prediction",font=3,cex=1.25,adj=0,line=0)
abline(v = mean(b1$predicted),
       col = "black",
       lwd = 2)
abline(v = median(b1$predicted),
       col = "black",lty=2,
       lwd = 2)
################################# for biomass ###################################
