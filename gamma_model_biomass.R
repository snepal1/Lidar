
############# for mixed effect models
rm(list=ls())
####################
library(nlme)
library(MASS)
library(lme4)
library(jtools)
library(car)
library(optimx)



#setwd("D:\\2015_DEM\\Model_data_squares")

setwd("S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\AOI_plots\\12m_bigdata")
#a<-read.csv("PLOT_Metrics1 - Copy.csv", header=T)

a<-read.csv("Biomass_merge.csv", header=T)
str(a)
summary(a$AGB.Gross)

a1 <-a%>% 
  group_by(LINK) %>%
  summarize(across(1:72, mean, na.rm = TRUE))
str(a1)
summary(a1$AGB.Gross)
write.csv(a1, file= "S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\AOI_plots\\12m_bigdata\\biomass.csv")

a1<-read.csv("biomass.csv", header=T)
sd(a1$AGB.Gross)
hist(a1$AGB.Gross)
#Training<-read.csv("S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\AOI_plots\\12m_bigdata\\Training.csv")
Testing<-read.csv("S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\AOI_plots\\Predicting_area\\CHM1.5\\Merge.csv")
str(Testing)

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

Candiddate_0 <- glm(AGB.Gross~zmax+imean+    
                      ipcumzq30+isd+             
                     p1th+p2th+p3th 
                    ,family = Gamma(link = "identity"),
                    
                    data = a1)

vif(Candiddate_0)
summary(Candiddate_0 )

plot(Candiddate_0)
b$predicted<-predict(Candiddate_0,b, type="response")

write.csv(b, file="S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\AOI_plots\\Predicting_area\\CHM1.6\\Predicted_scribner.csv")

b1<- b[b$predicted >0, ]
sd(b1$predicted)
summary(b1$predicted)
sum(b1$predicted)
hist(b1$predicted)
par(mfrow=c(2,1))
my.breaks <- seq(0,400, by=10)
#ALL
hist(a$AGB.Gross,breaks=my.breaks,xaxt = "n",col="grey", main=NULL,adj=NULL,ylab ="Frequency", xlab =NULL, ylim=c(0,400),cex.lab=1.5, cex.axis=1.65)
mtext(text="a) Ground",font=3,cex=1.25,adj=0,line=0)
abline(v = mean(a$AGB.Gross),
       col = "black",
       lwd = 2)
abline(v = median(a$AGB.Gross),
       col = "black",lty=2,
       lwd = 2)
#PP
hist(b1$predicted,breaks=my.breaks,col="grey", main=NULL,adj=NULL,ylab ="Frequency",xlab="Predicted",xlim=c(0,400), ylim=c(0,400),cex.lab=1.5, cex.axis=1.65)
mtext(text="b) LiDAr prediction",font=3,cex=1.25,adj=0,line=0)
abline(v = mean(b1$predicted),
       col = "black",
       lwd = 2)
abline(v = median(b1$predicted),
       col = "black",lty=2,
       lwd = 2)
################################# for biomass ###################################

