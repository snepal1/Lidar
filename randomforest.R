rm(list=ls())
###################
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(partykit)
library(party)
library(randomForest)
library(e1071)
library(caret)
#data<-read.csv("S:\\Lassen_data\\Lassen_Lidar\\Big_data\\All_Species.csv")
#str(data)

#data1<- data[,1:19]
#str(data)
######## Split the data into test and training set

#index<-sample(2, nrow(data), replace=TRUE, prob=c(0.70, 0.30))


##### training data
#Training<-data[index==1,]
#str(Training)
#write.csv(Training, "S:\\Lassen_data\\Lassen_Lidar\\Big_data\\Training78.csv")
######## test data
#Testing<-data[index==2,]
#write.csv(Testing, "S:\\Lassen_data\\Lassen_Lidar\\Big_data\\Testing78.csv")
#str(Testing)
##

####################### Bring in trainign and test data
#Training<-read.csv("S:\\Lassen_data\\Lassen_Lidar\\Big_data\\Training82.csv")
Training<-read.csv("S:\\Lassen_data\\Lassen_Lidar\\Big_data\\All_Species.csv")
Testing<-read.csv("S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\AOI_plots\\NDVI_extracted_pred\\Merge_1.csv")
summary(Training)
str(Testing)
species<- read.csv("S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\AOI_plots\\Predicted_species\\Species_percentage.csv")

Training[c(2:67)] <- lapply(Training[c(2:67)], function(x) c(scale(x)))

Testing[c(2:66)] <- lapply(Testing[c(2:66)], function(x) c(scale(x)))

species$Tree.Species<-as.factor(species$Tree.Species)
summary(species)

library(dplyr)

# Assuming 'df' is your data frame with the given data

data<-species %>%
  group_by(Tree.Species) %>%
  summarize(Percentage = n() / nrow(Tree.Species) * 100)

#Testing1 <- na.omit(Testing0)
summary(data)

#Training<- as.data.frame(scale(data1[,2:68]))
#str(Training)
#write.csv(Training, "S:\\Lassen_data\\Lassen_Lidar\\Big_data\\Training1.csv")
#Testing<- as.data.frame(scale(data2[,2:68]))
#str(Testing)
#write.csv(Testing, "S:\\Lassen_data\\Lassen_Lidar\\Big_data\\Testing1.csv")
 mytree1 <- rpart(
  as.factor(Species)~zmax+zsd+pzabov2+zq25+zq35+zq50+zq70+zq75+
    zq80+zq90+zq40+zpcum1+zpcum3+zpcum5+zpcum7+zpcum8+
    itot+imean+iskew+ipcmz10+ipcmz30+ipcmz90+
    p4th+p5th+Ref+Ref_SD+NDVI_SD+GNDVI+DEVI+DEVI_SD+Con+Con_SD, 
  data = Training, 
  method="class",
  minsplit = 4, 
  minbucket = 1
)

rpart.plot(mytree1,cex=1,
          yesno.yshift =1,fallen.leaves = FALSE,extra =100)


############# remofe the variable from testing################
Testing = subset(Testing, select = -c(zq95,zq85,zq65,zq30,zpcum2,ikurt,p1th,
                                       p2th,GNDVI_SD,NDVI,p3th,ipcmz70,ipcmz50,
                                       zpcum9,zpcum4,zq20) )
############### do the prediction############################################
species_pred<-predict(mytree1,Testing, type="class")
Testing$species_pred<-species_pred
str(Testing)
table(Testing$species_pred)
write.csv(Testing,file="S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\AOI_plots\\Predicted_species\\Species.csv")

#View(Testing)
######## confusion matrix
cfm=table(Testing$Species,Testing$species_pred)
cfm
### model aacuracy or classification accuracy
classification_accuracy=sum(diag(cfm)/sum(cfm))
classification_accuracy

################## calculate the cohens kappa ##########################
caret::confusionMatrix(as.factor(Testing$species_pred),as.factor(Testing$Species),mode = "prec_recall")


############### Using random forest for cost 10#################
#rfm<-randomForest(as.factor(Species)~zmax+zsd+pzabov2+zq20+zq25+zq35+zq50+zq70+zq75+
                    #zq80+zq90+zq40+zpcum1+zpcum3+zpcum4+zpcum5+zpcum7+zpcum8+zpcum9+
                    #itot+imean+iskew+ipcmz10+ipcmz30+ipcmz50+ipcmz70+ipcmz90+
                    #p3th+p4th+p5th+Ref+Ref_SD+NDVI_SD+GNDVI+DEVI+DEVI_SD+
                    #Con, data=Training)
#Testing = subset(Testing1, select = -c(zq95,zq85,zq65,zq30,zpcum2,ikurt,p1th,
                                       #p2th,GNDVI_SD,Con_SD,NDVI) )
set.seed(1)
rfm<-randomForest(as.factor(Species)~zmax+zsd+zq50+zq25+
                  zq90+zpcum3+zpcum5+zq80+ipcmz10+
                    itot+imean+iskew+ikurt+
                    p4th+Ref+Ref_SD+NDVI+NDVI_SD+GNDVI+Con+Con_SD, data=Training)

Testing = subset(Testing, select = -c(zq95,zq85,zq65,zq30,zpcum2,ikurt,p1th,
                                       p2th,GNDVI_SD,NDVI,p3th,ipcmz70,ipcmz50,
                                       zpcum9,zpcum4,zq20) )

varImpPlot(rfm) 




######## evaluate accuracy
species_pred<-predict(rfm, Testing)
Testing$species_pred<-species_pred
table(Testing$species_pred)

write.csv(Testing,file="S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\AOI_plots\\Predicted_species\\Species.csv")
#View(Testing)
######## confusion matrix
cfm=table(Testing$Species,Testing$species_pred)
cfm
### model accuracy or classification accuracy
classification_accuracy=sum(diag(cfm)/sum(cfm))
classification_accuracy

################## calculate the Cohen kappa ##########################
caret::confusionMatrix(as.factor(Testing$species_pred),as.factor(Testing$Species),mode = "prec_recall")

################################## Support vector machine classification ##############################
##### for linear kernel it was a trash job, the best line that I could fit for support vectors
#was the radial kernel with the lower cost without over fitting the data

######### variables that can be eliminated ###########
#zq85+zq95+zq65+zq30+zpcum2++ikurt+p1th+p2th+
#GNDVI_SD+Con_SD+NDVI+p3th+ipcmz70+ipcmz50+zpcum9++zpcum4+zq20
#################### support vecotr machine fit ###############
svmfit<-svm(as.factor(Species)~zmax+zsd+pzabov2+zq25+zq35+zq50+zq70+zq75+
              zq80+zq90+zq40+zpcum1+zpcum3+zpcum5+zpcum7+zpcum8+
              itot+imean+iskew+ipcmz10+ipcmz30+ipcmz90+
          p4th+p5th+Ref+Ref_SD+NDVI_SD+GNDVI+DEVI+DEVI_SD,data=Training, kernel="radial",cost=9.5,scale=TRUE)
print(svmfit)

############# remofe the variable from testing################
Testing = subset(Testing, select = -c(zq95,zq85,zq65,zq30,zpcum2,ikurt,p1th,
                                       p2th,GNDVI_SD,NDVI,p3th,ipcmz70,ipcmz50,
                                       zpcum9,zpcum4,zq20) )
#tuned<-tune(svm, as.factor(Species)~., data=Training, kernel="radial",ranges=list(cost=c(0.001,0.01,.1,1,10,100)))
#summary(tuned)
species_pred<-predict(svmfit,Testing, type="class")
Testing$species_pred<-species_pred
table(species_pred)
#View(Testing)
######## confusion matrix
cfm=table(Testing$Species,Testing$species_pred)
cfm
    ### model aacuracy or classification accuracy
classification_accuracy=sum(diag(cfm)/sum(cfm))
classification_accuracy

caret::confusionMatrix(as.factor(Testing$species_pred),as.factor(Testing$Species),mode = "prec_recall")
###################################################################################################################
####################################################################################################
