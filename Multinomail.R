#################################################################################
# logistic.R
#
###############################################################################
#################  Loading the required packages
library("nnet")
library(generalhoslem)
library(mlogit)
library(car)
library(caret)
################ Compiling auxilarry variables with the the cluster analysis results###########

rm(list=ls())
###################

############# Bring in the data####################

Training<-read.csv("S:\\Lassen_data\\Lassen_Lidar\\Big_data\\Training82.csv")

Training$Species<-as.factor(Training$Species)
str(Training)

Testing<-read.csv("S:\\Lassen_data\\Lassen_Lidar\\Big_data\\Testing82.csv")

Testing$Species<-as.factor(Testing$Species)
str(Testing)
################################################################ 

############ Start with the height variables from lidar



################ stag slope-aspect transformation##########################################


########################### Scatter plot #######################



# Loading the dplyr package
#library(dplyr)

# Using sample_frac to create 70 - 30 slipt into test and train
#train <- sample_frac(datin, 0.6)
#sample_id <- as.numeric(rownames(train)) # rownames() returns character so as.numeric
#test <- datin[-sample_id,]
#str(test)
#str(train)
#############################################AWC50###################################################3
#train$cluster_ID_4<-as.factor(train$cluster_ID_4)
#train$cluster_ID_4<- relevel(train$cluster_ID_4, ref ="4")

multi<-multinom(Species~zmax,data=Training)
summary(multi)

##############calculating z-score
z <- summary(multi)$coefficients/summary(multi)$standard.errors

########calculating p-value for each coefficient
p <- (1 - pnorm(abs(z), 0, 1))*2
p


AIC(multi)

##################################

#################### perdicted percentage ################

Testing$precticed <- predict(multi, newdata = Testing[,2:52], "class")
ctable <- table(Testing$Species, Testing$precticed)
ctable
round((sum(diag(ctable))/sum(ctable))*100,2)

##################### calculating kappa########################
caret::confusionMatrix(Testing$precticed,Testing$Species)

#Homser and Lemeshow test
logitgof(train$cluster_ID_4, fitted(multi))

###########subsoil ###############
multi1<-multinom(Species~zmax+zsd,data=Training)
summary(multi1)

##############calculating z-score
z <- summary(multi1)$coefficients/summary(multi1)$standard.errors

########calculating p-value for each coefficient
p <- (1 - pnorm(abs(z), 0, 1))*2
p

AIC(multi1)

#################### perdicted percentage ################

Testing$precticed <- predict(multi1, newdata = Testing[,2:52], "class")
ctable <- table(Testing$Species, Testing$precticed)
ctable
round((sum(diag(ctable))/sum(ctable))*100,2)

##################### calculating kappa########################
caret::confusionMatrix(Testing$precticed,Testing$Species)

#####################################################Surface soil#########################
multi2<-multinom(Species~zmax+zsd+pzabov2,data=Training)
summary(multi2)

##############calculating z-score
z <- summary(multi2)$coefficients/summary(multi2)$standard.errors

########calculating p-value for each coefficient
p <- (1 - pnorm(abs(z), 0, 1))*2
p

AIC(multi2)

##################################################
#################### predicted percentage ################

Testing$precticed <- predict(multi2, newdata = Testing[,2:52], "class")
ctable <- table(Testing$Species, Testing$precticed)
ctable
round((sum(diag(ctable))/sum(ctable))*100,2)

##################### calculating kappa########################
caret::confusionMatrix(Testing$precticed,Testing$Species)

#################################### Surface stoniness###########################
multi3<-multinom(Species~zq20+zq25+zq30+zq35+zq40,data=Training)
summary(multi3)

##############calculating z-score
z <- summary(multi3)$coefficients/summary(multi3)$standard.errors

########calculating p-value for each coefficient
p <- (1 - pnorm(abs(z), 0, 1))*2
p

AIC(multi3)

##################################################
#################### perdicted percentage ################
#################### predicted percentage ################

Testing$precticed <- predict(multi3, newdata = Testing[,2:52], "class")
ctable <- table(Testing$Species, Testing$precticed)
ctable
round((sum(diag(ctable))/sum(ctable))*100,2)

##################### calculating kappa########################
caret::confusionMatrix(Testing$precticed,Testing$Species)

#################################### Surface soil###########################
multi4<-multinom(Species~zmax+zsd+pzabov2+zq20+zq25+zq30+zq35+zq40,data=Training)
summary(multi4)

##############calculating z-score
z <- summary(multi4)$coefficients/summary(multi4)$standard.errors

########calculating p-value for each coefficient
p <- (1 - pnorm(abs(z), 0, 1))*2
p

AIC(multi4)

##################################################
#################### predicted percentage ################

Testing$precticed <- predict(multi4, newdata = Testing[,2:52], "class")
ctable <- table(Testing$Species, Testing$precticed)
ctable
round((sum(diag(ctable))/sum(ctable))*100,2)

##################### calculating kappa########################
caret::confusionMatrix(Testing$precticed,Testing$Species)




############## combine AWC150 and subsoil and surface stoniness########################
multi6<-multinom(Species~zmax+zsd+pzabov2+zq20+zq25+zq30+zq35+zq40+zq50+zq65+zq70+zq75+
                   zpcum1+zpcum2+zpcum3+zpcum4+zpcum5+zpcum7+zpcum8+zpcum9+
                   imean+iskew+ikurt+ipcmz10+ipcmz30+ipcmz50+ipcmz70+ipcmz90+
                   zq80+zq85+itot+p1th+p2th+p3th+p4th+p5th+Ref+Ref_SD+
                   NDVI+NDVI_SD+GNDVI+GNDVI_SD+DEVI+DEVI_SD+
                   Con+Con_SD
                 ,data=Training)
summary(multi6)

##############calculating z-score
z <- summary(multi6)$coefficients/summary(multi6)$standard.errors

########calculating p-value for each coefficient
p <- (1 - pnorm(abs(z), 0, 1))*2
p

AIC(multi6)

##################################################
#################### predicted percentage ################

Testing$precticed <- predict(multi6, newdata = Testing[,2:52], "class")
ctable <- table(Testing$Species, Testing$precticed)
ctable
round((sum(diag(ctable))/sum(ctable))*100,2)


##################### calculating kappa########################
caret::confusionMatrix(Testing$precticed,Testing$Species)

######################################################################################
multi7<-multinom(cluster_ID_4~AWC150+Surface_st+subsoil+Surface_so,data=train)
summary(multi7)

##############calculating z-score
z <- summary(multi7)$coefficients/summary(multi7)$standard.errors

########calculating p-value for each coefficient
p <- (1 - pnorm(abs(z), 0, 1))*2
p

AIC(multi7)

##################################################
#################### perdicted percentage ################

train$precticed <- predict(multi7, newdata = train, "class")
####### from caret
caret::confusionMatrix(as.factor(train$precticed),as.factor(as.factor(train$cluster_ID_4)))
######################
ctable <- table(train$cluster_ID_4, train$precticed)
ctable
round((sum(diag(ctable))/sum(ctable))*100,2)
############################### topography ####################################3


############################################ Topography################################
##################################################################################

mult8<-multinom(cluster_ID_4~MEAN_ELE,data=train)
summary(mult8)

##############calculating z-score
z1 <- summary(mult8)$coefficients/summary(mult8)$standard.errors

########calculating p-value for each coefficient
p1 <- (1 - pnorm(abs(z1), 0, 1))*2
p1
AIC(mult8)


train$precticed <- predict(mult8, newdata = train, "class")
####### from caret
caret::confusionMatrix(as.factor(train$precticed),as.factor(as.factor(train$cluster_ID_4)))
######################
ctable <- table(train$cluster_ID_4, train$precticed)
ctable
round((sum(diag(ctable))/sum(ctable))*100,2)



###########TWI############### Topographic wetness index#################
mult9<-multinom(cluster_ID_4~MEAN_TWI,data=train)
summary(mult9)

##############calculating z-score
z <- summary(mult9)$coefficients/summary(mult9)$standard.errors

########calculating p-value for each coefficient
p <- (1 - pnorm(abs(z), 0, 1))*2
p

train$precticed <- predict(mult9, newdata = train, "class")
####### from caret
caret::confusionMatrix(as.factor(train$precticed),as.factor(as.factor(train$cluster_ID_4)))
######################
ctable <- table(train$cluster_ID_4, train$precticed)
ctable
round((sum(diag(ctable))/sum(ctable))*100,2)

#####################################SOlar insolation######################

mult10<-multinom(cluster_ID_4~MEAN_SOLAR,data=train)
summary(mult10)

##############calculating z-score
z2 <- summary(mult10)$coefficients/summary(mult10)$standard.errors

########calculating p-value for each coefficient
p2 <- (1 - pnorm(abs(z2), 0, 1))*2
p2
train$precticed <- predict(mult10, newdata = train, "class")
####### from caret
caret::confusionMatrix(as.factor(train$precticed),as.factor(as.factor(train$cluster_ID_4)))
######################
ctable <- table(train$cluster_ID_4, train$precticed)
ctable
round((sum(diag(ctable))/sum(ctable))*100,2)

###########slope transformation ###############
mult11<-multinom(cluster_ID_4~slope_prop+slope_cos +slope_sin,data=train)
summary(mult11)

##############calculating z-score
z <- summary(mult11)$coefficients/summary(mult11)$standard.errors

########calculating p-value for each coefficient
p <- (1 - pnorm(abs(z), 0, 1))*2
p
train$precticed <- predict(mult11, newdata = train, "class")
####### from caret
caret::confusionMatrix(as.factor(train$precticed),as.factor(as.factor(train$cluster_ID_4)))
######################
ctable <- table(train$cluster_ID_4, train$precticed)
ctable
round((sum(diag(ctable))/sum(ctable))*100,2)
################################ combining ###########################
mult12<-multinom(cluster_ID_4~MEAN_ELE+slope_prop+slope_cos+slope_sin,data=train)
summary(mult12)

##############calculating z-score
z <- summary(mult12)$coefficients/summary(mult12)$standard.errors

########calculating p-value for each coefficient
p <- (1 - pnorm(abs(z), 0, 1))*2
p
train$precticed <- predict(mult12, newdata = train, "class")
####### from caret
caret::confusionMatrix(as.factor(train$precticed),as.factor(as.factor(train$cluster_ID_4)))
######################
ctable <- table(train$cluster_ID_4, train$precticed)
ctable
round((sum(diag(ctable))/sum(ctable))*100,2)
############################################################################
#########################Combining############################
# Bringing in AWC
mult18<-multinom(cluster_ID_4~MEAN_ELE+MEAN_TWI+slope_prop+slope_cos+slope_sin,data=train)
summary(mult18)

##############calculating z-score
z <- summary(mult18)$coefficients/summary(mult18)$standard.errors

########calculating p-value for each coefficient
p <- (1 - pnorm(abs(z), 0, 1))*2
p
train$precticed <- predict(mult18, newdata = train, "class")
####### from caret
caret::confusionMatrix(as.factor(train$precticed),as.factor(as.factor(train$cluster_ID_4)))
######################
ctable <- table(train$cluster_ID_4, train$precticed)
ctable
round((sum(diag(ctable))/sum(ctable))*100,2)

############################### bringing in solar ###################
mult19<-multinom(cluster_ID_4~MEAN_ELE+MEAN_SOLAR+slope_prop+slope_cos+slope_sin,data=train)
summary(mult19)

##############calculating z-score
z <- summary(mult19)$coefficients/summary(mult19)$standard.errors

########calculating p-value for each coefficient
p <- (1 - pnorm(abs(z), 0, 1))*2
p
train$precticed <- predict(mult19, newdata = train, "class")
####### from caret
caret::confusionMatrix(as.factor(train$precticed),as.factor(as.factor(train$cluster_ID_4)))
######################
ctable <- table(train$cluster_ID_4, train$precticed)
ctable
round((sum(diag(ctable))/sum(ctable))*100,2)

############################# combinetopography together #####################
mult20<-multinom(cluster_ID_4~MEAN_ELE+MEAN_TWI+MEAN_SOLAR+slope_prop+slope_cos +slope_sin,data=train)
summary(mult20)

##############calculating z-score
z <- summary(mult20)$coefficients/summary(mult20)$standard.errors

########calculating p-value for each coefficient
p <- (1 - pnorm(abs(z), 0, 1))*2
p
train$precticed <- predict(mult20, newdata = train, "class")
####### from caret
caret::confusionMatrix(as.factor(train$precticed),as.factor(as.factor(train$cluster_ID_4)))
######################
ctable <- table(train$cluster_ID_4, train$precticed)
ctable
round((sum(diag(ctable))/sum(ctable))*100,2)
######################## combine aa together #####################
mult21<-multinom(cluster_ID_4~MEAN_ELE+MEAN_SOLAR+slope_prop+slope_cos +slope_sin+AWC150+Surface_st+subsoil,data=train)
summary(mult21)

##############calculating z-score
z <- summary(mult21)$coefficients/summary(mult21)$standard.errors

########calculating p-value for each coefficient
p <- (1 - pnorm(abs(z), 0, 1))*2
p
train$precticed <- predict(mult21, newdata = train, "class")
####### from caret
caret::confusionMatrix(as.factor(train$precticed),as.factor(as.factor(train$cluster_ID_4)))
######################
ctable <- table(train$cluster_ID_4, train$precticed)
ctable
round((sum(diag(ctable))/sum(ctable))*100,2)
######################################### remove AWC150 keep TWI###################
mult22<-multinom(cluster_ID_4~MEAN_ELE+MEAN_TWI++slope_prop+slope_cos +slope_sin+MEAN_SOLAR+Surface_st+subsoil,data=train)
summary(mult22)

##############calculating z-score
z <- summary(mult22)$coefficients/summary(mult22)$standard.errors

########calculating p-value for each coefficient
p <- (1 - pnorm(abs(z), 0, 1))*2
p
train$precticed <- predict(mult22, newdata = train, "class")
####### from caret
caret::confusionMatrix(as.factor(train$precticed),as.factor(as.factor(train$cluster_ID_4)))
######################
ctable <- table(train$cluster_ID_4, train$precticed)
ctable
round((sum(diag(ctable))/sum(ctable))*100,2)

################################ keep AWC150 and remove TWI
mult23<-multinom(cluster_ID_4~MEAN_ELE+MEAN_SOLAR+Surface_st+subsoil+AWC150,data=train)
summary(mult23)

##############calculating z-score
z <- summary(mult23)$coefficients/summary(mult23)$standard.errors

########calculating p-value for each coefficient
p <- (1 - pnorm(abs(z), 0, 1))*2
p
train$precticed <- predict(mult23, newdata = train, "class")
####### from caret
caret::confusionMatrix(as.factor(train$precticed),as.factor(as.factor(train$cluster_ID_4)))
######################
ctable <- table(train$cluster_ID_4, train$precticed)
ctable
round((sum(diag(ctable))/sum(ctable))*100,2)
#################################################################
############################ END#######################################



mult24<-multinom(cluster_ID_4~MEAN_ELE+MEAN_SOLAR+(MEAN_ELE*MEAN_SOLAR)+Surface_st+subsoil+AWC150+(subsoil*AWC150),data=datin)
summary(mult24)

##############calculating z-score
z <- summary(mult24)$coefficients/summary(mult24)$standard.errors

########calculating p-value for each coefficient
p <- (1 - pnorm(abs(z), 0, 1))*2
p
datin$precticed <- predict(mult24, newdata = datin, "class")
####### from caret
caret::confusionMatrix(as.factor(datin$precticed),as.factor(as.factor(datin$cluster_ID_4)))
######################
ctable <- table(train$cluster_ID_4, train$precticed)
ctable
round((sum(diag(ctable))/sum(ctable))*100,2)
#################################################################
############################ END#######################################


mult24<-multinom(cluster_ID_4~MEAN_ELE+Surface_st+subsoil+AWC150+(subsoil*AWC150),data=datin)
summary(mult24)

##############calculating z-score
z <- summary(mult24)$coefficients/summary(mult24)$standard.errors

########calculating p-value for each coefficient
p <- (1 - pnorm(abs(z), 0, 1))*2
p
datin$precticed <- predict(mult24, newdata = datin, "class")
####### from caret
caret::confusionMatrix(as.factor(datin$precticed),as.factor(as.factor(datin$cluster_ID_4)))
######################
ctable <- table(train$cluster_ID_4, train$precticed)
ctable
round((sum(diag(ctable))/sum(ctable))*100,2)
#################################################################
############################ END#######################################


mult24<-multinom(cluster_ID_4~MEAN_ELE+MEAN_SOLAR+(MEAN_ELE*MEAN_SOLAR)+Surface_st+subsoil+(MEAN_SOLAR*subsoil)+AWC150+(subsoil*AWC150),data=datin)
summary(mult24)

##############calculating z-score
z <- summary(mult24)$coefficients/summary(mult24)$standard.errors

########calculating p-value for each coefficient
p <- (1 - pnorm(abs(z), 0, 1))*2
p
datin$precticed <- predict(mult24, newdata = datin, "class")
####### from caret
caret::confusionMatrix(as.factor(datin$precticed),as.factor(as.factor(datin$cluster_ID_4)))
######################
ctable <- table(train$cluster_ID_4, train$precticed)
ctable
round((sum(diag(ctable))/sum(ctable))*100,2)
#################################################################
############################ END#######################################

mult24<-multinom(cluster_ID_4~MEAN_ELE+Surface_so+(MEAN_ELE*Surface_so)+subsoil+AWC150+(subsoil*AWC150),data=datin)
summary(mult24)

##############calculating z-score
z <- summary(mult24)$coefficients/summary(mult24)$standard.errors

########calculating p-value for each coefficient
p <- (1 - pnorm(abs(z), 0, 1))*2
p
datin$precticed <- predict(mult24, newdata = datin, "class")
####### from caret
caret::confusionMatrix(as.factor(datin$precticed),as.factor(as.factor(datin$cluster_ID_4)))
######################
ctable <- table(train$cluster_ID_4, train$precticed)
ctable
round((sum(diag(ctable))/sum(ctable))*100,2)
#################################################################
############################ END#######################################











mult24<-multinom(cluster_ID_4~MEAN_ELE+MEAN_SOLAR+(MEAN_ELE*MEAN_S





















