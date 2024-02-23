

rm(list=ls())
library(ggplot2)
library(dplyr)

#df<-read.csv("S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\AOI_plots\\Predicted_species\\DBH_Species\\Merge.csv")
#summary(df)

df<-read.csv("S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\AOI_plots\\12m_bigdata\\Tree_265.csv")
summary(df)
df$Species <-as.factor(df$Species)
data<-subset(df,Species=="PP")
data$Cruised.DBH <- as.factor(round(data$Cruised.DBH/1)*1)
avg_height <- data %>%
  group_by(Cruised.DBH) %>%
  summarize(Average_Height = mean(Cruised.Height))

# Create the table
table_data <- data.frame(DBH = avg_height$Cruised.DBH, Average_Height = avg_height$Average_Height)

# Print the table
print(table_data)

data$dbh<- as.factor(round(data$dbh/3)*3)
avg_height <- data %>%
  group_by(dbh) %>%
  summarize(Average_Height = mean(height))
dbh20<-subset(df, dbh <=20 & species_pred =="Pondo")
dbh40<-subset(df, dbh > 20 & dbh <=40 & species_pred =="Pondo")
dbh60 <-subset (df, dbh > 40 & dbh <=60 & species_pred =="Pondo")
summary(dbh80)
dbh80 <-subset (df, dbh > 60 & dbh <=80 & species_pred =="Pondo")
hist (dbh80$dbh)


df1<-read.csv("D:\\Height_diameter\\Diameter_Merch_height\\Training.csv")
summary(df1)



df1$Tree_Speci<-as.factor(df1$Tree_Speci)
data1<-subset(df1,Tree_Speci=="WF")
data1$dbh<- as.factor(round(data1$dbh/3)*3)
avg_height <- data1 %>%
  group_by(dbh) %>%
  summarize(Average_Height = mean(height))

# Create the table
table_data1 <- data.frame(DBH = avg_height$dbh, Average_Height = avg_height$Average_Height)

# Print the table
print(table_data1)

field20<- subset(df1, Tree_Speci=="PP" & dbh <=20)
field40<- subset(df1, Tree_Speci=="PP" & dbh > 20 & dbh <=40)
field60<- subset(df1, Tree_Speci=="PP" & dbh > 40 & dbh <=60)
field80<- subset(df1, Tree_Speci=="PP" & dbh > 60 & dbh <=80)
str(field)
hist (field80$dbh)

df$Tree_Speci<-as.factor(df$Tree_Speci)
summary(df)
data<-subset(df,Tree_Speci=="PP")

data$dbh<- as.factor(round(data$dbh/3)*3)
avg_height <- data %>%
  group_by(dbh) %>%
  summarize(Average_Height = mean(height))
avg_height
a<-ggplot(avg_height, aes(x = dbh, y = Average_Height)) +
  geom_bar(stat = "identity", col = "green") +
  geom_text(aes(label = round(Average_Height, 1)),
            vjust = -0.5, color = "black", size = 3.5) +

  labs(x = NULL, y = "Average Height (ft)") +
  theme_classic() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank())
a
################################## Wite fir ##########################
data1<-subset(df,Tree_Speci=="WF")

data1$dbh<- as.factor(round(data1$dbh/3)*3)
avg_height1 <- data1 %>%
  group_by(dbh) %>%
  summarize(Average_Height1 = mean(height))
avg_height1
b<-ggplot(avg_height1, aes(x = dbh, y = Average_Height1)) +
  geom_bar(stat = "identity", col = "green") +
  geom_text(aes(label = round(Average_Height1, 1)),
            vjust = -0.5, color = "black", size = 3.5) +
  labs(x = NULL, y = NULL) +
  theme_classic() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())

b
################################## Incense cedar ####################

data2<-subset(df,Tree_Speci=="IC")

data2$dbh<- as.factor(round(data2$dbh/3)*3)
avg_height2 <- data2 %>%
  group_by(dbh) %>%
  summarize(Average_Height2 = mean(height))
avg_height2
c<-ggplot(avg_height2, aes(x = dbh, y = Average_Height2)) +
  geom_bar(stat = "identity", col = "green") +
  geom_text(aes(label = round(Average_Height2, 1)),
            vjust = -0.5, color = "black", size = 3.5) +
  labs(x = "DBH (in)", y = "Average Height (Ft)") +
  theme_classic()
c
################################## Douglasfir ####################

data3<-subset(df,Tree_Speci=="DF")

data3$dbh<- as.factor(round(data3$dbh/3)*3)
avg_height3 <- data3 %>%
  group_by(dbh) %>%
  summarize(Average_Height3 = mean(height))
avg_height3
d <- ggplot(avg_height3, aes(x = dbh, y = Average_Height3)) +
  geom_bar(stat = "identity", col = "green") +
  geom_text(aes(label = round(Average_Height3, 1)),
            vjust = -0.5, color = "black", size = 3.5) +
  labs(x = "DBH (in)", y = NULL) +
  theme_classic() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank())
d
########################################### Combine the plots together ##################

library(cowplot)

grids_bs <- plot_grid(a,b, c,d, ncol = 2, align = "v")
grids_bs



############################

