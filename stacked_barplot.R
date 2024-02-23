rm(list=ls(all=TRUE))


# library
library(ggplot2)
data<-read.csv("S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\New folder\\DBH-Height\\PP1_predicted.csv", header=T)
data$Species<-as.factor(data$Species)
data$Height<-round(data$Height, digits = 0)
summary(data$Height)
# Create a table of proportions for each species in each one-inch height class




# Create a contingency table of frequencies for each species in each one-inch height class
frequency_table <- table(data$Species, data$Height)

# Calculate the proportion table by dividing each species count by the total count in each diameter
proportion_table <- prop.table(frequency_table, margin = 2)

# Reshape the proportion table into a data frame with columns for species, height, and proportion
proportion_df <- as.data.frame.table(proportion_table)

# Rename the columns
colnames(proportion_df) <- c("Species", "Height", "Proportion")

# View the proportion table as a data frame
print(proportion_df)

write.csv(proportion_df, file="S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\New folder\\DBH-Height\\proportiontable.csv")
# Stacked

write.csv(data, file="S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\New folder\\DBH-Height\\Oneinch_predicted.csv")





data1<-read.csv("D:\\Height_diameter\\Clean_plots1\\Clean2_round2.csv", header=T)
summary(data1)

data2<-subset(data1,Tree_Speci=="PP"|Tree_Speci=="DF"|Tree_Speci=="WF"|Tree_Speci=="IC"|Tree_Speci=="SP"|
                Tree_Speci=="HW" )
data3<-subset(data2,Region=="NSierra")
summary(data3)
data3$Tree_Speci<-as.factor(data3$Tree_Speci)
data3$height<-round(data3$height, digits = 0)
summary(data3)


# Create a contingency table of frequencies for each species in each one-inch height class
frequency_table <- table(data3$Tree_Speci, data3$height)

# Calculate the proportion table by dividing each species count by the total count in each diameter
proportion_table <- prop.table(frequency_table, margin = 2)

# Reshape the proportion table into a data frame with columns for species, height, and proportion
proportion_df <- as.data.frame.table(proportion_table)

# Rename the columns
colnames(proportion_df) <- c("Species", "Height", "Proportion")

# View the proportion table as a data frame
print(proportion_df)

write.csv(proportion_df, file="S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\New folder\\DBH-Height\\proportiontable2.csv")
# Stacked



# Stacked
ggplot(data3, aes(y=height, x=dbh)) + 
  geom_line(aes(color = Tree_Speci, linetype =Tree_Speci))+
  scale_color_manual(values = c("darkred", "steelblue", "black", "green","yellow"))

ggplot(data3,aes(x=height,fill=Tree_Speci) )+geom_bar(stat="count")
######################## Seperating columns####################
data<-read.csv("S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\AOI_plots\\Predicting_area\\CHM1.5\\Merge.csv",header=T)
str(data)
library(tidyr)
df_separated <- separate(data, geometry, into = c("c", "parenthesis", "number"), sep = "[\\(]")

# Remove leading and trailing whitespace from columns
df_separated <- lapply(df_separated, trimws)

# Print the resulting data frame
str(df_separated)
###################
write.csv(df_separated,file="S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\AOI_plots\\Predicting_area\\CHM1.5\\Merge1.csv")
str(data)