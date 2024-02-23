# Sample data frames
df1 <- read.csv("S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\AOI_plots\\AOI_12\\CHM1\\Merge.csv")
str(df1)

df2 <- read.csv("S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\AOI_plots\\12m_bigdata\\Biomass.csv")


# Merge data frames based on ID
merged_df <- merge(df1, df2, by = "LINK", all.x = TRUE)
str(merged_df)
# Print the merged data frame
print(merged_df)
hist(merged_df$AGB.Net)
write.csv(merged_df ,file="S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\AOI_plots\\12m_bigdata\\Biomass_merge.csv")
