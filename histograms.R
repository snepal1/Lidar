# get list of all csv files in directory
csv_files <- list.files(path = "D:\\LiDAr_viewer\\Alex_data\\ground\\normal1", pattern = "*.csv", full.names = TRUE)

# load each csv file into a list of data frames and assign column names
df_list <- lapply(csv_files, function(x) {
  df <- read.csv(x, header = FALSE)
  colnames(df) <- unlist(df[1,])
  df <- df[-1,] # remove first row after assigning column names
  return(df)
})

# combine all data frames into one
final_df <- do.call(rbind, df_list)
final_df$zmean<-as.numeric(final_df$zmean)
final_df$area<-as.numeric (final_df$area)
str(final_df)

##################### render the table ################


######Draw histograms for the height and density metrics############################################

par(mfrow=c(2,1))
hist(final_df$zmean, ylab = "Frequency", xlab = "Height (m)", main=NULL,
     cex.lab=1.65)
abline(v = mean(final_df$zmean),
       col = "red",
       lwd = 2)
abline(v = median(final_df$zmean),
       col = "black",lty=2,
       lwd = 2)




hist(final_df$area, ylab = "Frequency", xlab = "Crown_area (Sq.m)", main=NULL,
     cex.lab=1.65)
abline(v = mean(final_df$area),
       col = "red",
       lwd = 2)
abline(v = median(final_df$area),
       col = "black",lty=2,
       lwd = 2)

######################################################################
