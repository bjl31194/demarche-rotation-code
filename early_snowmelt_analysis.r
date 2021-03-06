


#import data
phenology_data <- read.csv("~/Github/demarche-rotation-code/Phenology_data_2021.csv")

phenology_data <- subset(phenology_data, select = -c(X.1,X.2,X.3,X.4))

#dataframe with only flowering data, site, and plot info
flowering_data <- subset(phenology_data, select = -c(X,Y,TP,Comments,Sex,Pollination))



#remove B values and replace with zero
flowering_data[flowering_data == "B"] <- 0

#get rid of commas and sum values
for(i in 3:ncol(flowering_data)) {
  if(is.character(flowering_data[ , i])) {
    flowering_data[ , i] <- sapply(strsplit(flowering_data[ , i],","), function(x) sum(as.integer(x)))
  }
}

#get max flower numbers for each individual
flowering_data$Max <- apply(flowering_data[3:18], MARGIN =  1, FUN = max, na.rm = T)

#filter out individuals with no recorded flowers and get which dates contain max numbers of flowers
flowering_data_filtered <- flowering_data[!(flowering_data$Max==0),]
flowering_data_filtered$Peak <- apply(flowering_data_filtered,1,which.max)

# make new column containing peak flowering dates
for(i in 1:length(flowering_data_filtered$Peak)){
  flowering_data_filtered$Peak_dates[i] <- colnames(flowering_data_filtered[flowering_data_filtered$Peak[i]])
}

#make df with only control samples and early samples
flowering_data_control <- subset(flowering_data_filtered, Plot %in% c("C","CA","CB","CC"))
flowering_data_early <- subset(flowering_data_filtered, Plot %in% c("E","EA","EB","EC"))

#make new df with only peak dates and make it not classed as a list
peak_dates_control <- subset(flowering_data_control, select = c(Peak_dates))
peak_dates_control <- as.data.frame(lapply(peak_dates_control, unlist))
peak_dates_early <- subset(flowering_data_early, select = c(Peak_dates))
peak_dates_early <- as.data.frame(lapply(peak_dates_early, unlist))

#plot as frequency plot
ggplot(peak_dates_control, aes(x = Peak_dates)) +
  geom_bar(stat="count", fill = "firebrick") +
  labs(title = "Peak Flowering Dates in \n Simulated Control Plots", x = "Date of peak flower (control)", y = "Frequency") +
  theme_classic() +
  theme(text = element_text(size = 15),
        axis.text.x = element_text(size = 8, angle = 45, vjust = 1, hjust = 1), aspect.ratio = 0.8)

        ggplot(peak_dates_early, aes(x = Peak_dates)) +
          geom_bar(stat="count", fill = "cyan4") +
          labs(title = "Peak Flowering Dates in \n Simulated Early Snowmelt Plots", x = "Date of peak flower (early)", y = "Frequency") +
          theme_classic() +
          theme(text = element_text(size = 15),
                axis.text.x = element_text(size = 8, angle = 45, vjust = 1, hjust = 1), aspect.ratio = 0.8)

#plot as density graph
peak_dates_control$Treatment <- "C"

peak_dates_early$Treatment <- "E"

total_peak_dates <- bind_rows(peak_dates_control,peak_dates_early)

total_peak_dates <- replace_na(total_peak_dates, list(Treatment="E"))

total_peak_dates$Peak_dates = sub('X', '', total_peak_dates$Peak_dates)

total_peak_dates$Peak_dates <- as.Date(total_peak_dates$Peak_dates, format='%m.%d.%y')


ggplot(total_peak_dates, aes(x = Peak_dates, fill = Treatment)) +
  #geom_histogram(position = "identity", alpha = 0.5) +
  geom_density(aes(y=..count..), adjust=4, alpha = 0.5) +
  labs(title = "Peak Flowering Distributions in \n Simulated Early Snowmelt Plots vs. Control", x = "Peak Flowering Dates", y = "Frequency") +
  theme(
    plot.title = element_text(hjust = 0.4)
  )
