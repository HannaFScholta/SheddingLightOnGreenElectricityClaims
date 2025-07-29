###################################################################################################################
###################################################################################################################
############################### ALL RES - UNCOVERED INTERVALS & UNDERCOVERAGES ####################################
###################################################################################################################
###################################################################################################################

#Set correct  Working Directory
setwd("INSERTPATH")

#Load and execute packages
source("INSERTPATH/Input Files/Load Packages.R")
#install.packages("gridExtra")
library(gridExtra)

#Define the dataset names
data_type <- c("ALL RES")

#Load and execute level of analysis file
input_path <- "INSERTPATH/Input Files"
file_name <- paste0(data_type,"_All Level of Analysis.R")

file_path <- file.path(input_path, file_name)
source(file_path)

#Define Output path
output_path <- "INSERTPATH"

#############################################################################################
############################### DF CREATION & CSV STORING ###################################
#############################################################################################

###Preparation
# Create a vector of years from 2016 to 2021
years <- as.character(2016:2021)

### Share of covered intervals (decimal)
  # Create a data frame to show the share of covered intervals
  df_coveredIntervals_quantity_rel_dec <- data.frame(
    "Year Index" = c(years, "2016-21"),
    "yearly" = c(1,1,1,1,1,1,1),
    "quarterly" = c(qloa_2016_coveredIntervals_quantity_rel_dec, qloa_2017_coveredIntervals_quantity_rel_dec,qloa_2018_coveredIntervals_quantity_rel_dec,qloa_2019_coveredIntervals_quantity_rel_dec,qloa_2020_coveredIntervals_quantity_rel_dec,qloa_2021_coveredIntervals_quantity_rel_dec,qloa_2016to21_coveredIntervals_quantity_rel_dec),
    "monthly" = c(mloa_2016_coveredIntervals_quantity_rel_dec, mloa_2017_coveredIntervals_quantity_rel_dec,mloa_2018_coveredIntervals_quantity_rel_dec,mloa_2019_coveredIntervals_quantity_rel_dec,mloa_2020_coveredIntervals_quantity_rel_dec,mloa_2021_coveredIntervals_quantity_rel_dec,mloa_2016to21_coveredIntervals_quantity_rel_dec),
    "weekly" = c(wloa_2016_coveredIntervals_quantity_rel_dec, wloa_2017_coveredIntervals_quantity_rel_dec,wloa_2018_coveredIntervals_quantity_rel_dec,wloa_2019_coveredIntervals_quantity_rel_dec,wloa_2020_coveredIntervals_quantity_rel_dec,wloa_2021_coveredIntervals_quantity_rel_dec,wloa_2016to21_coveredIntervals_quantity_rel_dec),
    "daily" = c(dloa_2016_coveredIntervals_quantity_rel_dec, dloa_2017_coveredIntervals_quantity_rel_dec,dloa_2018_coveredIntervals_quantity_rel_dec,dloa_2019_coveredIntervals_quantity_rel_dec,dloa_2020_coveredIntervals_quantity_rel_dec,dloa_2021_coveredIntervals_quantity_rel_dec,dloa_2016to21_coveredIntervals_quantity_rel_dec),
    "hourly" = c(hloa_2016_coveredIntervals_quantity_rel_dec, hloa_2017_coveredIntervals_quantity_rel_dec,hloa_2018_coveredIntervals_quantity_rel_dec,hloa_2019_coveredIntervals_quantity_rel_dec,hloa_2020_coveredIntervals_quantity_rel_dec,hloa_2021_coveredIntervals_quantity_rel_dec,hloa_2016to21_coveredIntervals_quantity_rel_dec)
  )
  #Store as CSV
  output_file_name <- paste0(data_type, "_DF1.csv")
  write.csv(df_coveredIntervals_quantity_rel_dec, file.path(output_path, output_file_name), row.names = TRUE)


### Share of uncovered intervals (decimal)
  # Create a data frame to show the share of uncovered intervals
  df_uncoveredIntervals_quantity_rel_dec <- data.frame(
    "Year Index" = c(years, "2016-21"),
    "quarterly" = c(qloa_2016_uncoveredIntervals_quantity_rel_dec, qloa_2017_uncoveredIntervals_quantity_rel_dec,qloa_2018_uncoveredIntervals_quantity_rel_dec,qloa_2019_uncoveredIntervals_quantity_rel_dec,qloa_2020_uncoveredIntervals_quantity_rel_dec,qloa_2021_uncoveredIntervals_quantity_rel_dec,qloa_2016to21_uncoveredIntervals_quantity_rel_dec),
    "monthly" = c(mloa_2016_uncoveredIntervals_quantity_rel_dec, mloa_2017_uncoveredIntervals_quantity_rel_dec,mloa_2018_uncoveredIntervals_quantity_rel_dec,mloa_2019_uncoveredIntervals_quantity_rel_dec,mloa_2020_uncoveredIntervals_quantity_rel_dec,mloa_2021_uncoveredIntervals_quantity_rel_dec,mloa_2016to21_uncoveredIntervals_quantity_rel_dec),
    "weekly" = c(wloa_2016_uncoveredIntervals_quantity_rel_dec, wloa_2017_uncoveredIntervals_quantity_rel_dec,wloa_2018_uncoveredIntervals_quantity_rel_dec,wloa_2019_uncoveredIntervals_quantity_rel_dec,wloa_2020_uncoveredIntervals_quantity_rel_dec,wloa_2021_uncoveredIntervals_quantity_rel_dec,wloa_2016to21_uncoveredIntervals_quantity_rel_dec),
    "daily" = c(dloa_2016_uncoveredIntervals_quantity_rel_dec, dloa_2017_uncoveredIntervals_quantity_rel_dec,dloa_2018_uncoveredIntervals_quantity_rel_dec,dloa_2019_uncoveredIntervals_quantity_rel_dec,dloa_2020_uncoveredIntervals_quantity_rel_dec,dloa_2021_uncoveredIntervals_quantity_rel_dec,dloa_2016to21_uncoveredIntervals_quantity_rel_dec),
    "hourly" = c(hloa_2016_uncoveredIntervals_quantity_rel_dec, hloa_2017_uncoveredIntervals_quantity_rel_dec,hloa_2018_uncoveredIntervals_quantity_rel_dec,hloa_2019_uncoveredIntervals_quantity_rel_dec,hloa_2020_uncoveredIntervals_quantity_rel_dec,hloa_2021_uncoveredIntervals_quantity_rel_dec,hloa_2016to21_uncoveredIntervals_quantity_rel_dec)
  )
  #Store as CSV
  output_file_name <- paste0(data_type, "_DF2.csv")
  write.csv(df_uncoveredIntervals_quantity_rel_dec, file.path(output_path, output_file_name), row.names = TRUE)


### Average |Undercoverage| of the uncovered intervals depending on level of analysis relative to GO Demand of these intervals per year (percent; amount), relative value, not calculated by timestamp as in Excel File but calculated via SUM Shortage/SUM GO Demand
  #Create a data frame to show the avg undercoverage of the uncovered intervals
  df_uncoveredIntervals_avgUndercoverage_rel2demand_decAmount <- data.frame(
    "Year Index" = c(years, "2016-21"),
    "quarterly" = c(qloa_2016_uncoveredIntervals_avgUndercoverage_rel2demand_dec, qloa_2017_uncoveredIntervals_avgUndercoverage_rel2demand_dec,qloa_2018_uncoveredIntervals_avgUndercoverage_rel2demand_dec,qloa_2019_uncoveredIntervals_avgUndercoverage_rel2demand_dec,qloa_2020_uncoveredIntervals_avgUndercoverage_rel2demand_dec,qloa_2021_uncoveredIntervals_avgUndercoverage_rel2demand_dec,qloa_2016to21_uncoveredIntervals_avgUndercoverage_rel2demand_dec),
    "monthly" = c(mloa_2016_uncoveredIntervals_avgUndercoverage_rel2demand_dec, mloa_2017_uncoveredIntervals_avgUndercoverage_rel2demand_dec,mloa_2018_uncoveredIntervals_avgUndercoverage_rel2demand_dec,mloa_2019_uncoveredIntervals_avgUndercoverage_rel2demand_dec,mloa_2020_uncoveredIntervals_avgUndercoverage_rel2demand_dec,mloa_2021_uncoveredIntervals_avgUndercoverage_rel2demand_dec,mloa_2016to21_uncoveredIntervals_avgUndercoverage_rel2demand_dec),
    "weekly" = c(wloa_2016_uncoveredIntervals_avgUndercoverage_rel2demand_dec, wloa_2017_uncoveredIntervals_avgUndercoverage_rel2demand_dec,wloa_2018_uncoveredIntervals_avgUndercoverage_rel2demand_dec,wloa_2019_uncoveredIntervals_avgUndercoverage_rel2demand_dec,wloa_2020_uncoveredIntervals_avgUndercoverage_rel2demand_dec,wloa_2021_uncoveredIntervals_avgUndercoverage_rel2demand_dec,wloa_2016to21_uncoveredIntervals_avgUndercoverage_rel2demand_dec),
    "daily" = c(dloa_2016_uncoveredIntervals_avgUndercoverage_rel2demand_dec, dloa_2017_uncoveredIntervals_avgUndercoverage_rel2demand_dec,dloa_2018_uncoveredIntervals_avgUndercoverage_rel2demand_dec,dloa_2019_uncoveredIntervals_avgUndercoverage_rel2demand_dec,dloa_2020_uncoveredIntervals_avgUndercoverage_rel2demand_dec,dloa_2021_uncoveredIntervals_avgUndercoverage_rel2demand_dec,dloa_2016to21_uncoveredIntervals_avgUndercoverage_rel2demand_dec),
    "hourly" = c(hloa_2016_uncoveredIntervals_avgUndercoverage_rel2demand_dec, hloa_2017_uncoveredIntervals_avgUndercoverage_rel2demand_dec,hloa_2018_uncoveredIntervals_avgUndercoverage_rel2demand_dec,hloa_2019_uncoveredIntervals_avgUndercoverage_rel2demand_dec,hloa_2020_uncoveredIntervals_avgUndercoverage_rel2demand_dec,hloa_2021_uncoveredIntervals_avgUndercoverage_rel2demand_dec,hloa_2016to21_uncoveredIntervals_avgUndercoverage_rel2demand_dec)
  )
  #Display the Amount of the Undercoverage in Percent
  df_uncoveredIntervals_avgUndercoverage_rel2demand_percAmount <- df_uncoveredIntervals_avgUndercoverage_rel2demand_decAmount %>%
    mutate_at(vars(2:6), list(~ round(. * 100, 1)))
  #Store as CSV
  output_file_name <- paste0(data_type, "_DF3.csv")
  write.csv(df_uncoveredIntervals_avgUndercoverage_rel2demand_percAmount, file.path(output_path, output_file_name), row.names = TRUE)


### Summed |Undercoverage| depending on level of analysis relative to yearly GO Demand (of all intervals) per year (percent, amount) 
  #Create a data frame to show the Sum of undercoverage of uncovered intervals
  df_uncoveredIntervals_sumUndercoverage__absAmount <- data.frame(
    "Year Index" = c(years, "2016-21"),
    "quarterly" = c(qloa_2016_uncoveredIntervals_sumUndercoverage_abs, qloa_2017_uncoveredIntervals_sumUndercoverage_abs,qloa_2018_uncoveredIntervals_sumUndercoverage_abs,qloa_2019_uncoveredIntervals_sumUndercoverage_abs,qloa_2020_uncoveredIntervals_sumUndercoverage_abs,qloa_2021_uncoveredIntervals_sumUndercoverage_abs,qloa_2016to21_uncoveredIntervals_sumUndercoverage_abs),
    "monthly" = c(mloa_2016_uncoveredIntervals_sumUndercoverage_abs, mloa_2017_uncoveredIntervals_sumUndercoverage_abs,mloa_2018_uncoveredIntervals_sumUndercoverage_abs,mloa_2019_uncoveredIntervals_sumUndercoverage_abs,mloa_2020_uncoveredIntervals_sumUndercoverage_abs,mloa_2021_uncoveredIntervals_sumUndercoverage_abs,mloa_2016to21_uncoveredIntervals_sumUndercoverage_abs),
    "weekly" = c(wloa_2016_uncoveredIntervals_sumUndercoverage_abs, wloa_2017_uncoveredIntervals_sumUndercoverage_abs,wloa_2018_uncoveredIntervals_sumUndercoverage_abs,wloa_2019_uncoveredIntervals_sumUndercoverage_abs,wloa_2020_uncoveredIntervals_sumUndercoverage_abs,wloa_2021_uncoveredIntervals_sumUndercoverage_abs,wloa_2016to21_uncoveredIntervals_sumUndercoverage_abs),
    "daily" = c(dloa_2016_uncoveredIntervals_sumUndercoverage_abs, dloa_2017_uncoveredIntervals_sumUndercoverage_abs,dloa_2018_uncoveredIntervals_sumUndercoverage_abs,dloa_2019_uncoveredIntervals_sumUndercoverage_abs,dloa_2020_uncoveredIntervals_sumUndercoverage_abs,dloa_2021_uncoveredIntervals_sumUndercoverage_abs,dloa_2016to21_uncoveredIntervals_sumUndercoverage_abs),
    "hourly" = c(hloa_2016_uncoveredIntervals_sumUndercoverage_abs, hloa_2017_uncoveredIntervals_sumUndercoverage_abs,hloa_2018_uncoveredIntervals_sumUndercoverage_abs,hloa_2019_uncoveredIntervals_sumUndercoverage_abs,hloa_2020_uncoveredIntervals_sumUndercoverage_abs,hloa_2021_uncoveredIntervals_sumUndercoverage_abs,hloa_2016to21_uncoveredIntervals_sumUndercoverage_abs)
  )
  
  #Create a data frame to show the Sum of GO Demand of all intervals (damit aufgrund der teilweise in Kalenderwochen fehlerhaft anderen Jahren zugerechneten für Wochen keine falsche Jahressumme herangezogen wird, wurde hier auf monatliche SUM geswitcht; TODO: Lösen des Problems in Raw Data)
  df_allIntervals_sumGODemand_abs <- data.frame(
    "Year Index" = c(years, "2016-21"),
    "quarterly" = c(qloa_2016_allIntervals_sumGODemand_abs, qloa_2017_allIntervals_sumGODemand_abs,qloa_2018_allIntervals_sumGODemand_abs,qloa_2019_allIntervals_sumGODemand_abs,qloa_2020_allIntervals_sumGODemand_abs,qloa_2021_allIntervals_sumGODemand_abs,qloa_2016to21_allIntervals_sumGODemand_abs),
    "monthly" = c(mloa_2016_allIntervals_sumGODemand_abs, mloa_2017_allIntervals_sumGODemand_abs,mloa_2018_allIntervals_sumGODemand_abs,mloa_2019_allIntervals_sumGODemand_abs,mloa_2020_allIntervals_sumGODemand_abs,mloa_2021_allIntervals_sumGODemand_abs,mloa_2016to21_allIntervals_sumGODemand_abs),
    "weekly" = c(mloa_2016_allIntervals_sumGODemand_abs, mloa_2017_allIntervals_sumGODemand_abs,mloa_2018_allIntervals_sumGODemand_abs,mloa_2019_allIntervals_sumGODemand_abs,mloa_2020_allIntervals_sumGODemand_abs,mloa_2021_allIntervals_sumGODemand_abs,mloa_2016to21_allIntervals_sumGODemand_abs),
    "daily" = c(dloa_2016_allIntervals_sumGODemand_abs, dloa_2017_allIntervals_sumGODemand_abs,dloa_2018_allIntervals_sumGODemand_abs,dloa_2019_allIntervals_sumGODemand_abs,dloa_2020_allIntervals_sumGODemand_abs,dloa_2021_allIntervals_sumGODemand_abs,dloa_2016to21_allIntervals_sumGODemand_abs),
    "hourly" = c(hloa_2016_allIntervals_sumGODemand_abs, hloa_2017_allIntervals_sumGODemand_abs,hloa_2018_allIntervals_sumGODemand_abs,hloa_2019_allIntervals_sumGODemand_abs,hloa_2020_allIntervals_sumGODemand_abs,hloa_2021_allIntervals_sumGODemand_abs,hloa_2016to21_allIntervals_sumGODemand_abs)
  )
  
  #Multiply dataframes (only columns with num values)
  df_allIntervals_YearlyUndercoverage_rel2yearlyGODemand_percAmount <- df_uncoveredIntervals_sumUndercoverage__absAmount[,2:6] / df_allIntervals_sumGODemand_abs[,2:6] *-100
  
  #Extend by previously deleted Year Index column
  df_allIntervals_YearlyUndercoverage_rel2yearlyGODemand_percAmount <- cbind("Year Index" = c(years, "2016-21"),df_allIntervals_YearlyUndercoverage_rel2yearlyGODemand_percAmount)
  
  #Store as CSV
  output_file_name <- paste0(data_type, "_DF4.csv")
  write.csv(df_allIntervals_YearlyUndercoverage_rel2yearlyGODemand_percAmount, file.path(output_path, output_file_name), row.names = TRUE)


### Average coverage of all intervals depending on level of analysis relative to GO Demand per year (percent), relative value, not calculated by timestamp as in Excel File but calculated via SUM Coverage/SUM GO Demand
  #Create a dataframe to show the avg coverage of the intervals
  df_allIntervals_avgCoverage_rel2demand_perc <- data.frame(
    "Year Index" = c(years, "2016-21"),
    "quarterly" = c(qloa_2016_allIntervals_avgCoverage_rel2demand_dec, qloa_2017_allIntervals_avgCoverage_rel2demand_dec,qloa_2018_allIntervals_avgCoverage_rel2demand_dec,qloa_2019_allIntervals_avgCoverage_rel2demand_dec,qloa_2020_allIntervals_avgCoverage_rel2demand_dec,qloa_2021_allIntervals_avgCoverage_rel2demand_dec,qloa_2016to21_allIntervals_avgCoverage_rel2demand_dec),
    "monthly" = c(mloa_2016_allIntervals_avgCoverage_rel2demand_dec, mloa_2017_allIntervals_avgCoverage_rel2demand_dec,mloa_2018_allIntervals_avgCoverage_rel2demand_dec,mloa_2019_allIntervals_avgCoverage_rel2demand_dec,mloa_2020_allIntervals_avgCoverage_rel2demand_dec,mloa_2021_allIntervals_avgCoverage_rel2demand_dec,mloa_2016to21_allIntervals_avgCoverage_rel2demand_dec),
    "weekly" = c(wloa_2016_allIntervals_avgCoverage_rel2demand_dec, wloa_2017_allIntervals_avgCoverage_rel2demand_dec,wloa_2018_allIntervals_avgCoverage_rel2demand_dec,wloa_2019_allIntervals_avgCoverage_rel2demand_dec,wloa_2020_allIntervals_avgCoverage_rel2demand_dec,wloa_2021_allIntervals_avgCoverage_rel2demand_dec,wloa_2016to21_allIntervals_avgCoverage_rel2demand_dec),
    "daily" = c(dloa_2016_allIntervals_avgCoverage_rel2demand_dec, dloa_2017_allIntervals_avgCoverage_rel2demand_dec,dloa_2018_allIntervals_avgCoverage_rel2demand_dec,dloa_2019_allIntervals_avgCoverage_rel2demand_dec,dloa_2020_allIntervals_avgCoverage_rel2demand_dec,dloa_2021_allIntervals_avgCoverage_rel2demand_dec,dloa_2016to21_allIntervals_avgCoverage_rel2demand_dec),
    "hourly" = c(hloa_2016_allIntervals_avgCoverage_rel2demand_dec, hloa_2017_allIntervals_avgCoverage_rel2demand_dec,hloa_2018_allIntervals_avgCoverage_rel2demand_dec,hloa_2019_allIntervals_avgCoverage_rel2demand_dec,hloa_2020_allIntervals_avgCoverage_rel2demand_dec,hloa_2021_allIntervals_avgCoverage_rel2demand_dec,hloa_2016to21_allIntervals_avgCoverage_rel2demand_dec)
  )
  #Display the Amount of the Undercoverage in Percent
  df_allIntervals_avgCoverage_rel2demand_perc <- df_allIntervals_avgCoverage_rel2demand_perc %>%
    mutate_at(vars(2:6), list(~ round(. * 100, 1)))
  #Store as CSV
  output_file_name <- paste0(data_type, "_DF5.csv")
  write.csv(df_allIntervals_avgCoverage_rel2demand_perc, file.path(output_path, output_file_name), row.names = TRUE)


#############################################################################################
############################### DF CREATION & CSV STORING ###################################
#############################################################################################

####################### INDIVIDUAL PLOTS PER YEAR ###########################
#############################################################################

# Set the target folder path
target_folder <- "INSERTPATH"


#Plot share of green electricity (save as png; y-axis from 0 to 100%)
# Iterate through each row of the DataFrame without 2016-21
df_coveredIntervals_quantity_rel_dec_reduced <- df_coveredIntervals_quantity_rel_dec %>%
  slice(1:(n()-1))


for (i in 1:nrow(df_coveredIntervals_quantity_rel_dec_reduced)) {
  row_data <- df_coveredIntervals_quantity_rel_dec_reduced[i, ]
  plot_title <- row_data[[1]]  # Get the name of the plot from the first column
  row_data <- row_data[-1]  # Remove the first column
  
  # Convert the row data to a data frame with appropriate column names
  row_data_df <- data.frame(
    x = as.character(colnames(row_data)),
    y = as.numeric(row_data)
  )
  
  # Shift x-axis values to make "yearly" intercept at x = 0
  yc_value <- row_data_df[row_data_df$x == "yearly", "y"]
  row_data_df$x <- factor(row_data_df$x, levels = row_data_df$x)
  row_data_df$x <- factor(row_data_df$x, levels = c("yearly", levels(row_data_df$x)[-which(levels(row_data_df$x) == "yearly")]))
  
  
  # Create the line plot for the current row
  p <- ggplot(row_data_df, aes(x = as.integer(x), y = y*100)) +  # Display y-axis in percent
    geom_ribbon(aes(ymin = 0, ymax = y * 100), fill = "green")+
    geom_ribbon(aes(ymax = 100, ymin = y * 100), fill = "orange")+
    geom_line() +
    geom_point() +
    labs(title = plot_title, x = "Level of Analysis", y = "Covered Intervals (in %)") +  # Update y-axis label
    scale_x_continuous(breaks = as.integer(row_data_df$x), labels = levels(row_data_df$x), expand = expansion(add = c(0, 0.03))) +
    scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100), expand = expansion(add = c(0, 1))) +  # Adjust y-axis limits
    theme(axis.line.x = element_line(size = 0.5, color = "black")) +
    theme(axis.line.y = element_line(size = 0.5, color = "black")) +  # Adjust y-axis line size
    theme(axis.title.x = element_text(size = 12)) +  # Adjust x-axis title size
    theme(axis.title.y = element_text(size = 12)) +  # Adjust y-axis title size
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +  # Customize gridline
    coord_cartesian(ylim = c(0, 100))+   # Adjust y-axis limits for display
    theme(plot.margin = margin(1, 1, 1.5, 1, "cm"), 
          panel.spacing.y = unit(1, "lines"),
          plot.background = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "white", color = NA))
  
  # Add y-value labels using geom_text
  p <- p + geom_text(data = subset(row_data_df, y < 1), aes(label = paste0(round(y * 100), "%")), vjust = 1.5, size = 3, color = "black", hjust = 1)
  
  # Print or display the plot
  print(p)
  
  # Specify the full file path for saving
  file_path <- file.path(target_folder, paste0(data_type,"_",plot_title, ".png"))
  
  # Increase plot resolution
  ggsave(filename = file_path, plot = p, width = 10, height = 5, dpi = 300)
}

######################### ONE PLOT FOR ALL YEARS ############################
#############################################################################

#Plot share of green electricity (save as png; y-axis from 0 to 100%)

# Create a list to store individual plots
plots_list <- list()

# Iterate through each row of the DataFrame and store the plots in the list
for (i in 1:nrow(df_coveredIntervals_quantity_rel_dec_reduced)) {
  row_data <- df_coveredIntervals_quantity_rel_dec_reduced[i, ]
  plot_title <- row_data[[1]]  # Get the name of the plot from the first column
  row_data <- row_data[-1]  # Remove the first column
  
  # Convert the row data to a data frame with appropriate column names
  row_data_df <- data.frame(
    x = as.character(colnames(row_data)),
    y = as.numeric(row_data)
  )
  
  # Shift x-axis values to make "yearly" intercept at x = 0
  yc_value <- row_data_df[row_data_df$x == "yearly", "y"]
  row_data_df$x <- factor(row_data_df$x, levels = row_data_df$x)
  row_data_df$x <- factor(row_data_df$x, levels = c("yearly", levels(row_data_df$x)[-which(levels(row_data_df$x) == "yearly")]))
  
  # Create the line plot for the current row
  p <- ggplot(row_data_df, aes(x = as.integer(x), y = y*100)) +  # Display y-axis in percent
    geom_ribbon(aes(ymin = 0, ymax = y * 100), fill = "green")+
    geom_ribbon(aes(ymax = 100, ymin = y * 100), fill = "orange")+
    geom_line() +
    geom_point() +
    labs(title = plot_title, x = "Level of Analysis", y = "Covered Intervals (in %)") +  # Update y-axis label
    scale_x_continuous(breaks = as.integer(row_data_df$x), labels = levels(row_data_df$x), expand = expansion(add = c(0, 0.03))) +
    scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100), expand = expansion(add = c(0, 1))) +  # Adjust y-axis limits
    theme(axis.line.x = element_line(size = 0.5, color = "black")) +
    theme(axis.line.y = element_line(size = 0.5, color = "black")) +  # Adjust y-axis line size
    theme(axis.title.x = element_text(size = 12)) +  # Adjust x-axis title size
    theme(axis.title.y = element_text(size = 12)) +  # Adjust y-axis title size
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +  # Customize gridline
    coord_cartesian(ylim = c(0, 100))+   # Adjust y-axis limits for display
    theme(plot.margin = margin(1, 1, 1.5, 1, "cm"), 
          panel.spacing.y = unit(1, "lines"),
          plot.background = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "white", color = NA))

  # Add y-value labels using geom_text
  p <- p + geom_text(data = subset(row_data_df, y < 1), aes(label = paste0(round(y * 100), "%")), vjust = 1.5, size = 3, color = "black", hjust = 1)
  
  # Store the individual plot in the list
  plots_list[[i]] <- p
}

# Arrange and display all individual plots in a single graph
grid.arrange(grobs = plots_list, ncol = 2)  # Adjust the number of columns as needed

# Arrange all individual plots in a single graph
combined_plot <- grid.arrange(grobs = plots_list, ncol = 2)  # Adjust the number of columns as needed

# Save the combined graph in high resolution
output_file_name <- paste0(data_type, "_combined plot.png")
ggsave(file.path(output_path, output_file_name), plot = combined_plot, width = 15, height = 15, dpi = 300)# Save the plot as a high-resolution image
