###################################################################################################################
###################################################################################################################
########################################### HLOA - DISTRIBUTIONS OVER HOURS #######################################
###################################################################################################################
###################################################################################################################

#Set correct Working Directory
setwd("C:/Users/ga58qeg/OneDrive - TUM/Shedding Light on Green Claims - Supplementary Data/3_Data Analysis/2_R Results")
getwd()

#Install packages
#install.packages("dplyr")
#install.packages("readxl")
#install.packages("ggplot2")
#install.packages("tidyr")

#Load and execute packages
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)

# Define the dataset names
data_types <- c("ALL RES", "Solar", "Wind", "Solar&Wind")

for (data_type in data_types) {
  # Define the input and output path as well as file name
  input_path <- "C:/Users/ga58qeg/OneDrive - TUM/Shedding Light on Green Claims - Supplementary Data/2_entso-e & AIB Data Merging & Coverage Calculation/"
  file_name <- paste0("20240627 Data Merging & Coverage Calculation_", data_type, "_vPub.xlsx")
  output_path <- "C:/Users/ga58qeg/OneDrive - TUM/Shedding Light on Green Claims - Supplementary Data/3_Data Analysis/2_R Results"
  
  #Read in hourly_coverage file
  file_path <- file.path(input_path, file_name)
  hloa <- read_excel(file_path, sheet = "hourly_coverages")
  #View(hloa)
  
  #############################################################################################
  ####################################### DATA CLEANING #######################################
  #############################################################################################
  
  #Data Clean und Value formatting (as numeric) of hourly coverages hloa
  hloa_cleaned <- hloa %>% 
    filter(!is.na(.[,7])) #Remove all rows that show NA value in col 7
  hloa <- hloa_cleaned
  colnames (hloa)[2:5] <- c("GO Demand", "GO Supply", "Coverage", "Coverage/GO Demand") #Name cols 2-5
  hloa <- hloa %>%
    rename(
      "Timestamp" = "Hourly Coverages" #Rename Column "Hourly Coverages" to "Timestamp"
    )
  colnames (hloa)[7:8] <- c("Hour Index", "Year Index") #Name col 7 & 8
  hloa <- hloa[-1,] #Remove first row
  hloa <- hloa %>%
    select(-6,-9,-10) #Remove cols 6,9 & 10
  hloa <- hloa %>%
    select(1,7,6,2:5) #Change order of cols
  hloa <- hloa %>%
    mutate("Month Index" = substr(Timestamp,6,7)) #Add a new col, extracting the 6th until th 7th character of Timestamp
  hloa <- hloa %>%
    select(1,2,8,3:7) #Change order of cols
  
  hloa <- hloa %>%
    mutate_at(vars(5:8), as.numeric) #Ensure that Values in col 5-8 are numeric
  
  #############################################################################################
  ####################################### DATA ANALYSIS #######################################
  ############################################################################################# 
  
  ########################### SHARES OF COVERED & UNCOVERED INTERVALS #########################
  
  ### Amount of uncovered hours by hour of day and year (absolute)
    hloa_uncoveredIntervals_quantity_byHourOfDayandYear_abs <- hloa %>%
      group_by(`Hour Index`, `Year Index`) %>% #Group data frame by two variables
      summarize(NegativeCount = sum(Coverage <0, na.rm = TRUE)) %>% #Within each group defined by Hour Index and Year Index, sum up values less than 0; store them in new col "Negative Count"; (na.rm = TRUE --> ignore NAs)
      pivot_wider(names_from = `Year Index`, values_from = NegativeCount, values_fill = 0) #reshape data from long to wide format; creating new columns for each unique value of "Year Index" and fill with negative count values (missing combinations filled with 0)
    
    hloa_uncoveredIntervals_quantity_byHourOfDayandYear_abs <- hloa_uncoveredIntervals_quantity_byHourOfDayandYear_abs %>%
      mutate_at(vars(2:7), as.numeric) #Ensure that Values in col 2-7 are numeric 
    
    hloa_uncoveredIntervals_quantity_byHourOfDayandYear_abs_num <- hloa_uncoveredIntervals_quantity_byHourOfDayandYear_abs %>%
      ungroup() %>%  #Remove any existing grouping
      select(2:7) #Only keep col 2-7
    
    hloa_uncoveredIntervals_quantity_byHourOfDayandYear_abs_num <- hloa_uncoveredIntervals_quantity_byHourOfDayandYear_abs_num %>%
      mutate(`2016-2021` = rowSums(., na.rm = TRUE))#Create new col, in which sums of rows are stored
    
    result <- hloa_uncoveredIntervals_quantity_byHourOfDayandYear_abs %>%
      select(-2:-7) %>%  #Remove columns 2 to 7
      bind_cols(select(hloa_uncoveredIntervals_quantity_byHourOfDayandYear_abs_num, 1:6)) %>% #Select columns 1 to 7 from hloa_uncoveredIntervals_quantity_byHourOfDayandYear_abs_num (nicht gruppierte Variante)
      bind_cols(select(hloa_uncoveredIntervals_quantity_byHourOfDayandYear_abs_num,7)) #Add 2016-21 Col
    
    hloa_uncoveredIntervals_quantity_byHourOfDayandYear_abs <- result #Assign results back to hloa_uncoveredIntervals_quantity_byHourOfDayandYear_abs
    
    #View(hloa_uncoveredIntervals_quantity_byHourOfDayandYear_abs)
    
    output_file_name <- paste0(data_type, "_DF6.csv")
    write.csv(hloa_uncoveredIntervals_quantity_byHourOfDayandYear_abs, file.path(output_path, output_file_name), row.names = TRUE)
    
  
  ### Amount of intervals by hour of day and year (absolute) & Amount of uncovered intervals relative to amount of all intervals by hour of day and year
    #Determine amount of intervals per hour of day and year
    hloa_allIntervals_quantity_byHourOfDayandYear_abs <- hloa %>% 
      group_by(`Year Index`, `Hour Index`) %>%
      count() #Determin amount of intervals per group 
    
    hloa_allIntervals_quantity_byHourOfDayandYear_abs <- hloa_allIntervals_quantity_byHourOfDayandYear_abs %>%
      rename("Total Intervals" = "n") #Rename col "n" 
    
    hloa_allIntervals_quantity_byHourOfDayandYear_abs <- hloa_allIntervals_quantity_byHourOfDayandYear_abs %>%
      pivot_wider(names_from = `Year Index`, values_from = `Total Intervals`) #reshape data from long to wide format; creating new columns for each unique value of "Year Index" and fill with negative count values
    
    hloa_allIntervals_quantity_byHourOfDayandYear_abs_num <- hloa_allIntervals_quantity_byHourOfDayandYear_abs %>%
      ungroup() %>% #remove any existing grouping
      select(2:7) #select col 2-7
    
    hloa_allIntervals_quantity_byHourOfDayandYear_abs_num <- hloa_allIntervals_quantity_byHourOfDayandYear_abs_num %>%
      mutate(`2016-2021` = rowSums(., na.rm = TRUE)) #Create new col, in which sums of rows are stored
    
    result <- hloa_allIntervals_quantity_byHourOfDayandYear_abs %>%
      select(-2:-7) %>%  # Remove columns 2 to 7
      bind_cols(select(hloa_allIntervals_quantity_byHourOfDayandYear_abs_num, 1:6)) %>% #Select columns 1 to 6 from hloa_allIntervals_quantity_byHourOfDayandYear_abs_num
      bind_cols(select(hloa_allIntervals_quantity_byHourOfDayandYear_abs_num,7)) #Add col with rowSums
    
    hloa_allIntervals_quantity_byHourOfDayandYear_abs <- result #Assign the result back to Intervals
    
    #View(hloa_allIntervals_quantity_byHourOfDayandYear_abs) 
    
    output_file_name <- paste0(data_type, "_DF7.csv")
    write.csv(hloa_allIntervals_quantity_byHourOfDayandYear_abs, file.path(output_path, output_file_name), row.names = TRUE)
    
  
    # Assuming "hloa_uncoveredIntervals_quantity_byHourOfDayandYear_abs_num" and "hloa_allIntervals_quantity_byHourOfDayandYear_abs_num" have the same structure
    result <- hloa_uncoveredIntervals_quantity_byHourOfDayandYear_abs_num %>% 
      mutate(across(everything(), ~ . / hloa_allIntervals_quantity_byHourOfDayandYear_abs_num[[cur_column()]])) #divide each element of all cells of "hloa_uncoveredIntervals_quantity_byHourOfDayandYear_abs_num" by the corresponding element in "hloa_allIntervals_quantity_byHourOfDayandYear_abs_num"
    
    hloa_uncoveredIntervals_quantity_byHourOfDayandYear_rel_dec_num <- result # Assign the result to "hloa_uncoveredIntervals_quantity_byHourOfDayandYear_rel_dec_num"
    
    hloa_uncoveredIntervals_quantity_byHourOfDayandYear_rel_dec <- hloa_allIntervals_quantity_byHourOfDayandYear_abs %>% #Create Dataframe with col 1 "Hour Index"
      select(-2:-8) %>%
      bind_cols(select(hloa_uncoveredIntervals_quantity_byHourOfDayandYear_rel_dec_num, 1:7))
    
    #View(hloa_uncoveredIntervals_quantity_byHourOfDayandYear_rel_dec)
    
    output_file_name <- paste0(data_type, "_DF8.csv")
    write.csv(hloa_uncoveredIntervals_quantity_byHourOfDayandYear_rel_dec, file.path(output_path, output_file_name), row.names = TRUE)
  
  
    #Plot hloa_uncoveredIntervals_quantity_byHourOfDayandYear_rel_dec_num_reduced (without 2016-2021 line)
    hloa_uncoveredIntervals_quantity_byHourOfDayandYear_rel_dec_num_reduced <- hloa_uncoveredIntervals_quantity_byHourOfDayandYear_rel_dec_num %>%
      select(-7) #remove col 7 with rowsum (2016-21)
    
    plot_hloa_uncoveredIntervals_quantity_byHourOfDayandYear_rel_dec <- hloa_uncoveredIntervals_quantity_byHourOfDayandYear_rel_dec_num_reduced %>%
      mutate(RowIndex = row_number()) %>%
      pivot_longer(cols = -RowIndex, names_to = "Year", values_to = "Value") %>%
      ggplot(aes(x = RowIndex, y = Value, color = Year)) +
      geom_line(size = 1, linetype = "solid") +
      labs(
        title = "Distribution of uncovered hours over the day",
        x = "Hour of Day",
        y = "Uncovered time intervals [in percent of all intervals]",
        color = "Year"
      ) +
      scale_x_continuous(limits = c(1, 24), breaks = seq(1, 24, by = 1)) +
      theme_light() +
      coord_cartesian(xlim = c(1, 24), expand = FALSE)  # Zoom in without removing data
    
    #print(plot_hloa_uncoveredIntervals_quantity_byHourOfDayandYear_rel_dec) #Display the plot
    output_file_name <- paste0(data_type, "_P1rtDF8.png")
    ggsave(file.path(output_path, output_file_name), plot = plot_hloa_uncoveredIntervals_quantity_byHourOfDayandYear_rel_dec, width = 10, height = 6, dpi = 300)# Save the plot as a high-resolution image
  
    #Plot hloa_uncoveredIntervals_quantity_byHourOfDayandYear_rel_dec_num_reduced (without 2016-2021 line); with y-axis in percent
    plot_hloa_uncoveredIntervals_quantity_byHourOfDayandYear_rel_perc <- hloa_uncoveredIntervals_quantity_byHourOfDayandYear_rel_dec_num_reduced %>%
      mutate(RowIndex = row_number()) %>%
      pivot_longer(cols = -RowIndex, names_to = "Year", values_to = "Value") %>%
      ggplot(aes(x = RowIndex, y = Value, color = Year)) +
      geom_line(size = 1, linetype = "solid") +
      labs(
        title = "Distribution of uncovered hours over the day",
        x = "Hour of Day",
        y = "Uncovered time intervals [in relation to total intervals]",
        color = "Year"
      ) +
      scale_x_continuous(limits = c(1, 24), breaks = seq(1, 24, by = 1)) +
      scale_y_continuous(labels = function(x) paste0(x*100))+
      theme_light() +
      coord_cartesian(xlim = c(1, 24), expand = FALSE)  # Zoom in without removing data
    
    print(plot_hloa_uncoveredIntervals_quantity_byHourOfDayandYear_rel_perc) #Display the plot
    
    output_file_name <- paste0(data_type, "_P2rtDF8.png")
    ggsave(file.path(output_path, output_file_name), plot = plot_hloa_uncoveredIntervals_quantity_byHourOfDayandYear_rel_perc, width = 10, height = 6, dpi = 300)# Save the plot as a high-resolution image
  
  
    #Plot hloa_uncoveredIntervals_quantity_byHourOfDayandYear_rel_dec_num_reduced (without 2016-2021 line); with y-axis in percent and uniform scale
    plot_hloa_uncoveredIntervals_quantity_byHourOfDayandYear_rel_perc_0to1 <- hloa_uncoveredIntervals_quantity_byHourOfDayandYear_rel_dec_num_reduced %>%
      mutate(RowIndex = row_number()) %>%
      pivot_longer(cols = -RowIndex, names_to = "Year", values_to = "Value") %>%
      ggplot(aes(x = RowIndex, y = Value, color = Year)) +
      geom_line(size = 1, linetype = "solid") +
      labs(
        title = data_type,": Distribution of uncovered hours over the day",
        x = "Hour of Day",
        y = "Uncovered time intervals [in percent of all intervals]",
        color = "Year"
      ) +
      scale_x_continuous(limits = c(1, 24), breaks = seq(1, 24, by = 1)) +
      scale_y_continuous(limits = c(0, 1), labels = function(x) paste0(x*100)) + # Adjust y-axis to range from 0 to 1
      theme_light() +
      coord_cartesian(xlim = c(1, 24), expand = FALSE)  # Zoom in without removing data
    
    #print(plot_hloa_uncoveredIntervals_quantity_byHourOfDayandYear_rel_perc_0to1) #Display the plot
    
    output_file_name <- paste0(data_type, "_P3rtDF8.png")
    ggsave(file.path(output_path, output_file_name), plot = plot_hloa_uncoveredIntervals_quantity_byHourOfDayandYear_rel_perc_0to1, width = 6, height = 6, dpi = 300)# Save the plot as a high-resolution image
    
  
    #Plot hloa_uncoveredIntervals_quantity_byHourOfDayandYear_rel_dec_incl20165021
    plot_hloa_uncoveredIntervals_quantity_byHourOfDayandYear_rel_dec_incl2016to21 <- hloa_uncoveredIntervals_quantity_byHourOfDayandYear_rel_dec_num %>%
      mutate(RowIndex = row_number()) %>%
      pivot_longer(cols = -RowIndex, names_to = "Year", values_to = "Value") %>%
      mutate(
        Year = factor(Year, levels = c("2016", "2017", "2018", "2019", "2020", "2021", "2016-2021"))
      ) %>%  # Adjust the order of levels
      ggplot(aes(x = RowIndex, y = Value, color = Year, linetype = Year)) +
      geom_line(size = 1) +
      labs(
        title = "Distribution of uncovered hours over the day",
        x = "Hour of Day",
        y = "Uncovered time intervals [in relation to total intervals]",
        color = "Year"
      ) +
      scale_x_continuous(limits = c(1, 24), breaks = seq(1, 24, by = 1)) +
      theme_light() +
      scale_linetype_manual(
        values = c(
          "2016" = "solid", "2017" = "solid", "2018" = "solid", "2019" = "solid",
          "2020" = "solid", "2021" = "solid", "2016-2021" = "dashed"
        ),
        breaks = c("2016", "2017", "2018", "2019", "2020", "2021", "2016-2021"),
        labels = c("2016", "2017", "2018", "2019", "2020", "2021", "2016-2021")
      ) +  # Set linetype for "2016-2021" and adjust legend
      scale_color_manual(
        values = c(
          "2016" = "#F8766D", "2017" = "#B79F00", "2018" = "#00BA38", "2019" = "#00BFC4",
          "2020" = "#619CFF", "2021" = "#F564E3", "2016-2021" = "gray"
        )
      ) +  # Set colors, including gray for "2016-2021"
      coord_cartesian(xlim = c(1, 24), expand = FALSE)  # Zoom in without removing data
    
    #print(plot_hloa_uncoveredIntervals_quantity_byHourOfDayandYear_rel_dec_incl2016to21) #Display the plot
    
    output_file_name <- paste0(data_type, "_P4rtDF8.png")
    ggsave(file.path(output_path, output_file_name), plot = plot_hloa_uncoveredIntervals_quantity_byHourOfDayandYear_rel_dec_incl2016to21, width = 10, height = 6, dpi = 300)# Save the plot as a high-resolution image
  
  ################################### COVERAGES & SHORTAGES ###################################
  
  ### How strong was the coverage throughout all intervals by hour of day and year 
    
    # Average Coverage throughout all intervals by Hour of Day and Year, Average Coverage (percent); relative value, not calculated by timestamp but calculated via SUM Coverage/SUM Demand per Hour of Day and Year
      # Step 1: Grouping by 'hour index' and 'year index' and calculating the required values
      hloa_allIntervals_AvgCoverage_byHourOfDayandYear_viaSumCoverageDividedBySumGODemand_rel2demand_perc <- hloa %>%
        group_by(`Hour Index`, `Year Index`) %>%
        summarise(
          SumCoverage = sum(`Coverage`),
          SumGODemand = sum(`GO Demand`),
          AvgCoverage = (SumCoverage / SumGODemand) * 100  
        ) %>%
        ungroup()  # Remove grouping
      
      #View(hloa_allIntervals_AvgCoverage_byHourOfDayandYear_viaSumCoverageDividedBySumGODemand_rel2demand_perc)
      
      output_file_name <- paste0(data_type, "_DF9.csv")
      write.csv(hloa_allIntervals_AvgCoverage_byHourOfDayandYear_viaSumCoverageDividedBySumGODemand_rel2demand_perc, file.path(output_path, output_file_name), row.names = TRUE)
      
      # Step 2: Creating a ggplot geom_point
      pointchart_hloa_allIntervals_AvgCoverage_byHourOfDayandYear_viaSumCoverageDividedBySumGODemand_rel2demand_perc <- ggplot(hloa_allIntervals_AvgCoverage_byHourOfDayandYear_viaSumCoverageDividedBySumGODemand_rel2demand_perc, aes(x = `Hour Index`, y = AvgCoverage, fill = factor(`Year Index`))) +
        geom_point() +
        labs(
          title = "Average Coverage by Hour of Day and Year",
          x = "Hour Index",
          y = "Average Coverage (%)"
        ) +
        facet_wrap(~`Year Index`, scales = "free_y")  #Create separate plots for each year
      
      #print(pointchart_hloa_allIntervals_AvgCoverage_byHourOfDayandYear_viaSumCoverageDividedBySumGODemand_rel2demand_perc)
      output_file_name <- paste0(data_type, "_P5rtDF9.png")
      ggsave(file.path(output_path, output_file_name), plot = pointchart_hloa_allIntervals_AvgCoverage_byHourOfDayandYear_viaSumCoverageDividedBySumGODemand_rel2demand_perc, width = 10, height = 6, dpi = 300)# Save the plot as a high-resolution image
      
  
  ### How strong was the shortage throughout the uncovered intervals by hour of day and year 
    # Average Undercoverage throughout the uncovered intervals by Hour of Day and Year, Average Undercoverage (percent); relative value, not calculated by timestamp but calculated via SUM Shortage/SUM GO Demand per Hour of Day and Year
      # Step 1: Grouping by 'hour index' and 'year index' and calculating the required values
      hloa_uncoveredIntervals_AvgUndercoverage_byHourOfDayandYear_viaSumShortageDividedBySumGODemand_rel2demand_percAmount <- hloa %>%
        group_by(`Hour Index`, `Year Index`) %>%
        summarise(
          SumCoverage = sum(ifelse(`Coverage` < 0, `Coverage`, 0)),
          SumGODemand = sum(ifelse(`Coverage` < 0, `GO Demand`, 0)),
          AvgUndercoverage = (SumCoverage / SumGODemand) * -100  
        ) %>%
        ungroup()  # Remove grouping
      
      #View(hloa_uncoveredIntervals_AvgUndercoverage_byHourOfDayandYear_viaSumShortageDividedBySumGODemand_rel2demand_percAmount)
      
      output_file_name <- paste0(data_type, "_DF10.csv")
      write.csv(hloa_uncoveredIntervals_AvgUndercoverage_byHourOfDayandYear_viaSumShortageDividedBySumGODemand_rel2demand_percAmount, file.path(output_path, output_file_name), row.names = TRUE)
      
      
      # Step 2: Creating a ggplot geom_point
      pointchart_hloa_uncoveredIntervals_AvgUndercoverage_byHourOfDayandYear_viaSumShortageDividedBySumGODemand_rel2demand_percAmount <- ggplot(hloa_uncoveredIntervals_AvgUndercoverage_byHourOfDayandYear_viaSumShortageDividedBySumGODemand_rel2demand_percAmount, aes(x = `Hour Index`, y = AvgUndercoverage, fill = factor(`Year Index`))) +
        geom_point() +
        labs(
          title = "Average Undercoverage by Hour of Day and Year",
          x = "Hour Index",
          y = "Average Undercoverage (%)"
        ) +
        facet_wrap(~`Year Index`, scales = "free_y")  #Create separate plots for each year
      
      #print(pointchart_hloa_uncoveredIntervals_AvgUndercoverage_byHourOfDayandYear_viaSumShortageDividedBySumGODemand_rel2demand_percAmount)
      
      output_file_name <- paste0(data_type, "_P6rtDF10.png")
      ggsave(file.path(output_path, output_file_name), plot = pointchart_hloa_uncoveredIntervals_AvgUndercoverage_byHourOfDayandYear_viaSumShortageDividedBySumGODemand_rel2demand_percAmount, width = 10, height = 6, dpi = 300)# Save the plot as a high-resolution image
  
  ### How strong was the shortage throughout the uncovered intervals by hour of day
    # Average Undercoverage throughout the uncovered intervals by Hour of Day, Average Undercoverage (percent); relative value, not calculated by timestamp but calculated via SUM Shortage/SUM GO Demand per Hour of Day
      # Step 1: Grouping by 'hour index' and calculating the required values
      hloa_uncoveredIntervals_AvgUndercoverage_byHourOfDay_viaSumShortageDividedBySumGODemand_rel2demand_percAmount <- hloa %>%
        group_by(`Hour Index`) %>%
        summarise(
          SumCoverage = sum(ifelse(`Coverage` < 0, `Coverage`, 0)),
          SumGODemand = sum(ifelse(`Coverage` < 0, `GO Demand`, 0)),
          AvgUndercoverage = (SumCoverage / SumGODemand) * -100  
        ) %>%
        ungroup()  # Remove grouping
      
      #View(hloa_uncoveredIntervals_AvgUndercoverage_byHourOfDay_viaSumShortageDividedBySumGODemand_rel2demand_percAmount)
      
      output_file_name <- paste0(data_type, "_DF10a.csv")
      write.csv(hloa_uncoveredIntervals_AvgUndercoverage_byHourOfDay_viaSumShortageDividedBySumGODemand_rel2demand_percAmount, file.path(output_path, output_file_name), row.names = TRUE)
      
      
      # Step 2: Creating a ggplot geom_point
      pointchart_hloa_uncoveredIntervals_AvgUndercoverage_byHourOfDay_viaSumShortageDividedBySumGODemand_rel2demand_percAmount <- ggplot(hloa_uncoveredIntervals_AvgUndercoverage_byHourOfDay_viaSumShortageDividedBySumGODemand_rel2demand_percAmount, aes(x = `Hour Index`, y = AvgUndercoverage)) +
        geom_point() +
        labs(
          title = "Average Undercoverage by Hour of Day",
          x = "Hour Index",
          y = "Average Undercoverage (%)"
        ) 
      
      #print(pointchart_hloa_uncoveredIntervals_AvgUndercoverage_byHourOfDay_viaSumShortageDividedBySumGODemand_rel2demand_percAmount)
      
      output_file_name <- paste0(data_type, "_P6artDF10a.png")
      ggsave(file.path(output_path, output_file_name), plot = pointchart_hloa_uncoveredIntervals_AvgUndercoverage_byHourOfDay_viaSumShortageDividedBySumGODemand_rel2demand_percAmount, width = 10, height = 6, dpi = 300)# Save the plot as a high-resolution image
}