# SheddingLightOnGreenElectricityClaims
This repository includes all code related to the analyses performed as part of the manuscript "Shedding light on green electricity claims: temporal matching for more transparency in voluntary markets". All data used is publicly available of can be downloaded here: XXXXX

###################################################################################################################################################
###################################################################################################################################################
###### SHEDDING LIGHT ON GREEN CLAIMS: THE IMPACT OF A CLOSER TEMPORAL ALIGNMENT OF SUPPLY AND DEMAND IN VOLUNTARY GREEN ELECTRICITY MARKETS ######
############################################## by Hanna F. Scholta & Maximilian J. Blaschke #######################################################
###################################################################################################################################################
###################################################################################################################################################

#Note: Detailed explanations and reasoning on how we proceeded as well as on the used raw data sets can be derived from the Method chapter of our paper

###################################################################################################################
################################################################################################################### 
########################################### 1. ENTSO-E DATA PROCESSING ############################################
###################################################################################################################
###################################################################################################################

#Note: all files run in one session in the order outlined below
#Note: to run analysis on your device, input and output paths need to be updated

  # 1: Data Processing Generation_vPub.R
  # 2: Data Processing Generation_WindSolar_vPub.R
  # 3: Data Processing Load_vPub.R
  # 4: ExcelWrite_vPub.R

For outputs, see Folder "3_Outputs"
For analysis of data imputation, see Folder "4_Data Imputation Overview"

###################################################################################################################
###################################################################################################################
############################ 2. ENTSO-E & AIB DATA MERGING & COVERAGE CALCULATION #################################
###################################################################################################################
###################################################################################################################

# Entso-e and AIB data were merged in excel - see excel files for more information
# Coverages were also calculated there

# There is one workbook for 
  # 1: All renewables
  # 2: Solar
  # 3: Wind
  # 4: Solar&Wind

###################################################################################################################
###################################################################################################################
############################################### 3. DATA ANALYSIS ##################################################
###################################################################################################################
###################################################################################################################

The data analysis spans over the following four R-scripts (see Folder "R Analysis"):
	1_Master_ALL RES_Uncovered Intervals & Strengths (draws on Scripts in "Input Files" Folder: Load Packages.R, ALL RES_All Level of Analysis.R)	
	2_Master_Hloa_Distributions over Hours
	3_Master_Mloa_Qloa_Distributions over Indices
	4_Master_Further Analysis (

#Note: to run analysis on your device, input and output paths need to be updated

The folder "R Results" contains the outputs of the analysis scripts.
The deliverables of each skript are outlined below.

	##################################################################
	########## Master_ALL RES_Uncovered Intervals & Strengths ########
	##################################################################

	Dataframes
	File	R variable name																                    Content
	DF1	df_coveredIntervals_quantity_rel_dec                                Share of covered intervals (decimal)
 
	DF2	df_uncoveredIntervals_quantity_rel_dec													    Share of uncovered intervals (decimal)
 
	DF3	df_uncoveredIntervals_avgUndercoverage_rel2demand_percAmount				Average |Undercoverage| of the uncovered intervals depending on level of   
                                                                          analysis relative to GO Demand of these intervals per year (percent; 
                                                                          amount), relative value, not calculated by timestamp as in Excel File but 
                                                                          calculated via SUM Shortage/SUM GO Demand
                                                                          
	DF4	df_allIntervals_YearlyUndercoverage_rel2yearlyGODemand_percAmount		Summed |Undercoverage| depending on level of analysis relative to yearly 
                                                                          GO Demand (of all intervals) per year (percent, amount)
                                                                          
	DF5	df_allIntervals_avgCoverage_rel2demand_perc												  Average coverage of all intervals depending on level of analysis relative 
                                                                          to GO Demand per year (percent), relative value, not calculated by 
                                                                          timestamp as in Excel File but calculated via SUM Coverage/SUM GO Demand

	Plots		Content
	2016		Share of covered and uncovered intervals depending on level of analysis in 2016
	2017		Share of covered and uncovered intervals depending on level of analysis in 2017
	2018		Share of covered and uncovered intervals depending on level of analysis in 2018
	2019		Share of covered and uncovered intervals depending on level of analysis in 2019
	2020		Share of covered and uncovered intervals depending on level of analysis in 2020
	2021		Share of covered and uncovered intervals depending on level of analysis in 2021
	Combined plot	Share of covered and uncovered intervals depending on level of analysis in each year of anaylsis 

	# Figure 1 is based on DF2 and was created with PowerPoint/Think Cell
	# R plots merely served for initial illustration
	# Table A1 builds on DF4


	##################################################################
	############## Master_Hloa_Distribution over Hours ###############
	##################################################################

	Dataframes
	File		R variable name															                    Content
	DF6		hloa_uncoveredIntervals_quantity_byHourOfDayandYear_abs						Amount of uncovered hours by hour of day and year (absolute)
 
	DF7		hloa_allIntervals_quantity_byHourOfDayandYear_abs									Amount of intervals by hour of day and year (absolute)
 
	DF8		hloa_uncoveredIntervals_quantity_byHourOfDayandYear_rel_dec				Amount of uncovered intervals relative to amount of all intervals by hour 
                                                                          of day and year
                                                                          
	DF9		hloa_allIntervals_AvgCoverage_byHourOfDayandYear_viaSum           Average Coverage throughout all intervals by Hour of Day and Year, Average
        CoverageDividedBySumGODemand_rel2demand_perc				              Coverage (percent); relative value, not calculated by timestamp as in 
                                                                          Excel File but calculated via SUM Coverage/SUM Demand per Hour of Day and 
                                                                          Year
                                                                          
	DF10		hloa_uncoveredIntervals_AvgUndercoverage_byHourOfDayandYear     Average Undercoverage throughout the uncovered intervals by Hour of Day
          _viaSumShortageDividedBySumGODemand_rel2demand_percAmount		    and Year, Average Undercoverage (percent); relative value, not calculated 
                                                                          by timestamp as in Excel File but calculated via SUM Shortage/SUM GO 
                                                                          Demand per Hour of Day and Year	
                                                                          
	DF10a		hloa_uncoveredIntervals_AvgUndercoverage_byHourOfDay_via        Average Undercoverage throughout the uncovered intervals by Hour of Day,
          SumShortageDividedBySumGODemand_rel2demand_percAmount			      Average Undercoverage (percent); relative value, not calculated by 
                                                                          timestamp as in Excel File but calculated via SUM Shortage/SUM GO Demand 
                                                                          per Hour of Day

	Plots
	File		R plot name															                        Content	
	P1rtDF8		plot_hloa_uncoveredIntervals_quantity_byHourOfDayandYear      Amount of uncovered intervals relative to amount of all intervals by hour 
            _rel_dec								                                      of day and year (decimal)
            
	P2rtDF8		plot_hloa_uncoveredIntervals_quantity_byHourOfDayandYear      Amount of uncovered intervals relative to amount of all intervals by hour 
            _rel_perc								                                      of day and year (percent)
            
	P3rtDF8		plot_hloa_uncoveredIntervals_quantity_byHourOfDayandYear      Amount of uncovered intervals relative to amount of all intervals by hour
            _rel_perc_0to1								                                of day and year (percent; scale from 0-1)	
            
	P4rtDF8		plot_hloa_uncoveredIntervals_quantity_byHourOfDayandYear      Amount of uncovered intervals relative to amount of all intervals by hour
            _rel_dec_incl2016to21							                            of day and year (decimal; including combined line for complete timeframe                                                                             of analysis)
            
	P5rtDF9		pointchart_hloa_allIntervals_AvgCoverage_byHourOfDayandYear   Average Coverage throughout all intervals by Hour of Day and Year, Average 
            _viaSumCoverageDividedBySumGODemand_rel2demand_perc			      Coverage (percent); relative value, not calculated by timestamp as in   
                                                                          Excel File but calculated via SUM Coverage/SUM Demand per Hour of Day and 
                                                                          Year
                                                                          
	P6rtDF10	pointchart_hloa_uncoveredIntervals_AvgUndercoverage           Average Undercoverage throughout the uncovered intervals by Hour of Day
            _byHourOfDayandYear_viaSumShortageDividedBySumGODemand        and Year, Average Undercoverage (percent); relative value, not calculated
            _rel2demand_percAmount	                                      by timestamp as in Excel File but calculated via SUM Shortage/SUM GO 
                                                                          Demand per Hour of Day and Year 
                                                                                                                                             
	P6artDF10	pointchart_hloa_uncoveredIntervals_AvgUndercoverage_          Average Undercoverage throughout the uncovered intervals by Hour of Day,
            byHourOfDay_viaSumShortageDividedBySumGODemand_rel2demand     Average Undercoverage (percent); relative value, not calculated by 
            _percAmount	                                                  timestamp as in Excel File but calculated via SUM Shortage/SUM GO Demand 
                                                                          per Hour of Day	

	# Figure 2 is based on ALL RES_DF8 and was created with PowerPoint/Think Cell 
	# Figure A2 is based on Solar_DF8, Wind_DF8, Solar&Wind_DF8 and ALL RES_DF8 and was created with PowerPoint/Think Cell 
	# Figure A3 is based on ALL RES_DF10 and was created with PowerPoint/Think Cell 
	# R plots merely served for initial illustration

	# Text Value: 11.4% (Hour with largest volumetric shortfall) builds on DF10a


	##################################################################
	############ Master_Mloa_Qloa_Distribution over Indices ##########
	##################################################################

	Dataframes
	File		R variable name															                    Content
	DF11		mloa_uncoveredIntervals_quantity_byMonthOfYearAndYear_rel_dec   Amount of uncovered intervals relative to amount of all intervals by month 
                                                                          of year and year (decimal)
	DF12		mloa_uncoveredIntervals_quantity_byMonthOfYear_rel_dec					Amount of uncovered intervals relative to amount of all intervals by month 
                                                                          of year (decimal); spanning over complete timeframe of analysis
	DF13		mloa_uncoveredIntervals_avgUndercoverage_byMonthofYear
          _viaSumShortageDividedBySumGODemand_rel2demand_perc			        Average Undercoverage throughout the uncovered intervals by Month of Year, 
                                                                          Average Undercoverage (percent); relative value, not calculated by 
                                                                          timestamp but calculated via SUM Shortage/SUM GO Demand per Month of Year
	---
	DF15		qloa_uncoveredIntervals_quantity_byQuarterofYearAndYear_rel     Amount of uncovered intervals relative to amount of all intervals by
          _dec									                                          quarter of year and year (decimal)
                                                                          
	DF16		qloa_uncoveredIntervals_quantity_byQuarterOfYear_rel_dec				Amount of uncovered intervals relative to amount of all intervals by 
                                                                          quarter of year (decimal); spanning over complete timeframe of analysis
                                                                          
	DF17		qloa_uncoveredIntervals_avgUndercoverage_byQuarterOfYear        Average Undercoverage throughout the uncovered intervals by Quarter of
          _viaSumShortageDividedBySumGODemand_rel2demand_perc			        Year, Average Undercoverage (percent); relative value, not calculated by 
                                                                          timestamp but calculated via SUM Shortage/SUM GO Demand per Quarter of Year


	Plots
	File		  R plot name															                         Content	
	P7rtDF11	barchart_mloa_uncoveredIntervals_quantity_byMonthOfYearAndYear   Amount of uncovered intervals relative to amount of all intervals by 
            _rel_perc								                                         month of year and year (decimal)
                                                                              
	P8rtDF12	barchart_mloa_uncoveredIntervals_quantity_byMonthOfYear_rel_perc Amount of uncovered intervals relative to amount of all intervals by 
                                                                             month of year (decimal); spanning over complete timeframe of analysis
                                                                             
	P9rtDF13	pointchart_mloa_uncoveredIntervals_avgUndercoverage              Average Undercoverage throughout the uncovered intervals by Month of
            _byMonthofYear_viaSumShortageDividedBySumGODemand_rel2demand     Year, Average Undercoverage (percent); relative value, not calculated
            _perc                                                            by timestamp but calculated via SUM Shortage/SUM GO Demand per Month of 
                                                                             Year                                                                                
	P10rtDF13	barchart_mloa_uncoveredIntervals_avgUndercoverage_byMonthofYear  -"-
            _viaSumShortageDividedBySumGODemand_rel2demand_perc		

	P11rtDF15	barchart_qloa_uncoveredIntervals_quantity_byQuarterofYearAndYear  Amount of uncovered intervals relative to amount of all intervals by
            _rel_perc							                                            quarter of year and year (decimal)
            
	P12rtDF16	barchart_qloa_uncoveredIntervals_quantity_byQuarterOfYear         Amount of uncovered intervals relative to amount of all intervals by
            _rel_perc								                                          quarter of year (decimal); spanning over complete timeframe of 
                                                                              analysis	
                                                                              
	P13rtDF17	pointchart_qloa_uncoveredIntervals_avgUndercoverage               Average Undercoverage throughout the uncovered intervals by Month of 
            _byQuarterOfYear_viaSumShortageDividedBySumGODemand_rel2demand    Year, Average Undercoverage (percent); relative value, not calculated
            _perc		                                                          by timestamp but calculated via SUM Shortage/SUM GO Demand per Quarter 
                                                                              of Year
                                                                              
	P14rtDF17	barchart_qloa_uncoveredIntervals_avgUndercoverage_byQuarterOfYear -"-
            _viaSumShortageDividedBySumGODemand_rel2demand_perc			

	# Figure A1 is based on ALL RES_DF11 and ALL RES_DF15
	# Text Values of average undercoverage are based on ALL RES DF13 and 15
	# R plots merely served for initial illustration


	##################################################################
	##################### Master_Further Analysis ####################
	##################################################################

	Dataframes
	File		                                  R variable name									Content
	-				                                  hloa_min_coverages_df						Interval with minimum coverage under hourly matching, per year
	-				                                  dloa_min_coverages_df						Interval with minimum coverage under hourly matching, per year
  -				                                  wloa_min_coverages_df						Interval with minimum coverage under hourly matching, per year
  -				                                  mloa_min_coverages_df						Interval with minimum coverage under hourly matching, per year
  -				                                  qloa_min_coverages_df						Interval with minimum coverage under hourly matching, per year
  ALLRES_Min_Coverages_Scale-Up Solar&Wind	combined_df											Required scale-up in solar and wind generation to cover the most 
                                                                            undersupplied intervals under the different matching schemes in each 
                                                                            year (in % of TOTAL solar/wind generation)

	# Figure 3 is based on ALLRES_Min_Coverages_Scale-Up Solar&Wind
