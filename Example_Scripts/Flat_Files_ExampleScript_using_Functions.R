# READING EWRIMS FLAT FILES - EXAMPLE SCRIPTS
# 
# You must be able to read the functions files (eWRIMS.R) for this to work
#     eWRIMS.R contains functions (code) that are common to many analyses
#
# VLZ 2021.01.26
#
######## Set up R packages ######################################
install.packages(c("here", "dplyr", "data.table","readr", "ggplot2", "scales"))

# open the packages; need to do this every time you open the file.
library(here)
library(dplyr)
library(data.table) 
library(readr)
library(ggplot2)
library(scales) 

########## HOW TO SOURCE DATA AND FILES CORRECTLY #########

# the functions file needs to be sourced for this script to work
source(here("ewrims.R"))

######## READ IN DATA FILES ################
# Read in flat file (must be connected via HQ intranet for flat file access OR have downloaded locally) --------------------

ffloc <- "https://intapps.waterboards.ca.gov/downloadFile/faces/flatFilesEwrims.xhtml?fileName="  #flat file location

# designate which flat file you want to read. Options are:
# "POD", "Master", "RMS", "RMSext", "UseSeason", "Petition", "Party", AnnualRPT",
# "IrrAR", "Device", "Complaint", "Investigation", "Enforcement", "Violation", 
# "POI", "Inspection", "Status_Hist"

POD <-read_FF(ffloc, "POD")  # this loads the POD (point of diversion) flat file
RMS <-read_FF(ffloc, "RMS")  # this loads the year-month reported water use and diversion (LARGE FILE!!!)

####
###########   EXAMPLES OF BASIC DATA FILTERING #######################
###

### if you just want to see full records for a single water right (examples)
oneRMS <- RMS[grepl("S026091", RMS$APPL_ID),]
onePOD <- POD[grepl("S026091", POD$APPL_ID),]

#### see column names for a flat file; in this case the "POD" file 
POD_cols <- colnames(POD, do.NULL = TRUE, prefix = "col")
View(POD_cols) # to see as a datasheet tab,
print(POD_cols) # to see printed in the console

############ WATERSHED ANALYSIS #####################
#### filter for your watershed
# Note: not all flat files have the HUC #s. You may need use a flat file that has them (like PODs)
#       and then use a join with that POD file and the other file (like RMS). Many of the
#       built in functions below have this already embedded. See below for an example filter script.

## for a HUC6, keep only the matching HUC6 partial of the HUC8s
HUC6 <- as.character(180101)
PODs <- POD[grepl(HUC6, POD$HUC_8_NUMBER),] #keep your HUC6

## for a HUC8, filter directly
HUC8 <- as.character(18010110)
PODs <- POD %>% filter(HUC_8_NUMBER %in% HUC8)

# multiple HUC8s
HUC8s <- c("18010110", "18010106")
PODs <- POD %>% filter(HUC_8_NUMBER %in% HUC8s) 

## for a HUC10, keep only the matching HUC10 partial of the HUC12s
HUC10<- as.character(1801011007)
PODs <- POD[grepl(HUC10, POD$HUC_12_NUMBER),] #keep your HUC10

## for a HUC12, filter directly
HUC12<- as.character(180101100706)
PODs <- POD %>% filter(HUC_12_NUMBER %in% HUC12)


# If you want to filter a flat file that has APPL_ID but does NOT have HUC8 or HUC12:
#   run one of the above on POD or master (possible) and then filter based on the POD or master file, as demonstrated below:
# FOR EXAMPLE: RMS does not have HUC8 or HUC12 information, but you can still extract the files by first filtering the POD file, and then using that POD list to filter the RMS file.
RMS_filtered <- RMS %>% filter(APPL_ID %in% PODs$APPL_ID)

#### optional: choose columns to select (you can use full list if you prefer)
# generate list of columns to help you help the below list.
POD_cols <- colnames(POD, do.NULL = TRUE, prefix = "col")

# example: here are the columns selected
cols <- c("POD_ID", "APPL_ID","APPL_POD", "LATITUDE", "LONGITUDE",
          "POD_TYPE","POD_COUNT","PRIMARY_OWNER_NAME", "PARCEL_NUMBER",
          "WATER_RIGHT_TYPE", "SUB_TYPE", "WATER_RIGHT_STATUS", "POD_STATUS",
          
          #SOURCE
          "HUC_12_NUMBER","HUC_12_NAME", "SOURCE_NAME",
          "WATER_RIGHT_DESCRIPTION", "RECORD_SUMMARY",
          "POD_NAME", "DIVERSION_SITE_NAME",
          
          #AREA AND USE
          "USE_NET_ACREAGE","USE_GROSS_ACREAGE",
          "USE_CODE", "USE_COUNT",
          
          #SEASON
          "DIRECT_DIV_SEASON_START","DIRECT_DIV_SEASON_END","STORAGE_SEASON_START","STORAGE_SEASON_END",
          
          #AMOUNTS
          "FACE_VALUE_AMOUNT", "FACE_VALUE_UNITS","MAX_TAKEN_FROM_SOURCE",
          "DIRECT_DIV_AMOUNT","STORAGE_AMOUNT",
          "INI_REPORTED_DIV_AMOUNT","INI_REPORTED_DIV_UNIT"
)

PODs <- PODs %>% select(cols)

########################################################
### Optional: Remove non-relevant Records 

# Remove Inactive Water Rights. Note that WATER_RIGHT_STATUS and POD_STATUS must be in columns.
TYP_inactive <- list_inactive() #a list of inactive status types. 
PODs <- rminactive(PODs) #Note that WATER_RIGHT_STATUS and POD_STATUS must be in columns

# Remove Groundwater Wells
PODs <- rmgw(PODs) # overwrites PODs

# Remove Non-Irrigation Uses
View(list_noirr()) #a list of non-irrigation uses
PODs_remove <- list_noirr() 
PODs <- PODs[!(PODs$USE_CODE %in% PODs_remove),] # overwrites PODs

#Remove Non-Consumptive Uses
View(list_nocons()) #a list of non-consumptive uses
PODs_remove <- list_nocons()
PODs <- PODs[!(PODs$USE_CODE %in% PODs_remove),] # overwrites PODs

#Make your own list example (REMOVE OR KEEP)
my_list <- c("Recreational", "Aesthetic")
View(my_list) # see your list

# remove 
PODs <- PODs[!(PODs$USE_CODE %in% my_list),] # removes the examples in my_list if they match the column USE_CODE
# keep
PODs <- PODs[(PODs$USE_CODE %in% my_list),] # keeps only the examples in my_list if they match the column USE_CODE

#######################################
### Data Munging:  Filter for Watershed, remove USE, Add columns to RMS for convenience,
###   Summarize by month, summarize each APPL_ID by year

RMS_div <- rms_munge(RMS, PODs) # this removes use data (MOST COMMON, and default)

#RMS_use <- rms_munge(RMS, PODs, "use") # this removes diversion data (RARE)
#RMS_mixed <- rms_munge(RMS, PODs, "both") # this keeps mixed diversion and use data. NOT RECOMMENDED for further analysis because OTHER FUNCTIONS summarize and calculate on everything left. 

RMS_div_monthsum <- RMS_sum(RMS_div)  # sums up by year and month (for display)
RMS_div_by_APPLID <- RMS_asumyr(RMS_div) #sums up by water right for each year

#####################################################################
# Data Plotting and QA-QC

#### Plot the monthly summary

RMS_monthly_plot(title = "Total *Reported* Monthly Watershed Diversions, 2007-2020", RMS_div_monthsum, RMS_div)

##### Take Closer Look at Top Divertors for QA-QC Process ##################

#characterize top 50 monthly diversion reports, ever (HINT: all of these are wrong)
RMS_max_month <- RMS_div %>% arrange(desc(AMOUNT)) %>% slice(1:50)
View(RMS_max_month)  

# Review top 300 divertors by yearly amount. 
#      They should be large water divertors (muni, irrigation districts) first
RMS_max_yr <- rms300(RMS_div_by_APPLID, PODs) 
View(RMS_max_yr)
RMS_max_yr_save <- RMS_max_yr #save for later

###########################################################################
###### Delete or Fix bad records #################
# Need to develop lists of bad water rights records. Three
#     A. Manually developing lists. Easiest method for first time analysis.
#     B. Building a function. Must follow template in Bad_Russian exactly
#     C. importing from a csv file. May be easiest for subsequent runs.

# all three methods rely on inputting a 'list' into specialized functions.

#########
## STEP 1 GENERATE BAD WATER RIGHT LIST(S).  CHOOSE ONE METHOD, A, B, or C

#### A. Manual Example  ######################################################
delete_list <- data.frame("APPL_ID" = c("A031652", "S025446", "S001842", "S001842"),
                         YEAR=c(2015, 2019, 2014, 2019))

# fix_lists, use same format example as delete_list with new name

#### B. Keep the list of bad records in a R-code file (example Bad_Russian.R). (it is ok to use larger dataset for sub-watershed; extra records does not hurt the analysis) #############
 
source("C:/Users/vzimmer/Documents/R/eWRIMS_read/eWRIMS/imports/bad_RMS/Bad_Russian.R") # you must tell the script where to look. 

# generate lists
delete_list <- delete_Russian() #import the list of bad water rights to delete
fix_mgal_list <- mgal_Russian()
fix_af2gal_list <- af2gal_Russian()
fix_af2gpd_list <- af2gpd_Russian()


#### C - keep the list of bad records in a .csv file (example bad_Russian.csv) ##########################
# required fields APPL_ID and YEAR
bad_file <- "C:/Users/vzimmer/Documents/R/eWRIMS_read/eWRIMS/imports/bad_RMS/bad_Russian.csv"  #set the location of your file
bad_WRs <- fread(bad_file,header=TRUE, stringsAsFactors=FALSE, data.table=FALSE, blank.lines.skip=TRUE)

# generate_lists.  the 2nd argument is the name of the column
delete_list <- bad_csv(bad_WRs, "DIV_ERROR_TYPE", "delete")
fix_mgal_list <- bad_csv(bad_WRs, "DIV_ERROR_TYPE", "Gal2MGal") 
fix_af2gal_list <- bad_csv(bad_WRs, "DIV_ERROR_TYPE", "Gal2AF") 
fix_af2gpd_list <- bad_csv(bad_WRs, "DIV_ERROR_TYPE", "GPD") 

#########
## STEP 2 delete or fix water rights from the above lists

#initialize corrected RMS_div file
RMS_div_corr <- RMS_div

RMS_div_corr <- rms_delete(RMS_div_corr, delete_list) # Delete records (egregiously bad, or no time to fix)
RMS_div_corr <- rms_mgalfix(RMS_div_corr, fix_mgal_list) # fix million gallons errors (to gallons)
RMS_div_corr <- rms_ac2gal(RMS_div_corr, fix_af2gal_list) # fix acre-ft errors (to gallons)
RMS_div_corr <- rms_ac2gpd(RMS_div_corr, fix_af2gpd_list) # fix records listed as acre-ft that should be gpd to gallons

#######################################################################
######  POST QA-QC
RMS_div_monthsum <- RMS_sum(RMS_div_corr) # sums up by year and month (for display)
RMS_div_by_APPLID <- RMS_asumyr(RMS_div_corr) #sums up by water right for each year

RMS_monthly_plot(title = "Total *Reported* Monthly Watershed Diversions, 2007-2020", RMS_div_monthsum, RMS_div_corr)

RMS_max_month <- RMS_div_corr %>% arrange(desc(AMOUNT)) %>% slice(1:500)
View(RMS_max_month)  

RMS_max_yr <- rms300(RMS_div_by_APPLID, PODs) 
View(RMS_max_yr)

# Generate list of bad water rights for review (NOTE: for manual entry generate bad_WRs first)
RMS_bad_list <- left_join(bad_WRs, RMS_max_yr_save)
names(RMS_bad_list)[names(RMS_bad_list)=="AMOUNT"] <- "ORIGINAL_AMOUNT"

RMS_bad_list <- left_join(RMS_bad_list, RMS_max_yr)
names(RMS_bad_list)[names(RMS_bad_list)=="AMOUNT"] <- "REPAIRED_AMOUNT"

RMS_bad_list <- RMS_bad_list %>% select("APPL_ID", "YEAR", "ORIGINAL_AMOUNT", "REPAIRED_AMOUNT", "FACE_VALUE_AMOUNT", "DIV_ERROR_TYPE", "PRIMARY_OWNER_NAME", "USE_CODE", everything())
RMS_bad_list$ORIGINAL_AMOUNT <- round(RMS_bad_list$ORIGINAL_AMOUNT,digits=0)
RMS_bad_list$REPAIRED_AMOUNT <- round(RMS_bad_list$REPAIRED_AMOUNT,digits=3)
View(RMS_bad_list)         

##########################################################
# #summarize annual and monthly diversions and generate statistics

RMS_div_stats_annual <- rms_yrstat(RMS_div_corr)
RMS_div_stats_monthly <- rms_mstat(RMS_div_corr)

# add annual stats to the PODs list
RMS_PODs <- left_join(PODs, RMS_div_stats_annual)

####################################################################################
# WRITE ALL TO CSV

export_loc <- "C:/Users/vzimmer/Documents/R/eWRIMS_read/eWRIMS/exports/"

write.csv(RMS_PODs, file = paste(export_loc, "RMS_PODS.csv", sep=""))  # writes a file with POD information and annual statistics

write.csv(RMS_stats_monthly, file = paste(export_loc, "Monthly_Stats.csv", sep=""))  # writes a file with just the monthly statistics (each WR has 12 records)

write.csv(RMS_bad_list, file = paste(export_loc, "Fixed_and_Deleted_Records.csv", sep=""))  # tracks the records that were fixed and deleted.  

# Eliminate secondary PODs, generate single record for each water right with just one POD and annual statistics
RMS_PODs_Unique <- RMS_PODs[!duplicated(RMS_PODs$APPL_ID),]
write.csv(RMS_PODs_Unique, file = paste(export_loc, "RMSPODSunique.csv", sep=""))

