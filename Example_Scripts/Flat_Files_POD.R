# READING EWRIMS FLAT FILES
# Example: read, filter, and write the POD flat files.
#
##########################################################################

install.packages(c("dplyr", "data.table","readr"))

library(dplyr)
library(data.table) #fread() Fast and friendly file finagler
library(readr)

# the functions file needs to be sourced for this script to work
source("C:/Users/vzimmer/Documents/R/eWRIMS_read/eWRIMS/FUNCTIONS/ewrims.R")

######################
## Open up the Flat Files Data. Note: data files are LARGE - do not run the entire code unless you want all headers! 

# location of the flat files, either on the server or iin a local directory
ffloc <- "https://intapps.waterboards.ca.gov/downloadFile/faces/flatFilesEwrims.xhtml?fileName="

# designate which flat file you want to read. Options are:
# "POD", "Master", "RMS", "RMSext", "UseSeason", "Petition", "Party", AnnualRPT",
# "IrrAR", "Device", "Complaint", "Investigation", "Enforcement", "Violation", 
# "POI", "Inspection", "Status_Hist"

# Example: read the POD flat file
POD <-read_FF(ffloc, "POD")

#############################################
# OPTIONAL: filter for your watershed

#example below - change to your HUC8
HUC8_WS <- as.character(18010104)
#filter desired rows that match HUC8,
PODs <- POD %>% filter(HUC_8_NUMBER %in% HUC8_WS)


#################
# OPTIONAL: Keep these columns

cols <- c("POD_ID", "APPL_ID","APPL_POD", "LATITUDE", "LONGITUDE",
           "POD_TYPE","POD_COUNT",
           "PRIMARY_OWNER_NAME", "PARCEL_NUMBER", "COUNTY",
           "WATER_RIGHT_TYPE", "SUB_TYPE", "WATER_RIGHT_STATUS",
            "SECTION_NUMBER", "SECTION_CLASSIFIER", "TOWNSHIP_NUMBER", "TOWNSHIP_DIRECTION",
            "RANGE_NUMBER", "RANGE_DIRECTION", "QUARTER", "QUARTER_QUARTER",
           
           #SOURCE
          "SPECIAL_USE_AREA", "WATERSHED", 
          "HUC_8_NUMBER", "HUC_8_NAME",
           "HUC_12_NUMBER","HUC_12_NAME", "SOURCE_NAME",  "TRIB_DESC",
           "POD_NAME", "DIVERSION_SITE_NAME", "SOURCE_TYPE",
          
 
           #AREA AND USE
           "USE_NET_ACREAGE","USE_GROSS_ACREAGE",
           "USE_CODE", "USE_COUNT",
           
           #SEASON
           "DIRECT_DIV_SEASON_START","DIRECT_DIV_SEASON_END","STORAGE_SEASON_START","STORAGE_SEASON_END",
           
           #AMOUNTS
           "FACE_VALUE_AMOUNT", "FACE_VALUE_UNITS",
           "DIRECT_DIV_AMOUNT","STORAGE_AMOUNT"
)

PODs <- PODs %>% select(cols) 

############################
export_loc <- "C:/Users/vzimmer/Documents/R/eWRIMS_read/exports/"  # where you want the csv to be written
write.csv(PODs, file = paste(export_loc, "example_POD.csv", sep=""))







