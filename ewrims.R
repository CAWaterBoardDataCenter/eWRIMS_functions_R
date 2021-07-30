# Functions for eWRIMS analysis
#
#####################################################################

#### Reading Flat Files ####

read_FF <- function(ffloc, filetype) {
  if (filetype == "POD") {filename <- "ewrims_flat_file_pod.csv"}
  else if (filetype == "RMS") {filename <- "water_use_report.csv"}
  else if (filetype == "Master") {filename <- "ewrims_flat_file.csv"}
  else if (filetype == "RMSext") {filename <- "water_use_report_extended.csv"}
  else if (filetype == "UseSeason") {filename <- "ewrims_flat_file_use_season.csv"}
  else if (filetype == "Petition") {filename <- "ewrims_flat_file_petition.csv"}
  else if (filetype == "Party") {filename <- "ewrims_flat_file_party.csv"}
  else if (filetype == "AnnualRPT") {filename <- "ewrims_flat_file_annual_report.csv"}
  else if (filetype == "IrrAR") {filename <- "ewrims_flat_file_irrigation_annual_report.csv"}
  else if (filetype == "Device") {filename <- "ewrims_flat_file_device_annual_report.csv"}
  else if (filetype == "Complaint") {filename <- "ewrims_flat_file_complaint.csv"}
  else if (filetype == "Investigation") {filename <- "ewrims_flat_file_investigation.csv"}
  else if (filetype == "Enforcement") {filename <- "ewrims_flat_file_enforcement_action.csv"}
  else if (filetype == "Violation") {filename <- "ewrims_flat_file_violation.csv"}
  else if (filetype == "POI") {filename <- "ewrims_flat_file_point_of_investigation.csv"}
  else if (filetype == "Inspection") {filename <- "ewrims_flat_file_inspection.csv"}
  else if (filetype == "Status_Hist") {filename <- "ewrims_flat_file_master_status_history.csv"}
  else print("please add filetype argument as one of the following with quotes:
             POD, RMS, Master, RMSext, UseSeason, Petition, Party, AnnualRPT, IrrAR, Device,
             Complaint, Investigation, Enforcement, Violation, POI, Inspection, Status_Hist"
  )

  newfile <- fread(paste(ffloc,filename, sep =""),
                   header=TRUE, stringsAsFactors=FALSE, data.table=FALSE, blank.lines.skip=TRUE)
  return(newfile) 
  }


#### Combined Munging #####

rms_munge <- function(RMS, PODs, divuse=NULL) {
  print("rms_munge will return diversion data unless the third term is specifed as 'use' or 'both")
  
  # FIRST Filter RMS for records from watershed 
  RMS_div <- RMS %>% filter(APPL_ID %in% PODs$APPL_ID) 
  
  # SECOND Subset RMS for Diversion or Use only; default to diversion
  if (is.null(divuse)) {RMS_div <- RMS_remove(RMS_div, "use")}  # default, remove use data }
  else if (divuse == "use") {RMS_div <- RMS_remove(RMS_div, "div")}
  else if (divuse == "both") {}
  else if (divuse == "div") {RMS_div <- RMS_remove(RMS_div, "use")}
  else {
    print ("Unrecognized third term, please leave blank or specify 'div', 'use, or 'both'. returning default as diversion")
    RMS_div <- RMS_remove(RMS_div, "use")}  # unrecognized third term, remove use data
  
  # THIRD add YEARMO and WY Columns
  RMS_div <- yearmo(RMS_div)
  RMS_div <- wy(RMS_div)
  
  # Add Extra Columns to RMS for convenience
  RMS_div <- left_join(RMS_div,PODs[c("APPL_ID","USE_CODE","PRIMARY_OWNER_NAME", "FACE_VALUE_AMOUNT")])
  RMS_div <- unique(RMS_div) # this contains duplicates for each APPL_ID:PODID, so remove:

  return(RMS_div)
}

# Examine top reported diversions
rms300 <- function (RMS_div_by_APPLID, PODs) {
  RMS_div_top300 <- RMS_div_by_APPLID %>% arrange(desc(AMOUNT)) %>% slice(1:300)
  RMS_top300 <- left_join(RMS_div_top300,PODs[c("APPL_ID", "PRIMARY_OWNER_NAME", "USE_CODE", "FACE_VALUE_AMOUNT")])
  RMS_top300 <- unique(RMS_top300[order(-RMS_top300$AMOUNT),]) #duplicates where APPL_ID has > 1 POD
  return(RMS_top300)
}

#####  Data MUNGING and SUMMARIZING ##################################

### Add YEARMO and WY Column

yearmo <- function (file) {
  file$YEARMO <- paste0(file$YEAR,
                         ifelse(file$MONTH<10,paste0("0",file$MONTH),file$MONTH)) 
  return(file)
}

wy <- function(file){ 
  file$WY <- ifelse(file$MONTH<10,file$YEAR,file$YEAR+1)
  return(file)
}

### Subset RMS by Diversion_Type by removing categories

RMS_remove <- function(RMS_file, remove_type) {
  if (remove_type == "div" || remove_type == "diversion") {
    RMS_div <- RMS_file %>% filter(DIVERSION_TYPE == "USE")}  # only keep "Diversion" categories, not use ==
  else if (remove_type == "use") {
    RMS_div <- RMS_file %>% filter(DIVERSION_TYPE != "USE")}  #only keeping "Use", not diversion categories !=
  else {
    print ("rms_remove, nothing removed, please specifiy 'div' or 'use")
    RMS_div <- RMS_file }
  return (RMS_div)
}

### Summarizing

# summarize monthly diversions for entire dataset
RMS_sum <- function(RMS_div) {
  RMS_div_monthsum <- RMS_div %>% group_by(YEARMO) %>% summarize_at(vars(AMOUNT), sum)
  return(RMS_div_monthsum)
}

# summarize diversions by APPL_ID for each year
RMS_asumyr <- function(RMS_div) {
  RMS_div_by_APPLID <- RMS_div %>% group_by(APPL_ID, YEAR) %>% summarize_at(vars(AMOUNT),sum)
}

###################################################################
# PLOTTING
RMS_monthly_plot <- function(title, RMS_sum, RMS_div) {
  
  options(scipen=10000)

  yrmin = 2007
  yrmax = max(RMS_div$YEAR)
  xlabels <- as.character(seq(yrmin, yrmax, by=1 ))
  print(xlabels)
  
  yrmomin = 200701
  yrmomax = max(RMS_sum$YEARMO)
  xbreaks <- as.character(seq(yrmomin, yrmomax , by=100)) 

  ggplot(RMS_sum[RMS_sum$YEARMO>200912,]) + geom_col(aes(x=YEARMO,y=AMOUNT)) +
  scale_y_continuous(expand=c(0,0),breaks= pretty_breaks(),labels = scales::comma)+
  scale_x_discrete(name="Calendar Year",breaks=xbreaks,labels=xlabels) +
  ggtitle(title) +
  ylab("Surface Water Diversions (AF/month)") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0))
  
}

########  Lists to aid in filtering ##############################

list_inactive <- function() {
  clist <- c("Revoked","Cancelled","Inactive","Rejected")
  print(clist)
  return(clist)
}

list_noirr <- function() {
  clist <- c("Fire Protection", "Domestic", "Municipal", "Aquaculture", "Recreational",
            "Fish and Wildlife Preservation and Enhancement", "Aesthetic", "Dust Control",
            "Other", "Power", NA, "Stockwatering", "Industrial")
  print(clist)
  return (clist)
}

list_nocons <- function() {
  clist <- c("Recreational", "Fish and Wildlife Preservation and Enhancement", "Aesthetic", "Power")
  print(clist)
  return (clist)
}


########  Filtering functions ############################

# remove groundwater wells (works with PODs, uncertain if works with other FF)
rmgw <- function(inputRecord) {
  
  rec <- inputRecord
  
  rec <-rec[!grepl("UNDERFLOW", rec$SOURCE_NAME),]
  rec <-rec[!grepl("SUBTERRANEAN", rec$SOURCE_NAME),]
  rec <-rec[!grepl("Subsurface", rec$SOURCE_TYPE),]
  rec <-rec[!grepl("Well", rec$POD_NAME),]
  rec <-rec[!grepl("Well", rec$DIVERSION_SITE_NAME),]
  rec <-rec[!grepl("Well", rec$TYPE_OF_DIVERSION_FACILITY),]
  rec <-rec[!grepl("Well", rec$WATER_RIGHT_DESCRIPTION),]
  rec <-rec[!grepl("Well", rec$RECORD_SUMMARY),]
  
  return(rec)
}

# remove inactive water rights (works with PODs, uncertain if works with other FF)
rminactive <- function (inputRecord) {
  rec <- inputRecord
  rec_remove <- list_inactive()
  rec <- rec[!(rec$WATER_RIGHT_STATUS %in% rec_remove),]  # subset WATER_RIGHT_STATUS *NOT* in rec_remove
  rec <-rec[!grepl("Inactive", rec$POD_STATUS),]
  return(rec)
  
}


######## Removing and Filtering bad records ################ 

### making lists from a csv file

bad_csv <- function(badWRlist, column, listtype) {
  # add function if listtype is blank to assume that 
  
  #colnames <- colnames(badWRlist, do.NULL = TRUE, prefix = "col") # make list of col_names
  #print(list_cols)
  print ("bad_csv needs the badWRlist, listtype, and column. Column is where the error file is listed")
  print("bad_csv listtype should be the same as text in the ERROR_TYPE column: example delete, GPD, Gal2AF, Gal2MGal")
  
  newlist <- badWRlist #initialize newlist

  
  if (missing(column)) {
    print("Unknown Column Name for Error Designation, returning all")
    newlist <- newlist %>% select(c("APPL_ID", "YEAR"))}
  
  else {newlist <- newlist %>% select(c("APPL_ID", "YEAR", column))} # note that column is the third one 
    
  ### now the newlist should have [APPL_ID, YEAR, Column_with_Error]
  
  if (missing(listtype)) {print("Error type not specified, returning all records")}
  
  else {
    
    newlist <-newlist[grepl(listtype, newlist[[column]]),]}
  
  #newlist <- newlist %>% select(c("APPL_ID", "YEAR"))
  return (newlist)
}



########################
### there must be a watershed-specific R file OR an imported csv with a list of APPL_ID, YEAR to delete or edit.

# delete these records
rms_delete <- function(RMS_div, delete_list) {
  div_to_delete <- merge(RMS_div, delete_list) # inner join
  new_RMS <- anti_join(RMS_div, div_to_delete)
  return(new_RMS)
  
}

# convert records listed as million gallons to just gallons
rms_mgalfix <- function(RMS_div, fix_list) {
  
  # select the diversions to fix from RMS, make sure columns are the same
  div_to_fix <- merge(RMS_div, fix_list) #inner join, only works if there are records in both
  cols <- colnames(RMS_div, do.NULL = TRUE, prefix = "col")
  div_to_fix <- div_to_fix %>% select(cols)
  
  # delete the bad records out
  new_RMS <- anti_join(RMS_div, div_to_fix)  # delete records with error out
  
# fix the records and rejoin
  div_to_fix$AMOUNT <- div_to_fix$AMOUNT/1000000
  new_RMS <- rbind(new_RMS, div_to_fix)   # add records back in with fixed data
  
  return(new_RMS)

  }

# convert records listed as acre-ft to gallons
rms_ac2gal <- function(RMS_div, fix_list) {

  # select the diversions to fix from RMS, make sure columns are the same
  div_to_fix <- merge(RMS_div, fix_list) #inner join, only works if there are records in both
  cols <- colnames(RMS_div, do.NULL = TRUE, prefix = "col")
  div_to_fix <- div_to_fix %>% select(cols)
  
  # delete the bad records out
  new_RMS <- anti_join(RMS_div, div_to_fix)  # delete records with error out
  
  # fix the records and rejoin
  div_to_fix$AMOUNT <- div_to_fix$AMOUNT/325851
  new_RMS <- rbind(new_RMS, div_to_fix)   # add records back in with fixed data
  
  return(new_RMS)
  
}

# convert records listed as acre-ft that should be gallons-per-day into gallons

rms_ac2gpd <- function(RMS_div, fix_list) {

  # select the diversions to fix from RMS, make sure columns are the same
  div_to_fix <- merge(RMS_div, fix_list) #inner join, only works if there are records in both
  cols <- colnames(RMS_div, do.NULL = TRUE, prefix = "col")
  div_to_fix <- div_to_fix %>% select(cols)
  
  # delete the bad records out
  new_RMS <- anti_join(RMS_div, div_to_fix)  # delete records with error out
  
  # fix the records and rejoin
  div_to_fix$AMOUNT <- div_to_fix$AMOUNT*30/325851
  new_RMS <- rbind(new_RMS, div_to_fix)   # add records back in with fixed data
  
  return(new_RMS)
}


######################### GENERATING STATISTICS #####################

#
rms_yrstat <- function(RMS_div_corr) {
  
  RMS_div_annual <- RMS_div_corr %>% group_by(APPL_ID,WY,PRIMARY_OWNER_NAME,FACE_VALUE_AMOUNT) %>% summarize_at(vars(AMOUNT),sum)
  
  # generate mean, max, median. Filter WY > 2009 (W. Anderson)
  RMS_div_avg_annual <- RMS_div_annual %>% filter(WY>2009) %>% group_by(APPL_ID, PRIMARY_OWNER_NAME, FACE_VALUE_AMOUNT) %>% summarize_at(vars(AMOUNT),mean)
  RMS_div_max_annual <- RMS_div_annual %>% filter(WY>2009) %>% group_by(APPL_ID, PRIMARY_OWNER_NAME, FACE_VALUE_AMOUNT) %>% summarize_at(vars(AMOUNT),max)
  RMS_div_med_annual <- RMS_div_annual %>% filter(WY>2009) %>% group_by(APPL_ID, PRIMARY_OWNER_NAME, FACE_VALUE_AMOUNT) %>% summarize_at(vars(AMOUNT),median)

  # remame column, round to 3 digits, order by median
  names(RMS_div_avg_annual)[names(RMS_div_avg_annual)=="AMOUNT"] <- "AVG_DIVERTED"
  RMS_div_avg_annual$AVG_DIVERTED <- round(RMS_div_avg_annual$AVG_DIVERTED,digits=3)

  names(RMS_div_max_annual)[names(RMS_div_max_annual)=="AMOUNT"] <- "MAX_DIVERTED"
  RMS_div_max_annual$MAX_DIVERTED <- round(RMS_div_max_annual$MAX_DIVERTED,digits=3)

  RMS_div_med_annual <- RMS_div_med_annual[order(-RMS_div_med_annual$AMOUNT),]
  names(RMS_div_med_annual)[names(RMS_div_med_annual)=="AMOUNT"] <- "MEDIAN_DIVERTED"
  RMS_div_med_annual$MEDIAN_DIVERTED <- round(RMS_div_med_annual$MEDIAN_DIVERTED,digits=3)
  
  # compile into one file
  RMS_div_stats_annual <- left_join(RMS_div_avg_annual, RMS_div_med_annual)
  RMS_div_stats_annual <- left_join(RMS_div_stats_annual, RMS_div_max_annual)

  # delete redundant data and set No Face Value to Zero
  RMS_div_stats_annual <- unique(RMS_div_stats_annual)
  RMS_div_stats_annual$FACE_VALUE_AMOUNT[is.na(RMS_div_stats_annual$FACE_VALUE_AMOUNT)] <- 0
  
  return(RMS_div_stats_annual)
}


# summarize monthly diversions and average annual by APPL_ID --------------
rms_mstat <- function(RMS_div_corr) {
  
  # generate mean, max, median. Filter WY > 2009 (W. Anderson)
  RMS_div_med_monthly <- RMS_div_corr %>% filter(WY>2009) %>% group_by(APPL_ID,MONTH,PRIMARY_OWNER_NAME,FACE_VALUE_AMOUNT) %>% summarize_at(vars(AMOUNT),median)
  RMS_div_avg_monthly <- RMS_div_corr %>% filter(WY>2009) %>% group_by(APPL_ID,MONTH,PRIMARY_OWNER_NAME,FACE_VALUE_AMOUNT) %>% summarize_at(vars(AMOUNT),mean)
  RMS_div_max_monthly <- RMS_div_corr %>% filter(WY>2009) %>%  group_by(APPL_ID,MONTH,PRIMARY_OWNER_NAME,FACE_VALUE_AMOUNT) %>% summarize_at(vars(AMOUNT),max)

  # remame column, round to 3 digits, order by median
  names(RMS_div_avg_monthly)[names(RMS_div_avg_monthly)=="AMOUNT"] <- "AVG_DIVERTED"
  RMS_div_avg_monthly$AVG_DIVERTED <- round(RMS_div_avg_monthly$AVG_DIVERTED,digits=3)

  names(RMS_div_max_monthly)[names(RMS_div_max_monthly)=="AMOUNT"] <- "MAX_DIVERTED"
  RMS_div_max_monthly$MAX_DIVERTED <- round(RMS_div_max_monthly$MAX_DIVERTED,digits=3)

  RMS_div_med_monthly <- RMS_div_med_monthly[order(-RMS_div_med_monthly$AMOUNT),]
  names(RMS_div_med_monthly)[names(RMS_div_med_monthly)=="AMOUNT"] <- "MEDIAN_DIVERTED"
  RMS_div_med_monthly$MEDIAN_DIVERTED <- round(RMS_div_med_monthly$MEDIAN_DIVERTED,digits=3)
  
  # compile into one file
  RMS_div_stats_monthly <- left_join(RMS_div_avg_monthly, RMS_div_med_monthly)
  RMS_div_stats_monthly <- left_join(RMS_div_stats_monthly, RMS_div_max_monthly)
  
  return (RMS_div_stats_monthly)

}

