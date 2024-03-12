#'------------------------------------------------------------------------------
#
# Script to extract and process VMS and logbook data for ICES VMS data call
# 3.1: Construct and collect tables                                         ----
#
#'------------------------------------------------------------------------------

# Loop trough years to submit
for(year in yearsToSubmit){
  
  # load data
  load(file = paste0(outPath,paste0("/cleanEflalo",year,".RData")))
  
  #'----------------------------------------------------------------------------
  # 3.1.1 Create table 2                                                    ----
  #'----------------------------------------------------------------------------
  # Extract the year and month from the date-time column
  eflalo$Year <- year(eflalo$FT_LDATIM)
  eflalo$Month <- month(eflalo$FT_LDATIM)
  
  # Set interval to 1 day for later caculation of kwDays
  eflalo$INTV <- 1
  
  # Create a record variable for aggregation of records per vessel
  eflalo$record <- 1
  
  # Aggregate the dummy variable by VE_COU, VE_REF, and LE_CDAT
  res <- aggregate(
    eflalo$record,
    by = as.list(eflalo[, c("VE_COU", "VE_REF", "LE_CDAT")]),
    FUN = sum,
    na.rm = TRUE
  )
  
  # Rename the columns of the aggregated data frame
  colnames(res) <- c("VE_COU", "VE_REF", "LE_CDAT", "nrRecords")
  
  # Merge the aggregated data frame with eflalo
  eflalo <- merge(eflalo, res, by = c("VE_COU", "VE_REF", "LE_CDAT"))
  
  # Adjust the interval and calculate kilowatt-days
  eflalo$INTV <- eflalo$INTV / eflalo$nrRecords
  eflalo$kwDays <- eflalo$VE_KW * eflalo$INTV
  
  # Define the record type
  RecordType <- "LE"
  
  # Define the columns to be included in the table
  cols <- c(
    "VE_REF", "VE_COU", "Year", "Month", "LE_RECT", "LE_GEAR", "LE_MET",
    "VE_LEN", "tripInTacsat", "INTV", "kwDays", "LE_KG_TOT", "LE_EURO_TOT"
  )
  
  # Create or append to table2 based on the year
  if (year == yearsToSubmit[1]) {
    table2 <- cbind(RT = RecordType, eflalo[, cols])
  } else {
    table2 <- rbind(table2, cbind(RT = RecordType, eflalo[, cols]))
  }
  
  
  # Save table2 
  save(
    table2,
    file = file.path(outPath, "table2.RData" )
  )
  
  message(glue ("Table 2 for year {year} is completed") )
  
  
  #'----------------------------------------------------------------------------
  # 3.1.2   Create table 1                                                  ----
  #'----------------------------------------------------------------------------
  tacsatEflalo <- readRDS(paste0(outPath, "/tacsatEflalo_", year, ".rds"))
  tacsatEflalo <- data.frame(tacsatEflalo)
  
  # Define the record type
  RecordType <- "VE"
  
  # Define the columns to be included in the table
  cols <- c(
    "VE_REF", "VE_COU", "Year", "Month", "Csquare", "LE_GEAR",
    "LE_MET", "SI_SP", "INTV", "VE_LEN", "kwHour", "VE_KW", "LE_KG_TOT", "LE_EURO_TOT",
    "MSFD_BBHT", "GEARWIDTH", "SA_M2"
  )
  
  # Create or append to table1 based on the year
  if (year == yearsToSubmit[1]) {
    table1 <- cbind(RT = RecordType, tacsatEflalo[, cols])
  } else {
    table1 <- rbind(table1, cbind(RT = RecordType, tacsatEflalo[, cols]))
  }
  
  # Save
    save(
    table1,
    file = file.path(outPath, "table1.RData" )
  )
  
  message(glue("Table 1 for year {year} is completed") )
}

# Housekeeping
rm(tacsatEflalo, cols, RecordType, eflalo, res, table1, table2)
#'------------------------------------------------------------------------------
# End of script                                                             
#'------------------------------------------------------------------------------

