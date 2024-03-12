#'------------------------------------------------------------------------------
#
# Script to extract and process VMS and logbook data for ICES VMS data call
# 0.5: Changing folder? This is specifically for Denmark still, but maybe
# Other countries can use it too. It takes csv files and changes 
# them to Rdata files
#
#'------------------------------------------------------------------------------

for(year in 2009:2023){
  print(year)
  
  eflalo <- fread(paste0("//ait-pdfs.win.dtu.dk/Qdrev/AQUA/dfad/data/Data/eflalo/eflalo4_", year, ".csv"))

  #Corrections
  if(year %in% 2009:2011){
    eflalo[VE_REF == "DNK000011949" & is.na(as.numeric(VE_LEN)), VE_LEN := 8.11]
    eflalo[VE_REF == "DNK000011949" & is.na(as.numeric(VE_TON)), VE_TON := 4.99]
  }

  if(year %in% 2010:2017){
    eflalo[VE_REF == "DNK000013968" & is.na(as.numeric(VE_LEN)), VE_LEN := 40.8]
    eflalo[VE_REF == "DNK000013968" & is.na(as.numeric(VE_TON)), VE_TON := 441]
  }


  #Sometimes departure time will be after landing time because landing time is recorded as "0.00"
  #in the eflalo. In this case, set the landing time to "23.00"

  # Apply the convert to date-time function to the FT_DDAT and FT_DTIME columns
  eflalo$FT_DDATIM <- convert_to_datetime(eflalo$FT_DDAT, eflalo$FT_DTIME)
  # Apply the function to the FT_LDAT and FT_LTIME columns
  eflalo$FT_LDATIM <- convert_to_datetime(eflalo$FT_LDAT, eflalo$FT_LTIME)
  eflalo[FT_LDATIM < FT_DDATIM & FT_LTIME == "0:00", FT_LTIME := "23:00"]

  eflalo$FT_DDATIM <- NULL
  eflalo$FT_LDATIM <- NULL
  
  eflalo <- eflalo_clean(eflalo)
  save(eflalo, file = paste0(dataPath, "eflalo_", year, ".RData"))
  
  tacsat <- fread(paste0("//ait-pdfs.win.dtu.dk/Qdrev/AQUA/dfad/data/Data/eflalo/tacsat2_", year, ".csv"))
  tacsat <- tacsat_clean(tacsat)
  save(tacsat, file = paste0(dataPath, "tacsat_", year, ".RData"))
}



