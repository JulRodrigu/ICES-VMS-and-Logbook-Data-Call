#'------------------------------------------------------------------------------
#
# Script to extract and process VMS and logbook data for ICES VMS data call
# 2.1: Linking TACSAT and EFLALO data                                       ----
#
#'------------------------------------------------------------------------------

# Looping through the years to submit
for(year in yearsToSubmit){
  print(paste0("Start loop for year ",year))
  
  #'----------------------------------------------------------------------------
  # 2.1.0 load TACSAT and EFLALO data from file                             ----
  #'----------------------------------------------------------------------------
  load(file = paste0(outPath,paste0("/cleanEflalo",year,".RData")) )
  load(file = paste0(outPath, paste0("/cleanTacsat", year,".RData")) )
  
  # Assign geometry column to tacsat for later operations
  tacsat$geometry <- NULL
  
  #'----------------------------------------------------------------------------
  # 2.1.1 Merge TACSAT and EFLALO                                             ----
  #'----------------------------------------------------------------------------
  tacsatp <- mergeEflalo2Tacsat(eflalo,tacsat)
  
  
  #'----------------------------------------------------------------------------
  # 2.1.2 Assign gear and length                                              ----
  #'----------------------------------------------------------------------------
  # Define the columns to be added
  cols <- c("LE_GEAR", "LE_MSZ", "VE_LEN", "VE_KW", "LE_RECT", "LE_MET", "LE_WIDTH", "VE_FLT", "VE_COU")
  
  # Use a loop to add each column
  for (col in cols) {
    # Match 'FT_REF' values in 'tacsatp' and 'eflalo' and use these to add the column from 'eflalo' to 'tacsatp'
    tacsatp[[col]] <- eflalo[[col]][match(tacsatp$FT_REF, eflalo$FT_REF)]
  }

  tacsatp <- data.frame(tacsatp)
  
  # Save not merged tacsat data
  # Subset 'tacsatp' where 'FT_REF' equals 0 (not merged)
  tacsatpmin <- subset(tacsatp, FT_REF == 0)
  
  # Feedback on tacsatpmin
  cat(sprintf("%.2f%% of of the tacsat data did not merge\n", (nrow(tacsatpmin) / (nrow(tacsatpmin) + nrow(tacsatp))) * 100))
  
  # Save 'tacsatpmin' to a file named "tacsatNotMerged<year>.RData" in the 'outPath' directory
  save(
    tacsatpmin,
    file = file.path(outPath, paste0("tacsatNotMerged", year, ".RData"))
  )
  
  # Subset 'tacsatp' where 'FT_REF' does not equal 0 (merged)
  tacsatp <- subset(tacsatp, FT_REF != 0)
  
  #'----------------------------------------------------------------------------
  # 2.1.3 For multi gear/metier etc trips, divide the pings to the right gear/metier etc. ----
  #'----------------------------------------------------------------------------

  tacsatpa_LE_GEAR <- trip_assign(tacsatp, eflalo, col = "LE_GEAR", trust_logbook = T)
  tacsatp <- rbindlist(list(tacsatp[tacsatp$FT_REF %!in% tacsatpa_LE_GEAR$FT_REF,], tacsatpa_LE_GEAR), fill = T)
  
  tacsatpa_LE_MSZ <- trip_assign(tacsatp, eflalo, col = "LE_MSZ", trust_logbook = T)
  tacsatp <- rbindlist(list(tacsatp[tacsatp$FT_REF %!in% tacsatpa_LE_MSZ$FT_REF,], tacsatpa_LE_MSZ), fill = T)
  
  tacsatpa_LE_RECT <- trip_assign(tacsatp, eflalo, col = "LE_RECT", trust_logbook = T)
  tacsatp <- rbindlist(list(tacsatp[tacsatp$FT_REF %!in% tacsatpa_LE_RECT$FT_REF,], tacsatpa_LE_RECT), fill = T)
  
  tacsatpa_LE_MET <- trip_assign(tacsatp, eflalo, col = "LE_MET", trust_logbook = T)
  tacsatp <- rbindlist(list(tacsatp[tacsatp$FT_REF %!in% tacsatpa_LE_MET$FT_REF,], tacsatpa_LE_MET), fill = T)
  
  if("LE_WIDTH" %in% names(eflalo)){
    tacsatpa_LE_WIDTH <- trip_assign(tacsatp, eflalo, col = "LE_WIDTH", trust_logbook = T)
    tacsatp <- rbindlist(list(tacsatp[tacsatp$FT_REF %!in% tacsatpa_LE_WIDTH$FT_REF,], tacsatpa_LE_WIDTH), fill = T)
  }
  
  #Set catch date to be equal to SI_DATE 
  tacsatp$LE_CDAT <- tacsatp$SI_DATE
  
  # Save 'tacsatp' to a file named "tacsatMerged<year>.RData" in the 'outPath' directory
  save(
    tacsatp,
    file = file.path(outPath, paste0("tacsatMerged", year, ".RData"))
  )
  
  
  #'----------------------------------------------------------------------------
  # 2.1.4 Define activity                                                   ----
  #'----------------------------------------------------------------------------
  # Calculate time interval between points
  tacsatp <- intvTacsat(tacsatp, level = "trip", fill.na = TRUE)
  
  # Reset values that are simply too high to 2x the regular interval rate  
  tacsatp$INTV[tacsatp$INTV > intvThres ] <- 2 * intvThres
  
  # Assume that pings with NA in INTV has the normal interval value
  tacsatp$INTV[is.na(tacsatp$INTV)] <- intvThres
 
  # Remove points with NA's in them in critical places
  idx <-
    which(
      is.na(tacsatp$VE_REF) == TRUE |
        is.na(tacsatp$SI_LONG) == TRUE |
        is.na(tacsatp$SI_LATI) == TRUE |
        is.na(tacsatp$SI_DATIM) == TRUE |
        is.na(tacsatp$SI_SP) == TRUE
    )
  if (length(idx) > 0) {
    tacsatp <- tacsatp[-idx, ]
  }
  
  
  # Define speed thresholds associated with fishing for gears
  # Create speed threshold object # 
  t1 <- data.table(tacsatp)
  
  #Remove duplicates
  t1 <- unique(t1)
  # Optional: Control step for metiers
  # table(t1$LE_MET)
  
  # start by making a list of fleet segments from lvl 5 metiers
  t1$LE_SEG <-  sapply(strsplit(t1$LE_MET, "_"), function(x) paste(x[1:2], collapse = "_"))  
  
  # Change wrongly assigned TBB_DEF (with small mesh size) to TBB_CRU
  t1[(t1$LE_SEG %in% c("TBB_CRU", "TBB_DEF") & t1$LE_MSZ < 40) |
       (t1$LE_SEG == "TBB_DEF" & is.na(t1$LE_MSZ)), "LE_SEG"] <- "TBB_CRU"
  
  # Change wrongly assigned TBB_CRU (with large mesh size) to TBB_DEF
  t1[(t1$LE_SEG %in% c("TBB_CRU", "TBB_DEF") & t1$LE_MSZ >= 40) |
       (t1$LE_SEG == "TBB_CRU" & is.na(t1$LE_MSZ)), "LE_SEG"] <- "TBB_DEF"
  
  # Add special speed thresholds for some shrimp metiers
  t1[t1$LE_MET == "OTB_CRU_40-59_0_0", "LE_SEG"] <- "OTB_CRU_40-59_0_0"
  t1[t1$LE_MET == "OTB_CRU_32-69_0_0", "LE_SEG"] <- "OTB_CRU_32-69_0_0"
  t1[t1$LE_MET == "OTB_CRU_16-31_0_0", "LE_SEG"] <- "OTB_CRU_16-31_0_0"
  t1[t1$LE_MET == "OTB_DEF_32-69_0_0", "LE_SEG"] <- "OTB_DEF_32-69_0_0"
  
  # And also for one herring and blue whiting metier
  t1[t1$LE_MET == "OTM_SPF_32-69_0_0", "LE_SEG"] <- "OTM_SPF_32-69_0_0"
  
  # Group some metiers into lvl4
  # First see what you have
  table(t1$LE_SEG)
  t1[LE_SEG %like% "FPN", LE_SEG := "FPN"]
  t1[LE_SEG %like% "FPO", LE_SEG := "FPO"]
  t1[LE_SEG %like% "GNS", LE_SEG := "GNS"]
  t1[LE_SEG %like% c("GNS|GNC|GND"), LE_SEG := "GNS"]
  t1[LE_SEG %like% c("LHP|LLD|LLS|LH"), LE_SEG := "LL"]
  t1[LE_SEG %like% c("MIS"), LE_SEG := "MIS"]
  t1[LE_SEG %like% c("SDN"), LE_SEG := "SDN"]
  t1[LE_SEG %like% c("SSC"), LE_SEG := "SSC"]
  
  # Optional: Control if segments are logical
  # table(t1$LE_SEG)
  
  tacsatp <- data.frame(t1)
  
  saveRDS(tacsatp, paste0(outPath, "tacsatp_", year, ".rds"))
  
  tp <- data.table(tacsatp)[,.(year = year(SI_DATIM), .N), by = .(LE_MSZ, LE_MET, LE_GEAR, LE_SEG, SI_SP)]
                          
  # Optional: Control if table is logical
  # View(tp)
  
  saveRDS(tp, paste0(outPath, "tp_", year, ".rds"))
  
  # Signal end of run for year
  message(paste0("Linking tacsat and eflalo completed for year ", year))
  }
  
# Housekeeping
rm(eflalo, tacsat, tp, t1)
rm(list = ls(pattern = "tacsatp"))

#'------------------------------------------------------------------------------
# End of script                                                             
#'------------------------------------------------------------------------------

