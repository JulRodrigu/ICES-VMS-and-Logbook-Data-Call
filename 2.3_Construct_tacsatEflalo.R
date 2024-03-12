#'------------------------------------------------------------------------------
#
# Script to extract and process VMS and logbook data for ICES VMS data call
# 2.3: Constructing TacsatEflalo                                            ----
# This script uses fixed speed thresholds
#
#'------------------------------------------------------------------------------

#'------------------------------------------------------------------------------
# 2.3.0 Visualize speed distribution per segment for non-common gears       ----
#'------------------------------------------------------------------------------

# Load summary for all fleet segments and plot them by segment
tps <- rbindlist(lapply(paste0(outPath, "tp_", yearsToSubmit, ".rds"), readRDS))  
 
#By segment
his <- tps[,.(N = sum(N)), by = .(SI_SP = round(SI_SP/0.2)*0.2, LE_SEG)]
his <- tps[year %in% 2020:2023,.(N = sum(N)), by = .(LE_SEG, SI_SP)]
setorder(his, LE_SEG, SI_SP)

his[LE_SEG == i & SI_SP < 8]

  i <- "PTB_CRU"
  i <- "OTB_MCD" 
  i <- "PTM_SPF"
  for(i in unique(tps$LE_SEG)){
    cat(i)
    # ggplot(his[LE_SEG == i & SI_SP < 8], aes(x = SI_SP, y = N)) + 
    #   geom_line(aes(color=LE_SEG))+
    #     scale_x_continuous(breaks = seq(0, 8, by = 1))
    # ggplot(tps[LE_SEG == i & SI_SP < 8],aes(x=SI_SP,y=N))+
    #   geom_bar(stat='identity')+
    #   ggtitle(paste(i)) 
    ggplot(tps[LE_SEG == i & SI_SP < 8],aes(x=SI_SP,y=N))+
      geom_col()+
      ggtitle(paste(i)) 
    
    
    ggsave(paste0(plotPath, i, ".png"), dpi = 400,
           width = 15,
           height = 10,
           units = c("cm"))
  }
  
  
  # Create a data frame with minimum and maximum speed thresholds for each gear - look at the speed histograms in the plotPath
  speedarr <- data.frame(LE_SEG = sort(unique(tps$LE_SEG)),
                         min = 2,
                         max = 4
  )
  
  # Fill out the minimum and maximum speed thresholds
  fix(speedarr)
  
  # speedarr <- structure(list(LE_SEG = c("DRB_MOL", "FPN_CAT", "FPN_CRU", "FPN_DEF",
  #                                       "FPO_CRU", "FPO_DEF", "FPO_MOL", "GNC_DEF", "GND_SPF", "GNS_ANA",
  #                                       "GNS_CRU", "GNS_DEF", "GNS_SPF", "LHP_DEF", "LHP_SPF", "LLD_ANA",
  #                                       "LLS_DEF", "MIS_DEF", "OTB_CRU", "OTB_CRU_16-31_0_0", "OTB_CRU_32-69_0_0",
  #                                       "OTB_DEF", "OTB_DEF_32-69_0_0", "OTB_DWS", "OTB_MCD", "OTB_SPF",
  #                                       "OTM_DEF", "OTM_SPF", "OTM_SPF_32-69_0_0", "PS_SPF", "PTB_CRU",
  #                                       "PTB_DEF", "PTB_SPF", "PTM_DEF", "PTM_SPF", "SDN_DEF", "SDN_SPF",
  #                                       "SSC_DEF", "TBB_CRU", "TBB_DEF"),
  #                            min = c(2, 2, 2, 2, 2, 2, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 2, 1.2, 1.5, 2, 1.5, 2, 2, 2,
  #                                    2, 2, 2.5, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 5),
  #                            max = c(4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 0.1, 0.1, 0.1, 0.1, 4, 4, 2.5, 2.5, 4, 4, 4,
  #                                    4, 4, 4, 4, 5, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 7)),
  #                       row.names = c(NA, 40L), class = "data.frame")
  
  
  
  # saveRDS(speedarr, paste0(outPath, "speedarr.rds"))
  
  
  # Analyse activity automated fo# saveRDS(speedarr, paste0(outPath, "speedarr.rds"))

# 
# #Load speedaar that was created earlier
speedarr <- readRDS(paste0(outPath, "speedarr.rds"))

# Analyse activity automated fo

# Or if you prefer using common speed thresholds
# speedarr <- structure(list(LE_SEG = c("DRB_MOL", "FPN_CAT", "FPN_CRU", "FPN_DEF",
#                                       "FPO_CRU", "FPO_DEF", "FPO_MOL", "GNC_DEF", "GND_SPF", "GNS_ANA",
#                                       "GNS_CRU", "GNS_DEF", "GNS_SPF", "LHP_DEF", "LHP_SPF", "LLD_ANA",
#                                       "LLS_DEF", "MIS_DEF", "OTB_CRU", "OTB_CRU_16-31_0_0", "OTB_CRU_32-69_0_0",
#                                       "OTB_DEF", "OTB_DEF_32-69_0_0", "OTB_DWS", "OTB_MCD", "OTB_SPF",
#                                       "OTM_DEF", "OTM_SPF", "OTM_SPF_32-69_0_0", "PS_SPF", "PTB_CRU",
#                                       "PTB_DEF", "PTB_SPF", "PTM_DEF", "PTM_SPF", "SDN_DEF", "SDN_SPF",
#                                       "SSC_DEF", "TBB_CRU", "TBB_DEF"),
#                            min = c(2, 2, 2, 2, 2, 2, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 2, 1.2, 1.5, 2, 1.5, 2, 2, 2,
#                                    2, 2, 2.5, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 5),
#                            max = c(4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 0.1, 0.1, 0.1, 0.1, 4, 4, 2.5, 2.5, 4, 4, 4,
#                                    4, 4, 4, 4, 5, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 7)),
#                       row.names = c(NA, 40L), class = "data.frame")

# Fill out the minimum and maximum speed thresholds in the data editor 
# (will pop up). Closing the data editor will save data automatically.
# For this, you can use the speed histograms saved in the plotPath
# One decimal is precise enough
fix(speedarr)

saveRDS(speedarr, paste0(outPath, "speedarr.rds"))


#'----------------------------------------------------------------------------
# 2.3.1 Check if correct Metier Level 6 codes only                        ----
#'----------------------------------------------------------------------------

# Get unique metier level 6 codes in files
LE_MET <- unique(tps$LE_MET)

# Get correct metier codes from ICES
m6_ices         <-  getCodeList("Metier6_FishingActivity")

# Check (TRUE are correct codes, FALSE incorrect)
table(LE_MET %in% m6_ices$Key)

# Which ones not correct? filter these out or replace these!
LE_MET[LE_MET %!in% m6_ices$Key]


# loop over years
for(year in yearsToSubmit){
  print(paste0("Start loop for year ",year))
  
  tacsatp <- readRDS(paste0(outPath, "tacsatp_", year, ".rds"))
  load(file = paste0(outPath,paste0("/cleanEflalo",year,".RData")) )
  
  if(!all(tacsatp$LE_SEG %in% speedarr$LE_SEG))
    stop(paste("There is no speed filter for the segement(s):", MIS_SEG), collapse = ", ")
  
  #'----------------------------------------------------------------------------
  # 2.3.1 continued, filter out invalid metier level 6 codes                           
  #'----------------------------------------------------------------------------
    kept <- nrow(tacsatp)
    removed <- nrow(tacsatp %>% filter(LE_MET %!in% m6_ices$Key))
    tacsatp <- tacsatp %>% filter(LE_MET %in% m6_ices$Key)
    cat(sprintf("%.2f%% of of the tacsatp removed due to invalid metier l6 \n", (removed / (removed + kept) * 100)))
  
  #'----------------------------------------------------------------------------
  # 2.3.2 Analyse activity automated for common gears only.                 ----
  # Use the speedarr for the other gears
  #'----------------------------------------------------------------------------
  if(!all(tacsatp$LE_SEG %in% speedarr$LE_SEG)){
    SEG <- unique(tps$LE_SEG) # extracting unique segments
    MIS_SEG <- SEG[SEG %!in% speedarr$LE_SEG] # identifying missing segments
    stop(paste("There is no speed filter for the segement(s):", MIS_SEG, "\n"), collapse = ", ") # If this happens, add them in "speedarr"
  }

    # Add min and max speed of likely fishing speeds to tacsat data
    tacsatp <- tacsatp |> 
      left_join(speedarr, by = "LE_SEG")
    
    # Create column to which state can be assigned
    tacsatp$SI_STATE <- 0
    
    tacsatp[tacsatp$SI_SP >= tacsatp$min &
              tacsatp$SI_SP <= tacsatp$max, "SI_STATE"] <- 1
    
    table(tacsatp$SI_STATE, useNA = "always")
    
    
    message("Defining activity completed")
  

  #'----------------------------------------------------------------------------
  # 2.3.3 Dispatch landings of merged eflalo at the ping scales             ----
  #'----------------------------------------------------------------------------
  
  # Get the indices of columns in eflalo that contain "LE_KG_" or "LE_EURO_"
  idx_kg <- grep("LE_KG_", colnames(eflalo)[colnames(eflalo) %!in% c("LE_KG_TOT")])
  idx_euro <- grep("LE_EURO_", colnames(eflalo)[colnames(eflalo) %!in% c("LE_EURO_TOT")])
  
  # Calculate the total KG and EURO for each row
  if("LE_KG_TOT" %!in% names(eflalo))
    eflalo$LE_KG_TOT <- rowSums(eflalo[, idx_kg], na.rm = TRUE)
  if("LE_EURO_TOT" %!in% names(eflalo))
    eflalo$LE_EURO_TOT <- rowSums(eflalo[, idx_euro], na.rm = TRUE)
  
  # Remove the columns used for the total calculation
  # eflalo <- eflalo[, -c(idx_kg, idx_euro)]
  
  # Split eflalo into two data frames based on the presence of FT_REF in tacsatp
  eflaloNM <- subset(eflalo, !FT_REF %in% unique(tacsatp$FT_REF))
  eflaloM <- subset(eflalo, FT_REF %in% unique(tacsatp$FT_REF))
  
  message(sprintf("%.2f%% of the eflalo data not in tacsat\n", (nrow(eflaloNM) / (nrow(eflaloNM) + nrow(eflaloM))) * 100))
  
  # Filter rows where SI_STATE is 1
  tacsatEflalo <- tacsatp[tacsatp$SI_STATE == 1,] # This is not yet merged data, just named tacsatEflalo
  
  # Check the type of linking required and call splitAmongPings accordingly
  if (!"trip" %in% linkEflaloTacsat) stop("trip must be in linkEflaloTacsat")
  
  if (all(c("day", "ICESrectangle", "trip") %in% linkEflaloTacsat)) {
    level <- "day"
    tmpTa <- tacsatp
    tmpEf <- eflaloM
  } else if (all(c("day","trip") %in% linkEflaloTacsat) & !"ICESrectangle" %in% linkEflaloTacsat) {
    level <- "day"
    tmpTa <- tacsatp
    tmpEf <- eflaloM
    tmpTa$LE_RECT <- "ALL"
    tmpEf$LE_RECT <- "ALL"
  } else if (all(c("ICESrectangle", "trip") %in% linkEflaloTacsat) & !"day" %in% linkEflaloTacsat) {
    level <- "ICESrectangle"
    tmpTa <- tacsatp
    tmpEf <- eflaloM
  } else if (linkEflaloTacsat == "trip" & length(linkEflaloTacsat) == 1) {
    level <- "trip"
    tmpTa <- tacsatp
    tmpEf <- eflaloM
  }
  
  tacsatEflalo <- splitAmongPings(
    tacsat = tmpTa,
    eflalo = tmpEf,
    variable = "all",
    level = level,
    conserve = level != "trip"
  )
  
  
  print("Dispatching landings completed")
  
  eflalo$tripInTacsat <- ifelse(eflalo$FT_REF %in% tacsatEflalo$FT_REF, "Y", "N")
 
  # Save
  save(
    tacsatEflalo,
    file = file.path(outPath, paste0("tacsatEflalo", year, ".RData"))
  )
  
  save(
    eflalo,
    file = file.path(outPath, paste0("/cleanEflalo", year, ".RData"))
  )

  
}


# Housekeeping
rm(his, speedarr, tacsatp, tacsatEflalo,
   tmpEf, tmpTa, tps,
   eflalo, eflaloM, eflaloNM)

#'------------------------------------------------------------------------------
# End of script                                                             
#'------------------------------------------------------------------------------

 