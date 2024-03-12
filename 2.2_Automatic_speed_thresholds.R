#'------------------------------------------------------------------------------
#
# Script to extract and process VMS and logbook data for ICES VMS data call
# 2.2: Automatic speed thresholds                                           ----
# 
# This script is only necessary when you do NOT want to use fixed speeds
# for the determination of activity of vessels
#
#'------------------------------------------------------------------------------

#- Set the gear names for which automatic fishing activity is wanted
#  It is important to fill out the gears you want to apply auto detection for
autoDetectionGears        <- c("TBB","OTB","OTT","SSC","SDN","DRB","PTB","HMD", "MIS")

# Analyse activity automated for common gears only. Use the speedarr for the other gears =============== 
year <- 2023
for(year in yearsToSubmit){
  
  print(year)
  
  
tacsatp <- readRDS(paste0(outPath, "tacsatp_", year, ".rds"))
load(file = paste0(outPath,paste0("/cleanEflalo",year,".RData")) )

subTacsat <- subset(tacsatp, LE_GEAR %in% autoDetectionGears)
nonsubTacsat <- subset(tacsatp, !LE_GEAR %in% autoDetectionGears)

if (visualInspection == TRUE)  {
  storeScheme <-
    activityTacsatAnalyse(
      subTacsat,
      units = "year",
      analyse.by = "LE_SEG",
      identify = "means")
} else{
  storeScheme <-
    expand.grid(
      years = year,
      months = 0,
      weeks = 0,
      analyse.by = unique(subTacsat[,"LE_SEG"])
    )
  
  storeScheme$peaks <- NA
  storeScheme$means <- NA
  storeScheme$fixPeaks <- FALSE
  storeScheme$sigma0 <- 0.911
  
  
  # Fill the storeScheme values based on analyses of the pictures = 
  
  storeScheme$LE_GEAR <- sapply(strsplit(as.character(storeScheme$analyse.by), "_"), `[`, 1)
  
  # Define mean values of the peaks and the number of peaks when they are different from 5 # 
  
  
  storeScheme$means[which(storeScheme$LE_GEAR == "TBB")] <- c("-11.5 -6 0 6 11.5")
  storeScheme$means[which(storeScheme$LE_GEAR == "OTB")] <- c("-9 -3 0 3 9")
  storeScheme$means[which(storeScheme$LE_GEAR == "OTT")] <- c("-9 -3 0 3 9")
  storeScheme$means[which(storeScheme$LE_GEAR == "MIS")] <- c("-9 -3 0 3 9")
  storeScheme$means[which(storeScheme$LE_GEAR == "SSC")] <- c("-9 0 9")
  storeScheme$means[which(storeScheme$LE_GEAR == "PTB")] <- c("-10 -3 0 3 10")
  storeScheme$means[which(storeScheme$LE_GEAR == "DRB")] <- c("-10 0 10")
  storeScheme$means[which(storeScheme$LE_GEAR == "HMD")] <- c("-9 0 9")
  storeScheme$peaks[which(storeScheme$LE_GEAR == "SSC")] <- 3
  storeScheme$peaks[which(storeScheme$LE_GEAR == "DRB")] <- 3
  storeScheme$peaks[which(storeScheme$LE_GEAR == "HMD")] <- 3
  storeScheme$peaks[which(is.na(storeScheme$peaks) == TRUE)] <- 5
  storeScheme <- storeScheme[,-(dim(storeScheme)[2])]
}

#  acTa <- ac.tac.anal(subTacsat, units = "year", storeScheme = storeScheme, analyse.by = "LE_SEG", identify = "peaks")

acTa <-
  act.tac(
    subTacsat,
    units = "year",
    analyse.by = "LE_SEG",
    storeScheme = storeScheme,
    plot = FALSE,
    level = "all")
subTacsat$SI_STATE <- acTa
subTacsat$ID <- 1:nrow(subTacsat)

# Check results, and if results are not satisfactory, run analyses again but now with fixed peaks # 

for (iGear in autoDetectionGears) {
  subDat <- subset(subTacsat,LE_GEAR == iGear)
  minS <-
    min(
      subDat$SI_SP[which(subDat$SI_STATE == "s")],
      na.rm = TRUE)
  minF <-
    min(subDat$SI_SP[which(subDat$SI_STATE == "f")],
        na.rm = TRUE)
  if(minS < minF) {
    storeScheme$fixPeaks[which(storeScheme$analyse.by == iGear)] <- TRUE
    subacTa <-
      activityTacsat(
        subDat,
        units = "year",
        analyse.by = "LE_GEAR",
        storeScheme,
        plot = FALSE,
        level = "all"
      )
    subTacsat$SI_STATE[subDat$ID] <- subacTa
  }
}
subTacsat <-
  subTacsat[,
            -rev(grep("ID", colnames(subTacsat)))[1]
  ]

# Assign for visually inspected gears a simple speed rule classification =============== 



metiers <- unique(nonsubTacsat$LE_GEAR)
nonsubTacsat$SI_STATE <- NA
for (mm in metiers) {
  nonsubTacsat$SI_STATE[
    nonsubTacsat$LE_GEAR == mm &
      nonsubTacsat$SI_SP >= speedarr[speedarr$LE_GEAR == mm, "min"] &
      nonsubTacsat$SI_SP <= speedarr[speedarr$LE_GEAR == mm, "max"]
  ] <- "f";
}
nonsubTacsat$SI_STATE[
  nonsubTacsat$LE_GEAR == "NA" &
    nonsubTacsat$SI_SP >= speedarr[speedarr$LE_GEAR == "MIS", "min"] &
    nonsubTacsat$SI_SP <= speedarr[speedarr$LE_GEAR == "MIS", "max"]
] <- "f"
nonsubTacsat$SI_STATE[ is.na(nonsubTacsat$SI_STATE) ] <- "s"


# ??? Save somthing

# Combine the two dataset together again =============== 

}

#'------------------------------------------------------------------------------
# End of script                                                             
#'------------------------------------------------------------------------------
