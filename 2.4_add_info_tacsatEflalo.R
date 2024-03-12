#'------------------------------------------------------------------------------
#
# Script to extract and process VMS and logbook data for ICES VMS data call
# 2.4: Add information to TacsatEflalo                                      ----
# You can work with clean TacsatEflalo files that we have constructed 
# in previous scripts, or direct to your own merged and clean files.
#
#'------------------------------------------------------------------------------

#'------------------------------------------------------------------------------
# 2.4.1 Create / download EUSeaMap habitat layer                            ----
#'------------------------------------------------------------------------------
# The first section is optional, since this has already been done by jepol, and the resulting 
# rds file can be downloaded directly from the dtu server. 

# # Be sure that the download does not timeout before it is finished
# options(timeout = max(5000, getOption("timeout")))
# 
# # Download zip file to folder
# url <- "https://files.emodnet-seabedhabitats.eu/data/EUSeaMap_2023.zip"
# zipF <- paste0(outPath, "eusm.zip")
# download.file(url, zipF)
# sf_use_s2(F)
# #unzip and load into R Environment
# unzip(zipF, exdir=outPath) 
# 
# data(ICESareas)
# 
# ICESareas <- ICESareas |> 
#   st_zm() |> 
#   st_make_valid()
# 
# eusm <- st_read(paste0(outPath, "EUSeaMap_2023.gdb"), layer = "EUSeaMap_2023")
# 
# eusm <- eusm |> 
#   st_zm() |> 
#   select(all_of("MSFD_BBHT")) |> 
#   st_crop(ICESareas)
# 
# st_geometry(eusm) <- "geometry"
# 
# eusm <- eusm |> 
#   group_by(MSFD_BBHT) |> 
#   summarize(geometry = st_union(geometry))
# 
# saveRDS(eusm, paste0(outPath, "eusm.rds"))

######## Download directly from dtu server - https://figshare.com/s/46ed591ca29f87c53311 
######## and save the eusm.rds and ICES_GEBCO.rds file to your outPath

#Load the file into R
eusm <- readRDS(paste0(outPath, "eusm.rds"))
bathy <- readRDS(paste0(outPath, "ICES_GEBCO.rds"))


#'------------------------------------------------------------------------------
# 2.4.2 Add information to tacsatEflalo                                     ----
#'------------------------------------------------------------------------------
# If you already have cleaned tacsatEflalo files elsewhere, 
# change file location below, and make sure data is called tacsatEflalo
# Loop trough years to submit
for(year in yearsToSubmit){
  print(paste0("Start loop for year ",year))
  load(file = paste0(outPath,"tacsatEflalo",year,".RData"))
  
# Add habitat to the tacsatEflalo file
  tacsatEflalo <- tacsatEflalo |> 
    sf::st_as_sf(coords = c("SI_LONG", "SI_LATI"), remove = F) |> 
    sf::st_set_crs(4326) |> 
    st_join(eusm, join = st_intersects) |> 
    st_join(bathy, join = st_intersects) |> 
    mutate(geometry = NULL) |> 
    data.frame()
  

# Calculate the c-square based on longitude and latitude
tacsatEflalo$Csquare <- CSquare(tacsatEflalo$SI_LONG, tacsatEflalo$SI_LATI, degrees = 0.05)

# Extract the year and month from the date-time
tacsatEflalo$Year <- year(tacsatEflalo$SI_DATIM)
tacsatEflalo$Month <- month(tacsatEflalo$SI_DATIM)

# Calculate the kilowatt-hour and convert interval to hours
tacsatEflalo$kwHour <- tacsatEflalo$VE_KW * tacsatEflalo$INTV / 60
tacsatEflalo$INTV_h <- tacsatEflalo$INTV / 60

# Add the calculated gear width to each fishing point
tacsatEflalo$GEARWIDTH <- add_gearwidth(tacsatEflalo, met_name = "LE_MET", oal_name = "VE_LEN", kw_name = "VE_KW")

# Add swept area(m2) for each point in the tacsateflalo
tacsatEflalo$SA_M2 <- tacsatEflalo$SI_GEARWIDTH * tacsatEflalo$INTV_h * tacsatEflalo$SI_SP * 1852

# Check if logical
tacsatEflalo[,.(min = min(GEARWIDTH), max = max(GEARWIDTH)), by = .(LE_MET)]

saveRDS(data.frame(tacsatEflalo), paste0(outPath, "tacsatEflalo_", year, ".rds"))
}

rm(tacsatEflalo)
#'------------------------------------------------------------------------------
# End of script                                                             
#'------------------------------------------------------------------------------

