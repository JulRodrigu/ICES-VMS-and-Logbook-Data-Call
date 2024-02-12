if (!require("pacman")) install.packages("pacman")
p_load(data.table, dplyr, sf, mapview, stars, raster, terra, leaflet, leafem)

'%!in%' <- function(x,y)!('%in%'(x,y))

load(file = paste0(outPath, "table1.RData")  )

# lst <- paste0(outPath, "tacsatEflalo_", 2009:2023, ".rds")
# te <- rbindlist(lapply(lst, readRDS), fill = T)


grd_size <- 0.05


table1$SI_LONG <- round(CSquare2LonLat(table1$Csquare ,grd_size)$SI_LONG,2)
table1$SI_LATI <- round(CSquare2LonLat(table1$Csquare ,grd_size)$SI_LATI,2)


t1 <- data.table(table1)

table(t1[!is.na(SA_M2)]$LE_MET)

# start by making a list of fleet segments from lvl 5 metiers
t1$LE_SEG <-  sapply(strsplit(t1$LE_MET, "_"), function(x) paste(x[1:2], collapse = "_"))  

# Change wrongly assigned TBB_DEF (with small mesh size) to TBB_CRU
t1[(t1$LE_SEG %in% c("TBB_CRU", "TBB_DEF") & t1$LE_MSZ < 40) |
          (t1$LE_SEG == "TBB_DEF" & is.na(t1$LE_MSZ)), "LE_SEG"] <- "TBB_CRU"

# Change wrongly assigned TBB_CRU (with large mesh size) to TBB_DEF
t1[(t1$LE_SEG %in% c("TBB_CRU", "TBB_DEF") & t1$LE_MSZ >= 40) |
          (t1$LE_SEG == "TBB_CRU" & is.na(t1$LE_MSZ)), "LE_SEG"] <- "TBB_DEF"

#Add special speed thresholds for some shrimp metiers
t1[t1$LE_MET == "OTB_CRU_40-59_0_0", "LE_SEG"] <- "OTB_CRU_40-59_0_0"
t1[t1$LE_MET == "OTB_CRU_32-69_0_0", "LE_SEG"] <- "OTB_CRU_32-69_0_0"
t1[t1$LE_MET == "OTB_CRU_16-31_0_0", "LE_SEG"] <- "OTB_CRU_16-31_0_0"
t1[t1$LE_MET == "OTB_DEF_32-69_0_0", "LE_SEG"] <- "OTB_DEF_32-69_0_0"

#And also for one herring and blue whiting metier
t1[t1$LE_MET == "OTM_SPF_32-69_0_0", "LE_SEG"] <- "OTM_SPF_32-69_0_0"


#Group some metiers into lvl4
table(t1$LE_SEG)
t1[LE_SEG %like% "FPN", LE_SEG := "FPN"]
t1[LE_SEG %like% "FPO", LE_SEG := "FPO"]
t1[LE_SEG %like% "GNS", LE_SEG := "GNS"]
t1[LE_SEG %like% c("GNS|GNC|GND"), LE_SEG := "GNS"]
t1[LE_SEG %like% c("LHP|LLD|LLS"), LE_SEG := "LL"]
t1[LE_SEG %like% c("MIS"), LE_SEG := "MIS"]
t1[LE_SEG %like% c("SDN"), LE_SEG := "SDN"]
t1[LE_SEG %like% c("SSC"), LE_SEG := "SSC"]

###############


table(t1$LE_SEG)

i <- t1$LE_SEG[1]
i <- "OTB_DEF_32-69_0_0"
i <- "OTB_CRU"

## VALUE plots
for(i in unique(t1$LE_SEG)){
  cat("")
  print(i)
  sub <- t1[LE_SEG == i & !is.na(LE_EURO_TOT) & LE_EURO_TOT != 0]
  
  if(nrow(sub)==0)
    next
  
  #change to wide format
  sa <- dcast(sub, SI_LONG+SI_LATI~Year, value.var = "LE_EURO_TOT", fun = sum)
  
  sa[sa == 0] <- NA
  
  pts <- sa %>%
    sf::st_as_sf(coords = c("SI_LONG","SI_LATI")) %>%
    sf::st_set_crs(4326)
  
  rast <- st_rasterize(pts, dx = grd_size, dy = grd_size,
                       xlim = c(st_bbox(pts)[1] -grd_size/2, st_bbox(pts)[3] +grd_size/2),
                       ylim = c(st_bbox(pts)[2] -grd_size/2, st_bbox(pts)[4] +grd_size/2) 
  )
  rast$cellsize <- terra::cellSize(rast(rast), unit="m")
  r <- rast(rast)
  rb <- raster::brick(r)
  
  risk.bins <-c(0,10, 50, 100, 500, 1000, 2000, 5000, 10000, 50000, 100000000)
  pal <- colorBin("magma", bins = risk.bins, na.color = "transparent")
  
  m <- leaflet() |> 
    addProviderTiles(providers$Esri.WorldGrayCanvas)  |> 
    addProviderTiles(providers$OpenSeaMap, options = providerTileOptions(
      updateWhenZooming = FALSE,      # map won't update tiles until zoom is done
      updateWhenIdle = TRUE           # map won't load new tiles when panning
    )) 
  
  y <- 2009
  ys <- sort(unique(sub$Year))
  for(y in ys){
    cat(paste(y, ""))
    
    
    rb[[paste0("X", y)]] <- rb[[paste0("X", y, "_", y)]] / rb$cellsize_cellsize * 1000000
    
    m <- m |> 
      addRasterImage(rb[[paste0("X", y)]], colors = pal, project = TRUE, group = as.character(y),
                     layerId = as.character(y)) |> 
      addImageQuery(rb[[paste0("X", y)]], project = TRUE,
                    layerId = as.character(y))
    
  }
  
  m <- m |> 
    addLayersControl(baseGroups = ys,
                     options = layersControlOptions(collapsed = F)) %>%
    addLegend(pal=pal, values = values(r), title = paste0("Value (EURO) / km2 - ", i))
  
  # m
  
  htmlwidgets::saveWidget	(m, file=paste0(plotPath, "Value_", i, ".html"))
}

## Effort plots
for(i in unique(t1$LE_SEG)){
  cat("")
  print(i)
  sub <- t1[LE_SEG == i & !is.na(INTV) & INTV != 0]
  
  if(nrow(sub)==0)
    next
  
  #change to wide format
  sa <- dcast(sub, SI_LONG+SI_LATI~Year, value.var = "INTV", fun = sum)
  
  sa[sa == 0] <- NA
  
  pts <- sa %>%
    sf::st_as_sf(coords = c("SI_LONG","SI_LATI")) %>%
    sf::st_set_crs(4326)
  
  rast <- st_rasterize(pts, dx = grd_size, dy = grd_size,
                       xlim = c(st_bbox(pts)[1] -grd_size/2, st_bbox(pts)[3] +grd_size/2),
                       ylim = c(st_bbox(pts)[2] -grd_size/2, st_bbox(pts)[4] +grd_size/2) 
  )
  rast$cellsize <- terra::cellSize(rast(rast), unit="m")
  r <- rast(rast)
  rb <- raster::brick(r)
  
  risk.bins <-c(0,1,2,5,10,20,50,100,500,1000,5000, 1000000)
  pal <- colorBin("magma", bins = risk.bins, na.color = "transparent")
  
  m <- leaflet() |> 
    addProviderTiles(providers$Esri.WorldGrayCanvas)  |> 
    addProviderTiles(providers$OpenSeaMap, options = providerTileOptions(
      updateWhenZooming = FALSE,      # map won't update tiles until zoom is done
      updateWhenIdle = TRUE           # map won't load new tiles when panning
    )) 
  
  y <- 2009
  ys <- sort(unique(sub$Year))
  for(y in ys){
    cat(paste(y, ""))
    
    
    rb[[paste0("X", y)]] <- rb[[paste0("X", y, "_", y)]] / rb$cellsize_cellsize * 1000000 * 60
    
    m <- m |> 
      addRasterImage(rb[[paste0("X", y)]], colors = pal, project = TRUE, group = as.character(y),
                     layerId = as.character(y)) |> 
      addImageQuery(rb[[paste0("X", y)]], project = TRUE,
                    layerId = as.character(y))
    
  }
  
  m <- m |> 
    addLayersControl(baseGroups = ys,
                     options = layersControlOptions(collapsed = F)) %>%
    addLegend(pal=pal, values = values(r), title = paste0("Effort (minutes) / km2 - ", i))
  
  # m
  
  htmlwidgets::saveWidget	(m, file=paste0(plotPath, "Effort_", i, ".html"))
}



  ## SAR plots
  for(i in unique(t1$LE_SEG)){
    cat("")
    print(i)
    sub <- t1[LE_SEG == i & !is.na(SA_M2) & SA_M2 != 0]
    
  if(nrow(sub)==0)
    next
  
  #change to wide format
  sa <- dcast(sub, SI_LONG+SI_LATI~Year, value.var = "SA_M2", fun = sum)
  
  sa[sa == 0] <- NA
  
  pts <- sa %>%
    sf::st_as_sf(coords = c("SI_LONG","SI_LATI")) %>%
    sf::st_set_crs(4326)
  
  rast <- st_rasterize(pts, dx = grd_size, dy = grd_size,
                       xlim = c(st_bbox(pts)[1] -grd_size/2, st_bbox(pts)[3] +grd_size/2),
                       ylim = c(st_bbox(pts)[2] -grd_size/2, st_bbox(pts)[4] +grd_size/2) 
  )
  rast$cellsize <- terra::cellSize(rast(rast), unit="m")
  r <- rast(rast)
  rb <- raster::brick(r)
  
  risk.bins <-c(0,0.1,0.2,0.5,1,2,5,10,20,50,10000)
  pal <- colorBin("magma", bins = risk.bins, na.color = "transparent")
  
  m <- leaflet() |> 
    addProviderTiles(providers$Esri.WorldGrayCanvas)  |> 
    addProviderTiles(providers$OpenSeaMap, options = providerTileOptions(
      updateWhenZooming = FALSE,      # map won't update tiles until zoom is done
      updateWhenIdle = TRUE           # map won't load new tiles when panning
    )) 
  
  y <- 2009
  ys <- sort(unique(sub$Year))
  for(y in ys){
    cat(paste(y, ""))
    
    
    rb[[paste0("X", y)]] <- rb[[paste0("X", y, "_", y)]] / rb$cellsize_cellsize
    
    m <- m |> 
      addRasterImage(rb[[paste0("X", y)]], colors = pal, project = TRUE, group = as.character(y),
                     layerId = as.character(y)) |> 
      addImageQuery(rb[[paste0("X", y)]], project = TRUE,
                    layerId = as.character(y))
    
  }
  
  m <- m |> 
    addLayersControl(baseGroups = ys,
                     options = layersControlOptions(collapsed = F)) %>%
    addLegend(pal=pal, values = values(r), title = paste0("Swept area ratio (SAR) - ", i))
  
  m
  
  htmlwidgets::saveWidget	(m, file=paste0(plotPath, "SAR_", i, ".html"))
  
}


m3


m <- m |> 
  addRasterImage(rb[[paste0("X", y)]], colors = pal, project = TRUE, group = "RAS",
                 layerId = as.character(y)) |> 
  addImageQuery(rb[[paste0("X", y)]], project = TRUE,
                layerId = as.character(y))

}

subtey <- subte[Year == y]
m2 <- m |> 
  addCircleMarkers(subte$SI_LONG, subte$SI_LATI, group = "POINTS", 
                   layerId = as.character(y))

subtey <- subte[Year == y]
m2 <- m |> 
  addCircleMarkers(lng = subte$SI_LONG, lat = subte$SI_LATI, group = "POINTS", 
                   layerId = subte$Year) |> 
  groupOptions("POINTS", zoomLevels = 11:20)  

m2
|> 
  
                       
m3 <- m2 |> 
  addLayersControl(baseGroups = "2009",
                   overlayGroups = c("POINTS", "RAS"),
                   options = layersControlOptions(collapsed = F)) %>%
  addLegend(pal=pal, values = values(r), title = "Swept area ratio (SAR)") 

m3

m3 <- m2 |> 
     addLayersControl(baseGroups = c("2009", "2010"),
                      overlayGroups = c("POINTS", "RAS"),
                      options = layersControlOptions(collapsed = F)) %>%
     addLegend(pal=pal, values = values(r), title = "Swept area ratio (SAR)") 

m3


m <-
  leaflet(sf) %>%
  addTiles() %>%
  addProviderTiles("OpenSeaMap",
                   options = providerTileOptions(minzoom = 0, maxzoom = 10)) |>
  addCircles(popup = paste(sf$log, "speed =", sf$speed)) |> 
  addPolylines(data = l2, color = "yellow") |> 
  addPolylines(data = l, color = "green", popup = paste("haulid =", l$haulid, "length_m =", as.numeric(l$lenght_m)))   

addCircles(subte[subte$Year == y,] popup = paste(sf$log, "speed =", sf$speed))

rb[,paste0("X", 2009)]
rb[[paste0("X", 2010)]]
rb[["X2010"]]


  addRasterImage(rb[[1]], colors = pal, project = TRUE, group = names(rast)[1],
                 layerId = names(rast)[1]) %>%
  addImageQuery(rb[[1]], project = TRUE,
                layerId = names(rast)[1]) %>%
  addRasterImage(rb[[2]], colors = pal, project = TRUE, group = names(rast)[2],
                 layerId = names(rast)[2]) %>%
  addImageQuery(rb[[2]], project = TRUE,
                layerId = names(rast)[2]) %>%
  addRasterImage(rb[[3]], colors = pal, project = TRUE, group = names(rast)[3],
                 layerId = names(rast)[3]) %>%
  addImageQuery(rb[[3]], project = TRUE,
                layerId = names(rast)[3]) %>%
  addRasterImage(rb[[4]], colors = pal, project = TRUE, group = names(rast)[4],
                 layerId = names(rast)[4]) %>%
  addImageQuery(rb[[4]], project = TRUE,
                layerId = names(rast)[4]) %>%
  addRasterImage(rb[[5]], colors = pal, project = TRUE, group = names(rast)[5],
                 layerId = names(rast)[5]) %>%
  addImageQuery(rb[[5]], project = TRUE,
                layerId = names(rast)[5]) %>%
  addRasterImage(rb[[6]], colors = pal, project = TRUE, group = names(rast)[6],
                 layerId = names(rast)[6]) %>%
  addImageQuery(rb[[12]], project = TRUE,
                layerId = names(rast)[12]) %>%
  addRasterImage(rb[[7]], colors = pal, project = TRUE, group = names(rast)[7],
                 layerId = names(rast)[7]) %>%
  addImageQuery(rb[[7]], project = TRUE,
                layerId = names(rast)[7]) %>%
  addRasterImage(rb[[8]], colors = pal, project = TRUE, group = names(rast)[8],
                 layerId = names(rast)[8]) %>%
  addImageQuery(rb[[8]], project = TRUE,
                layerId = names(rast)[8]) %>%
  addRasterImage(rb[[9]], colors = pal, project = TRUE, group = names(rast)[9],
                 layerId = names(rast)[9]) %>%
  addImageQuery(rb[[9]], project = TRUE,
                layerId = names(rast)[9]) %>%
  addRasterImage(rb[[10]], colors = pal, project = TRUE, group = names(rast)[10],
                 layerId = names(rast)[10]) %>%
  addImageQuery(rb[[10]], project = TRUE,
                layerId = names(rast)[10]) %>%
  addRasterImage(rb[[11]], colors = pal, project = TRUE, group = names(rast)[11],
                 layerId = names(rast)[11]) %>%
  addImageQuery(rb[[11]], project = TRUE,
                layerId = names(rast)[11]) %>%
  addRasterImage(rb[[12]], colors = pal, project = TRUE, group = names(rast)[12],
                 layerId = names(rast)[12]) %>%
  addImageQuery(rb[[12]], project = TRUE,
                layerId = names(rast)[12]) %>%
  addRasterImage(rb[[13]], colors = pal, project = TRUE, group = names(rast)[13],
                 layerId = names(rast)[13]) %>%
  addImageQuery(rb[[13]], project = TRUE,
                layerId = names(rast)[13]) %>%
  addLayersControl(baseGroups = c(names(rast)[c(13, 1:12)]),
                   options = layersControlOptions(collapsed = F)) %>%
  addLegend(pal=pal, values = values(r), title = "Jomfruhummer / Total landinger (%)") |> 
  addSimpleGraticule(
    showOriginLabel = TRUE,
    redraw = "move",
    hidden = FALSE,
    zoomIntervals = list(
      list(start = 1, end = 3, interval = 10),
      list(start = 4, end = 7, interval = 1),
      list(start = 8, end = 11, interval = 0.1),
      list(start = 12, end = 15, interval = 0.01),
      list(start = 16, end = 20, interval = 0.001)
    ),
    layerId = NULL,
    group = NULL
  )

saveWidget(m, file="//ait-pdfs.win.dtu.dk/Qdrev/AQUA/dfad/users/jepol/home/24-02-05_Rikke_Jomfruhummer/Maps/pct.html")
ys <- 2009:2023
x <- paste0("rb$X", ys, " <- rb$X", ys, "_", ys, " / rb$cellsize_cellsize", sep = "\n")
cat(x)

