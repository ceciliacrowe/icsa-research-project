#load required packages
library(terra)
library(tidyverse)
library(landscapemetrics)
library(dismo)
library(randomForest)
library(sf)
library(dplyr)
library(ggplot2)
library(viridis)

#load lulc data
lulc_2002 <- vect("/Users/ceciliacrowe/Documents/ICSA/Research Project/lulc_2002/Land_lu_2002.shp")
lulc_2007 <- vect("/Users/ceciliacrowe/Documents/ICSA/Research Project/lulc_2007/Land_lu_2007_gen.shp")
lulc_2012 <- vect("/Users/ceciliacrowe/Documents/ICSA/Research Project/lulc_2012/Land_lu_2012_gen.shp")
lulc_2020 <- vect("/Users/ceciliacrowe/Documents/ICSA/Research Project/lulc_2020/Land_Use_Land_Cover_of_New_Jersey_2020.shp")

#load impervious surface data
is_2002 <- vect( "/Users/ceciliacrowe/Documents/ICSA/Research Project/is_2002/Land_lu_2002_is.gdb", layer = "Land_lu_2002_is"
)
is_2007 <- vect( "/Users/ceciliacrowe/Documents/ICSA/Research Project/is_2007/Land_lu_2007_is.gdb", layer= "Land_lu_2007_is"
)
is_2012 <- vect( "/Users/ceciliacrowe/Documents/ICSA/Research Project/is_2012/Land_lu_2012_is.gdb", layer= "Land_lu_2012_is"
)
is_2020 <- rast(
  "/Users/ceciliacrowe/Documents/ICSA/Research Project/is_2020/nj_2020_ccap_v2_hires_impervious_20231005.tif"
)

#load nj municipal boundaries shapefile data
nj_mun <- vect("/Users/ceciliacrowe/Documents/ICSA/Research Project/NJ_Municipal_Boundaries/NJ_Municipal_Boundaries_3424.shp")
#create bounding box variable that is a shapefile of the study area, "mendhams"
#(in same CRS as 2002-2012)
mendhams <- nj_mun[nj_mun$NAME %in% c("Mendham Township", "Mendham Borough"), ]

#reprojecting the 2020 raster was taking a very long time. instead, i am using the 2020 raster crs to crop it to the mendham boundary, 
#and then converting back into the crs of all the other files. 
# Project Mendham polygon to match 2020 raster CRS
mendhams_2020crs <- project(mendhams, crs(is_2020))

# Crop and mask older vectors (2002-2012) using original Mendham polygon
is_2002_m <- mask(crop(is_2002, mendhams), mendhams)
is_2007_m <- mask(crop(is_2007, mendhams), mendhams)
is_2012_m <- mask(crop(is_2012, mendhams), mendhams)

# Crop and mask 2020 raster using projected Mendham polygon
is_2020_crop <- mask(crop(is_2020, mendhams_2020crs), mendhams_2020crs)

# Reproject cropped 2020 raster to match CRS of older layers
is_2020_m <- project(is_2020_crop, crs(is_2002))

# Now, all datasets (2002-2020) are trimmed to Mendham and in the same CRS

#trim lulc data to municipality, remove data outside of study area
lulc_2002_m <- mask(crop(lulc_2002, mendhams), mendhams)  # trims to bounding box, removes everything outside the polygons
lulc_2007_m <- mask(crop(lulc_2007, mendhams), mendhams)
lulc_2012_m <- mask(crop(lulc_2012, mendhams), mendhams)
lulc_2020_m <- mask(crop(lulc_2020, mendhams), mendhams)

#going to rasterize all the layers that are currently vectors in order to use terra,
#landscapemetrics, and randomforest . using the one existing vector as a template, 
#which is ok because everything now has the same crs. 
template <- is_2020_m

lulc_2002_sf <- st_as_sf(lulc_2002_m)
lulc_2007_sf <- st_as_sf(lulc_2007_m)
lulc_2012_sf <- st_as_sf(lulc_2012_m)
lulc_2020_sf <- st_as_sf(lulc_2020_m)

is_2002_sf <- st_as_sf(is_2002_m)
is_2007_sf <- st_as_sf(is_2007_m)
is_2012_sf <- st_as_sf(is_2012_m)
#is_2020_m is already a vector

#rasterize lulc layers
lulc_2002_r <- rasterize(lulc_2002_sf, template, field = "LABEL02") #LABELXX is the field for Description of land use/land cover category for that year
lulc_2007_r <- rasterize(lulc_2007_sf, template, field = "LABEL07") 
lulc_2012_r <- rasterize(lulc_2012_sf, template, field = "LABEL12") 
lulc_2020_r <- rasterize(lulc_2020_sf, template, field = "LABEL20") 

#rasterize is layers
is_2002_r <- rasterize(is_2002_sf, template, field = "IS02") #ISXX is the field for Percent of each land use polygon covered by impervious surface for that year
is_2007_r <- rasterize(is_2007_sf, template, field = "IS07")
is_2012_r <- rasterize(is_2012_sf, template, field = "IS12")
#2020 variable name change for consistency
is_2020_r <- is_2020_m


#------------------------------
#PLOTTING FOREST LOSS
#--------------------------

#here is a list of 2002 lulc labels. I derived whether they should be classified as forest according to the following document
#A Land Use and Land Cover Classification System for Use with Remote Sensor Data, U. S. Geological Survey Professional Paper 964, 1976; edited by NJDEP, OIRM, BGIA, 1998, 2000, 2001, 2002, 2007, 2012
#i am reclassifying the raster into 1= forest, 0=non-forest. here are the labels and how i classify them
# [1] "MIXED WOODED WETLANDS (DECIDUOUS DOM.)"   1 Because they are wooded                  
#[2] "DECIDUOUS FOREST (10-50% CROWN CLOSURE)"   1 Because it is explicitly labeled forest                 
#[3] "DECIDUOUS WOODED WETLANDS"                 1 Because they are wooded               
#[4] "BRIDGE OVER WATER"                         0 Because it is not wooded             
#[5] "RESIDENTIAL, RURAL, SINGLE UNIT"           0 Because it is developed             
#[6] "CROPLAND AND PASTURELAND"                  0 Because it is not wooded                
#[7] "ARTIFICIAL LAKES"                          0 Because it is not wooded                
#[8] "DECIDUOUS FOREST (>50% CROWN CLOSURE)"     1 Because it is explicitly labeled forest                 
#[9] "DECIDUOUS BRUSH/SHRUBLAND"                 1 Because it is wooded              
#[10] "OTHER URBAN OR BUILT-UP LAND"             0  Because it is developed               
#[11] "RESIDENTIAL, SINGLE UNIT, LOW DENSITY"    0  Because it is developed                 
#[12] "MIXED FOREST (>50% DECIDUOUS WITH >50% CROWN CLOSURE)"     1  Because it is explicitly labeled forest
#[13] "TRANSITIONAL AREAS"                       0  Because it is not wooded                 
#[14] "RECREATIONAL LAND"                        0  Because it is not wooded                  
#[15] "MIXED SCRUB/SHRUB WETLANDS (DECIDUOUS DOM.)"                1  Because it is wooded (young trees)
#[16] "MIXED FOREST (>50% DECIDUOUS WITH 10-50% CROWN CLOSURE)"    1  Because it is explicitly labeled forest
#[17] "COMMERCIAL/SERVICES"                                        0  Because it is developed
#[18] "DECIDUOUS SCRUB/SHRUB WETLANDS"                             1  Because it is wooded (young trees)
#[19] "CEMETERY"                                                   0  Because it is developed
#[20] "MIXED DECIDUOUS/CONIFEROUS BRUSH/SHRUBLAND"                 1  Because it is wooded (young trees)
#[21] "CONIFEROUS BRUSH/SHRUBLAND"                                 1  Because it is wooded (young trees)
#[22] "MIXED FOREST (>50% CONIFEROUS WITH >50% CROWN CLOSURE)"     1  Because it is explicitly labeled forest
#[23] "RESIDENTIAL, SINGLE UNIT, MEDIUM DENSITY"                   0 Because it is developed
#[24] "RESIDENTIAL, HIGH DENSITY OR MULTIPLE DWELLING"             0 Because it is developed
#[25] "OTHER AGRICULTURE"                                          0 Because it is not wooded
#[26] "TRANSPORTATION/COMMUNICATION/UTILITIES"                     0 Because it is developed
#[27] "OLD FIELD (< 25% BRUSH COVERED)"                            0 Because it is not wooded
#[28] "STREAMS AND CANALS"                                         0 Because it is not wooded
#[29] "MANAGED WETLAND IN MAINTAINED LAWN GREENSPACE"              0 Because it is not wooded
#[30] "ORCHARDS/VINEYARDS/NURSERIES/HORTICULTURAL AREAS"           0  Because it is not wooded
#[31] "AGRICULTURAL WETLANDS (MODIFIED)"                           0 Because it is not wooded
#[32] "STORMWATER BASIN"                                           0  Because it is not wooded
#[33] "HERBACEOUS WETLANDS"                                        0 Because it is not wooded
#[34] "CONIFEROUS FOREST (>50% CROWN CLOSURE)"                     1  Because it is explicitly labeled forest
#[35] "FORMER AGRICULTURAL WETLAND (BECOMING SHRUBBY, NOT BUILT-UP)" 0 Because it is not wooded
#[36] "MANAGED WETLAND IN BUILT-UP MAINTAINED REC AREA"            0 Because it is not wooded
#[37] "NATURAL LAKES"                                              0 Because it is not wooded
#[38] "PLANTATION"                                                 0 Because it is managed, and not ecologically equivalent to natural forest
#[39] "CONIFEROUS FOREST (10-50% CROWN CLOSURE)"                   1 Because it is explicitly labeled forest
#[40] "MIXED FOREST (>50% CONIFEROUS WITH 10-50% CROWN CLOSURE)"   1 Because it is explicitly labeled forest
#[41] "WETLAND RIGHTS-OF-WAY"                                      0 Because it is not wooded
#[42] "UPLAND RIGHTS-OF-WAY UNDEVELOPED"                           0 Because it is not wooded
#[43] "ATHLETIC FIELDS (SCHOOLS)"                                  0 Because it is not wooded
#[44] "CONIFEROUS WOODED WETLANDS"                                 1 Because it is wooded
#[45] "CONIFEROUS SCRUB/SHRUB WETLANDS"                            1 Because it is wooded (young trees)
#[46] "MIXED SCRUB/SHRUB WETLANDS (CONIFEROUS DOM.)"               1 Because it is wooded (young trees)

#here is a list of 2007 lulc labels. I derived whether they should be classified as forest or not forest based on this document
#A Land Use and Land Cover Classification System for Use with Remote Sensor Data, U. S. Geological Survey Professional Paper 964, 1976; edited by NJDEP, OIRM, BGIA, 1998, 2000, 2001, 2002, 2007, 2012
#i am reclassifying the raster into 1= forest, 0=non-forest. here are the labels and how i classify them
# here is a list the one additional labels that do not appear in the 2002 label list that appears variably in 2007, 2012,and 2020 lists:

#[47] "DISTURBED WETLANDS (MODIFIED)"                           0 Because it is not wooded


forest_labels <- c(
"MIXED WOODED WETLANDS (DECIDUOUS DOM.)",                   
"DECIDUOUS FOREST (10-50% CROWN CLOSURE)",                
"DECIDUOUS WOODED WETLANDS", "DECIDUOUS FOREST (>50% CROWN CLOSURE)", "DECIDUOUS BRUSH/SHRUBLAND",
"MIXED FOREST (>50% DECIDUOUS WITH >50% CROWN CLOSURE)", "MIXED SCRUB/SHRUB WETLANDS (DECIDUOUS DOM.)", 
"MIXED FOREST (>50% DECIDUOUS WITH 10-50% CROWN CLOSURE)", "DECIDUOUS SCRUB/SHRUB WETLANDS", 
"MIXED DECIDUOUS/CONIFEROUS BRUSH/SHRUBLAND", "CONIFEROUS BRUSH/SHRUBLAND", 
"MIXED FOREST (>50% CONIFEROUS WITH >50% CROWN CLOSURE)","CONIFEROUS FOREST (>50% CROWN CLOSURE)",
"CONIFEROUS FOREST (10-50% CROWN CLOSURE)", "MIXED FOREST (>50% CONIFEROUS WITH 10-50% CROWN CLOSURE)",
"CONIFEROUS WOODED WETLANDS", "CONIFEROUS SCRUB/SHRUB WETLANDS", "MIXED SCRUB/SHRUB WETLANDS (CONIFEROUS DOM.)" 
)

#the following code takes an sf object + the LULC label field and creates a 
#forest_bin (binary) column, rasterizes the binary forest classification to my template, and returns a binary forest raster
  
  make_forest_raster <- function(lulc_sf, label_field, forest_labels, template) {
    
    lulc_sf <- lulc_sf %>%
      dplyr::mutate(forest_bin = ifelse(.data[[label_field]] %in% forest_labels, 1, 0))
    
    sv <- terra::vect(lulc_sf)
    
    r <- terra::rasterize(
      sv,
      template,
      field = "forest_bin",
      fun = "max",
      background = 0
    )
    return(r)
  }
  
  #i am running the function on all LULC years to create the forest binary column in all the years
  forest_2002_r <- make_forest_raster(lulc_2002_sf, "LABEL02", forest_labels, template)
  forest_2007_r <- make_forest_raster(lulc_2007_sf, "LABEL07", forest_labels, template)
  forest_2012_r <- make_forest_raster(lulc_2012_sf, "LABEL12", forest_labels, template)
  forest_2020_r <- make_forest_raster(lulc_2020_sf, "LABEL20", forest_labels, template)
  
  # plotting forest loss
  forest_loss_r <- forest_2002_r - forest_2020_r
  # Replace 0 values with NA so the figure only shows loss and gain
  forest_loss_plot <- forest_loss_r
  forest_loss_plot[forest_loss_plot == 0] <- NA
  # interpretation: 1 - 0 = 1 → forest lost 
  #1 - 1 = 0 → still forest 
  #0 - 0 = 0 → still non-forest 
  #0 - 1 = -1 → forest gained
col <- c("green", "purple")
  plot(forest_loss_plot, col=col, main = "Forest Loss and Gain 2002–2020")


  #--------------------------------
  
  # FOREST METRICS AND PLOTTING
  
  #------------------------------
  
  # List of forest rasters (binary: 1=forest, 0=non-forest)
  
  forest_rasters <- list(
    "2002" = forest_2002_r,
    "2007" = forest_2007_r,
    "2012" = forest_2012_r,
    "2020" = forest_2020_r
  )
  
  # Ensure no NaN/NA values in rasters
  
  forest_rasters <- lapply(forest_rasters, function(r) {
    r[is.na(r[])] <- 0
    return(r)
  })
  forest_2002_r[is.na(forest_2002_r[])] <- 0
  
  # Function to calculate forest area metrics
  
  calc_forest <- function(r) {
    a <- cellSize(r, unit = "ha")        # per-cell area in hectares
    valid <- !is.na(r[])
    
    total_area_ha   <- sum(a[valid], na.rm = TRUE)
    total_forest_ha <- sum(a[r[] == 1], na.rm = TRUE)
    percent_forest  <- 100 * total_forest_ha / total_area_ha
    
    return(c(total_area_ha = total_area_ha,
             total_forest_ha = total_forest_ha,
             percent_forest = percent_forest))
  }
  
  # Apply to all years
  
  forest_metrics <- sapply(forest_rasters, calc_forest)
  forest_df <- as.data.frame(t(forest_metrics))
  
  # Calculate changes
  
  forest_df$forest_change_ha <- c(NA, diff(forest_df$total_forest_ha))
  forest_df$overall_change_ha <- forest_df$total_forest_ha[nrow(forest_df)] - forest_df$total_forest_ha[1]
  forest_df$forest_lost_each_period <- c(NA, -forest_df$forest_change_ha[-1])
  forest_df$percent_forest_lost_each_period <- c(
    NA,
    (forest_df$forest_lost_each_period[-1] / forest_df$total_forest_ha[-nrow(forest_df)]) * 100
  )
  
  # Turn off scientific notation and round for display
  
  options(scipen = 999)
  forest_df_rounded <- round(forest_df, 2)
  
  # Plot forest area over time
  
  library(ggplot2)
  ggplot(forest_df, aes(x = year, y = total_forest_ha)) +
    geom_line(size = 1.2, color = "darkgreen") +
    geom_point(size = 3, color = "darkgreen") +
    labs(title = "Forest Area in the Mendhams (2002–2020)",
         x = "Year",
         y = "Forest Area (ha)") +
    theme_minimal()
  
  #----------------------------
  
  #IMPERVIOUS SURFACE CHANGE
  
  #-------------------------------
  
  
  #plot impervious surface change (i dont have to do all that binary stuff because impervious surface is presented as a percentage already in the data)
  #Each cell represents the percentage point change between 2002 and 2020. 
  #Ex: If a cell was 10% impervious in 2002 and 30% in 2020 → 20% increase.
  #If no change → 0.
  is_2020_r <- is_2020_r * 100
  #change year by year
  change_2002_2007 <- is_2007_r - is_2002_r
  change_2007_2012 <- is_2012_r - is_2007_r
  change_2012_2020 <- is_2020_r - is_2012_r
  change_2002_2020 <- is_2020_r - is_2002_r
  
  # Put all rasters in a list
  change_list <- list(
    "2002–2007" = change_2002_2007,
    "2007–2012" = change_2007_2012,
    "2012–2020" = change_2012_2020,
    "2002–2020" = change_2002_2020
  )
  # Set 0 (no change) to NA
  change_list <- lapply(change_list, function(r) { r[r == 0] <- NA; r })
 
  #plotting the data
  
  # Color palette
  col <- hcl.colors(100, "RdBu", rev=TRUE)
  
  par(mfrow=c(2,2))
  
  for(i in 1:length(change_list)){
    plot(change_list[[i]],
         col=col) }

  #finding % impervious surface (metrics)
 
  # List of the original rasters
  rasters <- list(is_2002_r, is_2007_r, is_2012_r, is_2020_r)
  names(rasters) <- c("2002","2007","2012","2020")
  
  # Getting cell area in square meters and hectares
  cell_res <- res(is_2020_r)   # same for all rasters
  cell_area <- cell_res[1] * cell_res[2]         # m squared
  cell_area_ha <- cell_area / 10000              # hectares
  
  calc_impervious <- function(r) {
    # total raster area = all cells, regardless of NA values
    total_area <- ncell(r) * cell_area      # m²
    total_area_ha <- total_area / 10000
    
    # total impervious area (only sum non-NA values)
    total_imperv <- sum(values(r), na.rm=TRUE) * cell_area / 100
    total_imperv_ha <- total_imperv / 10000

    percent_imperv <- (total_imperv / total_area) * 100
    # Turn off scientific notation 
    options(scipen=999)
    round(percent_imperv, 2)
    round(total_imperv_ha, 2)
    return(c(total_area_ha = total_area_ha,
             total_imperv_ha = total_imperv_ha,
             percent_imperv = percent_imperv))
  }
  
  imperv_metrics <- sapply(rasters, calc_impervious)
  
  imperv_metrics
  

  
  