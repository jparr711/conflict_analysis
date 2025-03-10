
#### geospatial vars #### 
# terrain ruggedness
# nightlights? infra?
# forest cover
# distance to city
library(tidyverse)
library(rgeoboundaries)
library(elevatr)
library(raster)
library(spatialEco)
library(sf)
# Terrain Ruggedness
lib_adm2 <- sf::read_sf("C:/Users/jparr/OneDrive - DAI/ACLED/Conflict/Libya/lby_adm_unosat_lbsc_20180507_SHP/lby_admbnda_adm2_unosat_lbsc_20180507.shp")
# lib_adm2 <- gb_adm2(c("colombia"), type = "sscgs")
lib_adm0 <- gb_adm0(c("libya"), type = "sscgs")
lib_terrain_raster <- get_elev_raster(locations = lib_adm2, z = 5)
elev_crop = crop(lib_terrain_raster, lib_adm0)

r.tri <- spatialEco::tri(elev_crop) 

lib_tri <- data.frame(raster::extract(r.tri, lib_adm2, fun=mean, na.rm =TRUE, sp = TRUE))

lib_tri <- lib_tri %>% dplyr::rename(r_tri = "layer")

lib_tri <- lib_tri %>% dplyr::select(ADM2_EN, ADM1_EN, r_tri)


# City distance
# https://forobs.jrc.ec.europa.eu/products/gam/
# https://forobs.jrc.ec.europa.eu/products/gam/images/large/access_50k.png
dist_raster <- raster("C:/Users/jparr/OneDrive - DAI/ACLED/Colombia/access_50k/acc_50k.tif")
elev_crop = crop(dist_raster, lib_adm0)

acc_50k <- data.frame(raster::extract(elev_crop, lib_adm2, fun=mean, na.rm =TRUE, sp = TRUE))

acc_50k <- acc_50k %>% dplyr::rename(dist = "acc_50k")

acc_50k <- acc_50k %>% dplyr::select(ADM2_EN, ADM1_EN,  dist)


rm(elev_crop, dist_raster, lib_terrain_raster)

# nightlights
# devtools::install_github("walshc/nightlights")
library(nightlights)

library(httr)
library(jsonlite)
library(utils)
library(R.utils)
# download
# Retrieve access token
params <- list(
  client_id = 'eogdata_oidc',
  client_secret = '2677ad81-521b-4869-8480-6d05b9e57d48',
  username = 'jamie_parr@dai.com',
  password = 'Cleokim1!',
  grant_type = 'password'
)
token_url <- 'https://eogauth.mines.edu/auth/realms/master/protocol/openid-connect/token'
response <- POST(token_url, body = params, encode = "form")
access_token_list <- fromJSON(content(response,as="text",encoding="UTF-8"))
access_token <- access_token_list$access_token
# Submit request with token bearer and write to output file
## Change data_url variable to the file you want to download
data_url <- 'https://eogdata.mines.edu/nighttime_light/monthly_notile/v10/2021/202101/vcmslcfg/SVDNB_npp_20210101-20210131_global_vcmslcfg_v10_c202102062300.cf_cvg.tif'
auth <- paste('Bearer', access_token)
## You can either define the output file name directly
# output_file <- 'EOG_sensitive_contents.txt'
## Or get the filename from the data_url variable
output_file <- basename(data_url)
download.file(data_url,output_file,mode = "wb", headers = list(Authorization = auth))
gunzip(paste0(getwd(), "/", output_file))
output_file <- "VNL_v2_npp_2021_global_vcmslcfg_c202203152300.average_masked.tif"

nl <- raster(paste0("C:/Users/jparr/OneDrive - DAI/ACLED/Colombia", "/", output_file))

elev_crop = crop(nl, lib_adm0)

elev_crop_agg <- aggregate(elev_crop, fact = 2)

nl_adm2 <- data.frame(raster::extract(elev_crop_agg, lib_adm2, fun=mean, na.rm = T, sp = T))

nl_adm2 <- nl_adm2 %>% dplyr::rename(nl = "VNL_v2_npp_2021_global_vcmslcfg_c202203152300.average_masked")

nl_adm2 <- nl_adm2 %>% dplyr::select(ADM2_EN, ADM1_EN, nl)


# vegetation

library(MODIStsp)

map_boundary <- geoboundaries("Libya")

# Defining filepath to save downloaded spatial file
spatial_filepath <- "Libya/vegetation_data/lib.shp"

# Saving downloaded spatial file on to our computer
st_write(map_boundary, paste0(spatial_filepath), append = F)
# C:\Users\jparr\OneDrive - DAI\ACLED\Conflict\Libya
MODIStsp(
  gui = FALSE,
  out_folder = "VegetationData",
  out_folder_mod = "VegetationData",
  selprod = "Vegetation_Indexes_16Days_1Km (M*D13A2)",
  bandsel = "NDVI",
  user = "mstp_test",
  password = "MSTP_test_01",
  start_date = "2020.01.01",
  end_date = "2020.01.01",
  verbose = FALSE,
  spatmeth = "file",
  spafile = paste0(getwd(), "/", spatial_filepath),
  out_format = "GTiff"
)

vegetation <- raster("C:/Users/jparr/OneDrive - DAI/ACLED/Conflict/VegetationData/lib/VI_16Days_1Km_v6/NDVI/MOD13A2_NDVI_2020_001.tif")

vegetation <- projectRaster(vegetation, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

elev_crop = crop(vegetation, lib_adm0)

elev_crop_agg <- aggregate(elev_crop, fact = 2)

ndvi <- data.frame(raster::extract(elev_crop, lib_adm2, fun=mean, na.rm = T, sp = T))

ndvi <- ndvi %>% dplyr::rename(ndvi = "MOD13A2_NDVI_2020_001")

ndvi_adm2 <- ndvi %>% dplyr::select(ADM2_EN, ADM1_EN, ndvi)


rm(vegetation, r.tri, nl, ndvi, elev_crop, elev_crop_agg)


lib_spatial <-
  lib_adm2 %>%
  left_join(acc_50k) %>%
  left_join(lib_tri) %>%
  left_join(nl_adm2) %>%
  left_join(ndvi_adm2)

plot(lib_spatial[, c(20:24)])


### health socioeconomic data
library(expss)
libya_papfam <- read_labelled_xlsx(paste0(getwd(), "/Libya/lby-region-results-mpi-2021.xlsx"), data_sheet = 1)

names(libya_papfam) <-libya_papfam[c(4), ]

libya_papfam <- libya_papfam[c(9:29),]

libya_papfam <- libya_papfam[, c(1:20)]

libya_papfam <- as.data.frame(libya_papfam)

names(libya_papfam) <- c("ISO_Num", "ISO_CNTY", "Country", "Region", "Survey", "Year", "ADM2", "Cnty_MPI",
"MPI_Index", "MPI_Pct", "Deprivation_Pct", "Vulnerable_Pct", "Severe_Pov_Pct",
"Population_Yr_Survey", "Pop_2018", "Pop_2019", "Pop_Share_Reg_2019", "Pop_Size_Reg_2019",
"Tot_MPI_Poor", "Nr_Indicators_in_MPI")

libya_papfam <-
libya_papfam %>%
  dplyr::mutate_at(dplyr::vars(Cnty_MPI:Nr_Indicators_in_MPI), 
                   dplyr::funs(as.numeric))


libya_papfam %>% anti_join(lib_spatial, by = c("ADM2" = "ADM2_EN")) %>%
  dplyr::select(ADM2)

lib_spatial %>% anti_join(libya_papfam, by = c("ADM2_EN" = "ADM2")) %>%
  dplyr::select(ADM2_EN)

libya_papfam <- libya_papfam %>%
  dplyr::mutate(ADM2_EN = case_when(ADM2 == "Ajdabya" ~ "Ejdabia",
                                    ADM2 == "Ben-Ghazi" ~ "Benghazi",
                                    ADM2 == "El-Jabal El-Akhdar" ~ "Al Jabal Al Akhdar",
                                    ADM2 == "El-Jabal El-Gharbi" ~ "Al Jabal Al Gharbi",
                                    ADM2 == "El-Marj" ~ "Almarj",
                                    ADM2 == "El-Merqab" ~ "Almargeb",
                                    
                                    ADM2 == "El-Wahat/El-Kufra" ~ "Alkufra",
                                    ADM2 == "El-Zawya" ~ "Azzawya",
                                    ADM2 == "Jfara" ~ "Aljfara",
                                    ADM2 == "Murzuk" ~ "Murzuq",
                                    ADM2 == "Musrata" ~ "Misrata",
                                    
                                    ADM2 == "Qasr Ben-Ghesheer" ~ ,
                                    ADM2 == "Sebha/El-Shate" ~ "Sebha",
                                    ADM2 == "Sert/Jafra" ~ "Sirt",
                                    ADM2 == "Tarhuna" ~ ,
                                    ADM2 == "Tubruk" ~ "Tobruk",
                                    
                                    ADM2 == "Wadi El-Hayat" ~ "Wadi Ashshati",
                                    
                                    TRUE ~ as.character(ADM2)))
                   

write_sf(lib_spatial, "Libya/lib_spatial.shp")




