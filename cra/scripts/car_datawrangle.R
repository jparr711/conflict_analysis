
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
cra_adm2 <- sf::read_sf("C:/Users/jparr/OneDrive - DAI/ACLED/Conflict/CRA/caf_admbnda_adm2_200k_sigcaf_reach_itos_v2/caf_admbnda_adm2_200k_sigcaf_reach_itos_v2.shp")
# cra_adm2 <- gb_adm2(c("colombia"), type = "sscgs")
cra_adm0 <- gb_adm0(c("central african republic"), type = "sscgs")
cra_terrain_raster <- get_elev_raster(locations = cra_adm2, z = 5)
elev_crop = crop(cra_terrain_raster, cra_adm0)

r.tri <- spatialEco::tri(elev_crop) 

cra_tri <- data.frame(raster::extract(r.tri, cra_adm2, fun=mean, na.rm =TRUE, sp = TRUE))

cra_tri <- cra_tri %>% dplyr::rename(r_tri = "layer")

cra_tri <- cra_tri %>% dplyr::select(admin2Name, admin1Name, r_tri)


# City distance
# https://forobs.jrc.ec.europa.eu/products/gam/
# https://forobs.jrc.ec.europa.eu/products/gam/images/large/access_50k.png
dist_raster <- raster("C:/Users/jparr/OneDrive - DAI/ACLED/Colombia/access_50k/acc_50k.tif")
elev_crop = crop(dist_raster, cra_adm0)

acc_50k <- data.frame(raster::extract(elev_crop, cra_adm2, fun=mean, na.rm =TRUE, sp = TRUE))

acc_50k <- acc_50k %>% dplyr::rename(dist = "acc_50k")

acc_50k <- acc_50k %>% dplyr::select(admin2Name, admin1Name, dist)


rm(elev_crop, dist_raster, cra_terrain_raster)

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

elev_crop = crop(nl, cra_adm0)

elev_crop_agg <- aggregate(elev_crop, fact = 2)

nl_adm2 <- data.frame(raster::extract(elev_crop_agg, cra_adm2, fun=mean, na.rm = T, sp = T))

nl_adm2 <- nl_adm2 %>% dplyr::rename(nl = "VNL_v2_npp_2021_global_vcmslcfg_c202203152300.average_masked")

nl_adm2 <- nl_adm2 %>% dplyr::select(admin2Name, admin1Name, nl)


# vegetation

library(MODIStsp)

map_boundary <- geoboundaries("Central African Republic")

# Defining filepath to save downloaded spatial file
spatial_filepath <- "CRA/vegetation_data/CRA.shp"

# Saving downloaded spatial file on to our computer
st_write(map_boundary, paste0(getwd(), "/", spatial_filepath), append = F)
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
# C:\Users\jparr\OneDrive - DAI\ACLED\Conflict\VegetationData\CRA\VI_16Days_1Km_v6\NDVI
vegetation <- raster("C:/Users/jparr/OneDrive - DAI/ACLED/Conflict/VegetationData/CRA/VI_16Days_1Km_v6/NDVI/MOD13A2_NDVI_2020_001.tif")

vegetation <- projectRaster(vegetation, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

elev_crop = crop(vegetation, cra_adm0)

elev_crop_agg <- aggregate(elev_crop, fact = 2)

ndvi <- data.frame(raster::extract(elev_crop, cra_adm2, fun=mean, na.rm = T, sp = T))

ndvi <- ndvi %>% dplyr::rename(ndvi = "MOD13A2_NDVI_2020_001")

ndvi_adm2 <- ndvi %>% dplyr::select(admin2Name, admin1Name, ndvi)


rm(vegetation, r.tri, nl, ndvi, elev_crop, elev_crop_agg)


cra_spatial <-
  cra_adm2 %>%
  left_join(acc_50k) %>%
  left_join(cra_tri) %>%
  left_join(nl_adm2) %>%
  left_join(ndvi_adm2)

plot(cra_spatial[, c(16:20)])

write_sf(cra_spatial, "CRA/cra_spatial.shp")




