
## Script to recover some missing lat/lon coordinates from pluto using NYC Geoclient 

library(dplyr)
library(stringr)
library(httr)
library(parallel)
library(doParallel)


# Read in primary pluto dataframe -----------------------------------------

pluto.all <- readRDS("/Users/billbachrach/Dropbox (hodgeswardelliott)/Data Science/Project Data/PLUTO/pluto_lean_compressed_2003_2017.rds") %>%
  mutate(bbl = as.character(bbl))

## only observations missing lat/lon and no duplicates, change borough to format API requires
pluto.restr <- pluto.all %>%
  filter(is.na(lat)) %>%
  select(BBL,Borough,Block,Lot) %>% 
  mutate(Borough = ifelse(Borough=="MN","MANHATTAN",Borough)
                                          ,Borough = ifelse(Borough=="BX","BRONX",Borough)
                                          ,Borough = ifelse(Borough=="BK","BROOKLYN",Borough)
                                          ,Borough = ifelse(Borough=="QN","QUEENS",Borough)
                                          ,Borough = ifelse(Borough=="SI","STATEN ISLAND",Borough)
  ) %>%
  filter(!duplicated(BBL))



# URLS --------------------------------------------------------------------

## Function for creating the urls to call API with 
url_create.fun <- function(baseurl = "https://api.cityofnewyork.us/geoclient/v1/bbl.json?"
                           ,keyid = "&app_id=43dc14b8&app_key=2df323d9189dfd5f1d2c8bf569843588"
                           ,borough
                           ,block
                           ,lot
                           ,zip=zip
){
  url <- gsub(" ","+"
              ,paste(
                baseurl
                ,"borough="
                ,borough
                ,"&block="
                ,block
                ,"&lot="
                ,lot
                ,sep=""
              )
  )
  
  url <- gsub(" ","+"
              ,paste(
                url
                ,keyid
                ,sep=""
              )
  )
  
  return(url)
}
colnames(pluto.restr)

## create vector of urls and put into the restricted pluto dataframe
tmp.vec <- unlist(
  lapply(1:nrow(pluto.restr), function(x){
    url <- url_create.fun(
      borough = pluto.restr[x,"Borough"]
      ,block =  pluto.restr[x,"Block"]
      ,lot =  pluto.restr[x,"Lot"]
    )
    return(url)}
  )
)

pluto.restr[,"url"] <- tmp.vec



# API call ----------------------------------------------------------------

start <- 1
end <- length(tmp.vec)

## call API 
cl <- makeCluster(round(detectCores()*.85))
registerDoParallel(cl)

ptm <- proc.time()
initial_output.list <- foreach(i = start:end
                               , .packages = c("httr")) %dopar% {
                                 out <- try(GET(url = tmp.vec[i]))
                                 return(out)
                               }
stopCluster(cl)
api_call.time <- proc.time() - ptm

## get contents of results
cl <- makeCluster(8)
registerDoParallel(cl)

ptm <- proc.time()
tmp_out.list <- foreach(i = start:end
                        , .packages = c("httr","dplyr")) %dopar% {
                          output <- initial_output.list[[i]]
                          
                          if(class(output)=="response"){
                            output <- content(output)
                            if(class(output)=="list"){
                              output <- output$bbl
                              content.names <- names(output)
                            }
                          }
                          
                          if(class(output)=="list"){
                            
                            out <- list(
                              bbl = output$bbl
                              ,buildingIdentificationNumber = output$buildingIdentificationNumber
                              ,latitude = output$latitude
                              ,longitude = output$longitude
                              ,latitude_internal = output$latitudeInternalLabel
                              ,longitude_internal = output$longitudeInternalLabel
                              ,url = tmp.vec[i]
                            )
                            
                          } else {
                            out <- list(url = tmp.vec[i])
                          }
                          out <- t(unlist(out)) %>% as.data.frame()
                          return(out)
                          cat(i,"\n")
                        }

stopCluster(cl)
content_time.all <- proc.time() - ptm

## remove list items which did not return results 
tmp_out.list[which(unlist(
  lapply(tmp_out.list, function(x) class(x))
) != "data.frame")] <- NULL


## bind and put lat/lon into numeric format
tmp.out <- bind_rows(tmp_out.list) %>%
  mutate(latitude = as.numeric(latitude)
         ,longitude = as.numeric(longitude)
         )


# save.image("/Users/billbachrach/Dropbox (hodgeswardelliott)/Data Science/Bill Bachrach/ad_hoc/misc/pluto_geoclient_latlon_workspace 20180420_1532.RData")

## remove rows without results and bbls of 0
tmp.out <- tmp.out %>%
  filter(!is.na(bbl) & bbl != "0") %>%
  mutate(bbl_lat_lon = paste(bbl,latitude,longitude))

## for bbls with more than one output, see if there are lat/lon combinations which appear more frequently
## pick the lat/lon that appears most frequently
## remove remaining duplicates
tmp_dupefix.out <- tmp.out %>%
  semi_join(
    tmp.out %>%
      filter(duplicated(bbl))
    ,by="bbl"
  )  %>%
  group_by(bbl) %>%
  group_by(bbl_lat_lon) %>%
  mutate(appearances = n()) %>% 
  ungroup() %>%
  group_by(bbl) %>%
  filter(appearances == max(appearances)) %>%
  ungroup() %>%
  filter(!duplicated(bbl))
## NOTE: duplicate bbls appear to have coordinates that are very close to one another

tmp.out <- bind_rows(
  tmp.out %>%
    anti_join(
      tmp_dupefix.out
      ,by="bbl"
    )
  ,tmp_dupefix.out
) %>%
  select(-bbl_lat_lon,-appearances)
  
## check to see how many bbls were initially missing lat/lon
init.summary <- pluto.all %>%
  summarize(
    count = n()
    ,na_lat.count = sum(is.na(lat))
  )

## replace NA values with those just found
# pluto.all.hold <- pluto.all
pluto.all <- pluto.all %>%
  left_join(tmp.out %>%
              mutate(BBL = bbl) %>%
              select(BBL,latitude,longitude)) %>%
  mutate(lat = ifelse(is.na(lat)
                      ,latitude
                      ,lat
  )
  ,lon = ifelse(is.na(lon)
                ,longitude
                ,lon
  )
  ) %>%
  select(-latitude,-longitude)


saveRDS(pluto.all,"/Users/billbachrach/Dropbox (hodgeswardelliott)/Data Science/Project Data/PLUTO/pluto_lean_compressed_2003_2017.rds")

pluto.all <- pluto.all %>%
  arrange(desc(Year),is.na(lat))



# Read in 2017 pluto rds --------------------------------------------------


pluto_2017 <- readRDS("/Users/billbachrach/Dropbox (hodgeswardelliott)/Data Science/Project Data/PLUTO/pluto_lean_compressed_2017.rds")

## replace NA values with those just found
pluto_2017 <- pluto_2017 %>%
  left_join(
    pluto.all %>%
      select(BBL,lat,lon) %>%
      filter(!is.na(lat)) %>%
      filter(!duplicated(BBL)) %>%
      rename(lat.tmp = lat
             ,lon.tmp = lon)
    ,by="BBL"
  ) %>%
  mutate(lat = as.numeric(ifelse(is.na(lat)
                      ,lat.tmp
                      ,lat
                      ))
         ,lon = as.numeric(ifelse(is.na(lon)
                                  ,lon.tmp
                                  ,lon
         ))
         ) %>%
  select(-lat.tmp,-lon.tmp)

## save to disk 
saveRDS(pluto_2017,"/Users/billbachrach/Dropbox (hodgeswardelliott)/Data Science/Project Data/PLUTO/pluto_lean_compressed_2017.rds")


# Pluto 2003-2016 all -----------------------------------------------------

pluto.wide <- readRDS('/Users/billbachrach/Dropbox (hodgeswardelliott)/Data Science/Project Data/PLUTO/pluto_all_compressed_2003_2016.rds')

pluto.wide <- pluto.wide %>%
  left_join(
    pluto.all %>%
      select(BBL,lat,lon) %>%
      filter(!is.na(lat)) %>%
      filter(!duplicated(BBL)) %>%
      rename(lat.tmp = lat
             ,lon.tmp = lon)
    ,by="BBL"
  ) %>%
  mutate(lat = as.numeric(ifelse(is.na(lat)
                                 ,lat.tmp
                                 ,lat
  ))
  ,lon = as.numeric(ifelse(is.na(lon)
                           ,lon.tmp
                           ,lon
  ))
  ) %>%
  select(-lat.tmp,-lon.tmp)

saveRDS(pluto.wide,'/Users/billbachrach/Dropbox (hodgeswardelliott)/Data Science/Project Data/PLUTO/pluto_all_compressed_2003_2016.rds')