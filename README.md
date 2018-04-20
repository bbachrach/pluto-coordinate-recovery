Pluto Coordinate Recovery
================

In DOF's PLUTO database, in all versions from 2003 to current approximately 3.6% are missing geographical coordinates. By feeding BBLs into the NYC Geoclient API latitude and longitude can be recovered for a large chunk of these. After recovering coordinates, missingness in 2003-2017 drops down to 2.2% of observations. For the pluto 2017 v2 it drops down to 0.2% of observations and 0.3% of residential units.

Initial mutate
==============

After reading in our time series pluto dataframe a separate dataframe to be fed into the API is created. It only includes BBLs with missing coordinates, does not include duplicated BBLs and puts Borough into a format accepted by the API.

``` r
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
```

URLs
====

Function
--------

This function turns a Borough Block and Lot into a url that when passed to the GEO Client API will return a json object containing all pertinent geographic and political information regarding the bbl.

``` r
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
```

The function is then used to create a vector of urls. Though not strictly necessary, the vector of URLs is put into the restricted pluto dataframe. The URL can be used as a primary key during troubleshooting.

``` r
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
```

API call
========

Define parameters to be used in foreach loop. Rather than using all of the machine's resources I leave some cores free to be able to multitask while the loop is running.

``` r
start <- 1
end <- length(tmp.vec)

cl <- makeCluster(round(detectCores()*.85))
registerDoParallel(cl)
```

Call API for each individual url within a foreach loop

``` r
ptm <- proc.time()
initial_output.list <- foreach(i = start:end
                               , .packages = c("httr")) %dopar% {
                                 out <- try(GET(url = tmp.vec[i]))
                                 return(out)
                               }
stopCluster(cl)
api_call.time <- proc.time() - ptm
```

API Contents
============

The API and content extraction are put in separate loops so as to be able to create a saved workspace or rds object in between. While extracting the content a decent amount of error handling is included. Note that the output here also includes BIN.

``` r
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
```

Remove non-dataframe objects and bind into a list with lat/lon as numeric variables. NA BBLs and 0 BBLs are removed.

``` r
## remove list items which did not return results 
tmp_out.list[which(unlist(
  lapply(tmp_out.list, function(x) class(x))
) != "data.frame")] <- NULL


## bind and put lat/lon into numeric format
tmp.out <- bind_rows(tmp_out.list) %>%
  mutate(latitude = as.numeric(latitude)
         ,longitude = as.numeric(longitude)
         )
```

Somehow the output includes a small number of duplicated BBLs. Upon inspection the lat/lon output for these duplicates are in some circumstances slightly different. The variation is small enough that it likely does not matter and it didn't seem worth the time to investigate further for 60 some-odd observations. This portion goes through each duplicated BBL and if there are lat/lon combinations that appear multiple times selects those coordinates. For BBLs where there is not a dominant lat/lon combination duplicates are simply removed without specification as to which.

``` r
## remove rows without results and bbls of 0
tmp.out <- tmp.out %>%
  filter(!is.na(bbl) & bbl != "0") %>%
  ## create variable for bbl/lat/lon combination
  mutate(bbl_lat_lon = paste(bbl,latitude,longitude))

tmp_dupefix.out <- tmp.out %>%
  semi_join(
    tmp.out %>%
      filter(duplicated(bbl))
    ,by="bbl"
  )  %>%
  ## group by bbl and then see how many times within those groups a lat/lon combination pops up
  group_by(bbl) %>%
  group_by(bbl_lat_lon) %>%
  mutate(appearances = n()) %>% 
  ungroup() %>%
  ## within each bbl, keep observations with the dominant lat/lon combination
  group_by(bbl) %>%
  filter(appearances == max(appearances)) %>%
  ungroup() %>%
  ## remove duplicates 
  filter(!duplicated(bbl))


tmp.out <- bind_rows(
  tmp.out %>%
    anti_join(
      tmp_dupefix.out
      ,by="bbl"
    )
  ,tmp_dupefix.out
) %>%
  select(-bbl_lat_lon,-appearances)
```

Re-join with full pluto dataset
===============================

Just for posterity, check to see how many observations are missing coordinates prior to recovery

``` r
## check to see how many bbls were initially missing lat/lon
init.summary <- pluto.all %>%
  summarize(
    count = n()
    ,na_lat.count = sum(is.na(lat))
  )
```

Left join with tmp.out. Where lat and lon are NA specify to take the new values, where they are not keep the old values. Then drop the columns taken from tmp.out. Save to disk.

``` r
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
```

Add to our other rds files
==========================

2017
----

First order the new pluto.all dataset by year and place the na latitudes in lower order (for de-duping).

``` r
pluto.all <- pluto.all %>%
  arrange(desc(Year),is.na(lat))

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
```

2003-2016 all variables
-----------------------

``` r
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
```
