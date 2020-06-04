library(stringr)
library(data.table)
library(dplyr)

setwd("D:/Dropbox/Censustract_demo/NY")
demo<-fread('D:/Dropbox/Censustract_demo/acs_2012_tract_new.csv')
demo$statea<-str_pad(demo$statea, 2, pad = "0")
demo$countya<-str_pad(demo$countya, 3, pad = "0")
demo$tracta<-str_pad(demo$tracta, 6, pad = "0")

demo$censustract<-paste0(demo$statea, demo$countya, demo$tracta)
nytract<-demo%>%select(statea, countya, tracta, censustract)%>%filter(statea==36) #36 is the new yorkr
nytract<-unique(nytract$censustract)
nytract<-as.numeric(nytract)

lists<-list.files('D:/safegraph/social-dist/')
nysd<-data.frame()
for (i in 1:116){
  d<- read.table(paste0('D:/safegraph/social-dist/', lists[i]),header = TRUE, sep = "," )
  d$date<-as.Date(date_range_end)

  d$censustract<-stringr::str_sub(d$origin_census_block_group, 1, 10)
  d1<-d[d$censustract %in% nytract, ]
  nysd<-rbind(nysd, d1)
  print(i)
}

nysd$date1<-as.Date(nysd$date_range_start)
summary(nysd$date1)
#--------------------------------------------------------
# function to relax the nested row 
library(rjson)
library(plyr)

relaxcol<-function(x){
  if(nchar(x)>10){
  x<-fromJSON(x)
  x<-data.frame(t(data.frame(unlist(x))))}else{x<-data.frame(matrix())}
  return(x)
}
nysd$bucketed_distance_traveled<-as.character(nysd$bucketed_distance_traveled)
nysd$bucketed_home_dwell_time<-as.character(nysd$bucketed_home_dwell_time)
nysd$median_dwell_at_bucketed_distance_traveled<-as.character(nysd$median_dwell_at_bucketed_distance_traveled)
nysd$at_home_by_each_hour<-as.character(nysd$at_home_by_each_hour)

# ------------------aplly function to each column now -----------------
# distance travelled 
bucketed_distance_traveled_dt <- do.call("rbind.fill", lapply(nysd$bucketed_distance_traveled, relaxcol))
bucketed_distance_traveled_dt<- bucketed_distance_traveled_dt[, 1:7]
c<- colnames(bucketed_distance_traveled_dt)
c<- gsub('\\.','_',c )
c<- gsub('\\X','distance_travel_',c )
colnames(bucketed_distance_traveled_dt)<-c
nysd<-cbind(nysd, bucketed_distance_traveled_dt)

# home dwell time
bucketed_home_dwell_time_dt <- do.call("rbind.fill", lapply(nysd$bucketed_home_dwell_time, relaxcol))
bucketed_home_dwell_time_dt<- bucketed_home_dwell_time_dt[, 1:5]
c<- colnames(bucketed_home_dwell_time_dt)
c<- gsub('\\.','_',c )
c<- gsub('\\X','home_dwell_',c )
colnames(bucketed_home_dwell_time_dt)<-c
nysd<-cbind(nysd, bucketed_home_dwell_time_dt)

# travel dwell time
median_dwell_at_bucketed_distance_traveled_dt <- do.call("rbind.fill", lapply(nysd$median_dwell_at_bucketed_distance_traveled, relaxcol))
median_dwell_at_bucketed_distance_traveled_dt<- median_dwell_at_bucketed_distance_traveled_dt[, 1:6]
c<- colnames(median_dwell_at_bucketed_distance_traveled_dt)
c<- gsub('\\.','_',c )
c<- gsub('\\X','medium_dwell_travel_',c )
colnames(median_dwell_at_bucketed_distance_traveled_dt)<-c
nysd<-cbind(nysd, median_dwell_at_bucketed_distance_traveled_dt)

# athomehours
at_home_by_each_hour_dt <- do.call("rbind.fill", lapply(nysd$at_home_by_each_hour, relaxcol))
at_home_by_each_hour_dt<- at_home_by_each_hour_dt[, 1:24]
c<- colnames(at_home_by_each_hour_dt)
c<- gsub('\\.','_',c )
c<- gsub('\\X','athome_hour_',c )
colnames(at_home_by_each_hour_dt)<-c
nysd<-cbind(nysd, at_home_by_each_hour_dt)

colnames(nysd)
summary(nysd$origin_census_block_group)

nysd_temp<-nysd# just a temp file in case something happens 

# updating the file 
nysd_old<-fread('ny_socialdisst.csv')
nysd<-rbind(nysd, nysd_old, fill=TRUE)

summary(nysd$origin_census_block_group)
summary(nysd$date1)


fwrite(nysd, 'ny_socialdisst.csv')


#------------------------------------------
# create tract level file
#---------------------------------------------


nysd<-fread('ny_socialdisst.csv')
nysd$censustract<-stringr::str_sub(nysd$origin_census_block_group, 1, 11)

nysd$date1<-as.Date(nysd$date_range_start)
summary(nysd$date1)
summary(as.numeric(nysd$censustract))

detach("package:plyr", unload = TRUE)

nysd_tract<- nysd%>% 
  group_by(censustract , date1)%>%
  summarise( device_count= sum(device_count), 
             distance_traveled_from_home= median(distance_traveled_from_home), 
             completely_home_device_count= sum(completely_home_device_count), 
             median_home_dwell_time= median(median_home_dwell_time), 
             part_time_work_behavior_devices= sum(part_time_work_behavior_devices), 
             full_time_work_behavior_devices= sum(full_time_work_behavior_devices),
             nblock= length(unique(origin_census_block_group)) )

#read in other tract level files
contri<-fread('D:/Dropbox/Censustract_demo/score_demo.csv')
summary(contri$censustract)
summary(as.numeric(nysd_tract$censustract))
contri$censustract<-as.character(contri$censustract)

# subset to only new york city 
us1<-get_acs(geography = 'tract', variables = "B01003_001", state = "NY", 
             county = c("New York County",'Kings County', 
                        'Queens County','Bronx', 'Richmond County'), 
             geometry=TRUE) 


us1$censustract<-us1$GEOID
us1<-us1%>%select(censustract, NAME)

nysd1<-merge(nysd, us1, by= 'censustract', all.y=TRUE)
nysd1$censustract<-as.character(nysd1$censustract)
contri$censustract<-as.character(contri$censustract)

nysd1<-merge(nysd1, contri, by= 'censustract', all.x=TRUE)
ntract$censustract<-as.character(ntract$censustract)
nysd1<-merge(nysd1, ntract, by= 'censustract', all.x=TRUE)

nysd1$geometry<-NULL

fwrite(nysd1, 'nyc_sd_time.csv')

#fwrite(nysd_tract1, 'ny_sd_tractlevel_merged.csv')

nysd1<-fread('nyc_sd_demo.csv')


#----------------------------------------------------------------
# PLOT :  download the new york blocks data
#----------------------------------------------------------------

library(tidycensus)
library(sf)
library(leaflet)

#census_api_key("2899a53bed46dbfe55377c6113ab1a931e9aaa85", install = TRUE)
us1<-get_acs(geography = 'tract', variables = "B01003_001", state = "NY", 
             #county = c("New York County",'Kings County', 'Queens County','Bronx'), 
             geometry=TRUE, keep_geo_vars=TRUE) 


# merge the data together
nysd_temp<-nysd_tract%>%mutate(meandevice = device_count/nblock)

nysd_temp$GEOID<- as.character(nysd_temp$censustract)
temp<-merge(us1, nysd_temp,  by='GEOID', all.x=TRUE)
temp<-st_as_sf(temp)# need to do this, otherwise the mapping does not work 

color_pal <- colorNumeric(palette = "viridis", domain = temp$full_time_work_behavior_devices)
#color_pal <- colorQuantile(palette = "viridis", 
#                          domain = temp$meandevice, n=5)

temp %>%
  st_transform(crs = "+init=epsg:4326") %>%
  leaflet(width = "100%") %>%
  addProviderTiles(provider = "CartoDB.Positron") %>%
  addPolygons(popup = ~ str_extract(GEOID, "^([^,]*)"),
              stroke = FALSE,
              smoothFactor = 0,
              fillOpacity = 0.7,
              color = ~ color_pal(full_time_work_behavior_devices)) %>%
  addLegend("bottomright", 
            pal = color_pal, 
            values = ~ full_time_work_behavior_devices,
            title = "",
            opacity = 1)

temp<- nysd[nysd$censustract=='36061003100', ]
plotly::plot_ly(data= temp, x =temp$date1, 
                y=temp$full_time_work_behavior_devices)



#--------------------------------------------
#read in the coreplaces file
#-----------------------------------------------

placeCountyCBG <- read.csv("D:/Dropbox/Censustract_demo/NY/placeCountyCBG.csv")
core_poi.part1 <- fread("D:/safegraph/core/core_poi-part1.csv")
core_poi.part2 <- fread("D:/safegraph/core/core_poi-part2.csv")
core_poi.part3 <- fread("D:/safegraph/core/core_poi-part3.csv")
core_poi.part4 <- fread("D:/safegraph/core/core_poi-part4.csv")
core_all<-rbind(core_poi.part1, core_poi.part2, core_poi.part3,  core_poi.part4 )

head(core_all)
length(unique(core_all$safegraph_place_id))
length(unique(placeCountyCBG$safegraph_place_id))
# merge in the cbg file here 
poi<-merge(core_all, placeCountyCBG, by= 'safegraph_place_id', all.y=TRUE)
View(summary(as.factor(core_all$sub_category)))

cbgsummary<-poi%>%group_by(CBGFIPS, top_category, stateFIPS, countyFIPS, 
                           state, countyName  )%>%summarise(n=n())
cbgsummary$top_category[is.na(cbgsummary$top_category)]<-'NotIden'
  
fwrite(poi, 'coreplaces.csv')
fwrite(cbgsummary, 'cbg_poi_summary.csv')

#--------------------------------------------------
poi<-fread('coreplaces.csv')
poi$censustract<-stringr::str_sub(poi$CBGFIPS, 1, 11)
poi_tract<-poi%>%group_by(censustract, top_category, stateFIPS, countyFIPS, 
                          state, countyName  )%>%summarise(n=n())
ntract<- tidyr::spread(poi_tract, top_category, n  )

colnames(ntract)
ntract[is.na(ntract)]<-0
colnames(ntract)[6]<-'notiden'
ntract$totalbuildings = apply(ntract[,6:179], 1, sum)
ntract[,6:179]<-ntract[,6:179]/ntract$totalbuildings

fwrite(ntract, 'tract_poi_summary.csv')
ntract<- fread('tract_poi_summary.csv')




