library(sf)
library(stringr)
library(leaflet)
library(dplyr)
library(data.table)

inter.file<-'data.gdb'
#st_layers(iner.file)
street<-st_read('data.gdb',layer='Melbourne_Street_Names_MGA')
citycir<-st_read('data.gdb',layer='Melbourne_CityCircle_tram_MGA')
bus<-st_read('data.gdb',layer='BusMetroRoutes')
muni<-st_read('data.gdb',layer='Melbourne_Municipal_Boundary_MGA')




muni<-st_transform(muni,4326)
citycir<-st_transform(citycir,4326)
street<-st_transform(street,4326)
bus<-st_transform(bus,4326)
bus<-st_zm(bus,drop=T)  ###turning stringz to string
street<-st_intersection(street,muni)
bus<-st_intersection(bus,muni)

street<-street %>% select(name)
bus<- bus %>% select(ROUTE_SHORT_NAME,ROUTE_LONG_NAME,ROUTE_KM,FIRST_STOP_NAME,LAST_STOP_NAME,NUM_OF_STOPS)

bike<-fread('Bike_Share_Dock_Locations.csv')

bike<-st_as_sf(x = bike,                         
         coords = c("lon", "lat"),
         crs = 4326)
		 
bike<-st_intersection(bike,muni)

save(muni,citycir,bus,street,bike,file='geo.RData')



################
library(data.table)
sensor.f1<-'crowdness/Pedestrian_Counting_System_-_Sensor_Locations.csv'
sensor.f2<-'crowdness/Pedestrian_Counting_System_-_Monthly__counts_per_hour_.csv'

sen1<-fread(sensor.f1)
sen2<-fread(sensor.f2)
sen1<-sen1[order(sensor_id),.(Sensor_ID=sensor_id,sensor_description,longitude,latitude)]

sen2[,Month:=ordered(Month,month.name,month.abb)]
sen2[,Hourly_Counts:=Hourly_Counts/1e3]  ###to thousands
den<-sen2[,.(count=sum(Hourly_Counts)),by=c('Sensor_ID','Month')]

save(sen1,den,file='sensor.RData')
