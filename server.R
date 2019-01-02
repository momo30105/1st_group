library(shiny)
library(leaflet) 
library(rgdal) 
library(readr)
library(jsonlite)
library(RCurl)
library(bitops)
library(Rcpp)
library(ggplot2)
library(data.table)
library(hms)
library(pkgconfig)
library(datasets)
library(utils)
library(stats)
library(sp)
library(methods)
library(graphics)
library(grDevices)
library(XML)
library(rvest)
library(xml2)
library(maptools)
library(gstat)
library(gpclib)
library(leaflet.extras)


#運算政府IDW
getdata_gov<-reactive({ 
invalidateLater(900000, session = NULL) 
shape <- readOGR("/home/hpc/1x1_WGS4326", "taichungmap_1x1")
shape_new <- readOGR("/home/hpc/1x1_0830","1x1")

#get gov aqi
url<-'http://taqm.epb.taichung.gov.tw/TQAMNEWAQITABLE.ASPX'
data1<-readHTMLTable(url,stringsAsFactors = FALSE,as.data.frame = TRUE,header = T)
data2<-data1[2]$GridView1[-1,-c(5,7,9,11,13,15)]
colnames(data2)<-c('Sname','對健康影響等級','AQI','指標污染物','二氧化硫(SO2)','一氧化碳(CO','臭氧(O3)','二氧化氮(NO2)','懸浮微粒(PM10)','細懸浮微粒(PM2.5)')
rownames(data2)<-c('1','2','3','4','5','6','7','8','9','10','11','12','13','14','15','16')
gov<-data2


gov_16 <- read.csv("/home/hpc/16latlong.csv", fileEncoding="big5")
gov_16 <- merge(gov_16, gov, by = "Sname") 

#準備要idw的data
idw_data <-data.frame(name=gov_16$Sname,lon=gov_16$Slong,lat=gov_16$Slat,pm2.5=gov_16$`細懸浮微粒(PM2.5)`,stringsAsFactors = FALSE)
idw_data$pm2.5<-as.integer(idw_data$pm2.5)

#地圖格內資訊表
count<-read.csv("/home/hpc/air_2826.csv")
count$idw_value<-count$count


count$pm2.5<-as.integer(count$pm2.5)

#idw

estonia_air_temperature_2_test <- idw_data  # duplicate air temp. data file
estonia_air_temperature_2_test$x <- estonia_air_temperature_2_test$lon  # define x & y as longitude and latitude
estonia_air_temperature_2_test$y <- estonia_air_temperature_2_test$lat
coordinates(estonia_air_temperature_2_test) = ~x + y
plot(estonia_air_temperature_2_test)
x.range <- as.numeric(c(120.4533, 121.45765))  # min/max longitude of the interpolation area
y.range <- as.numeric(c(23.990715, 24.451395))  # min/max latitude of the interpolation area

#3X3 0.0295 0.02709
#1X1 0.00985 0.00904
grd <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = 0.00985), y = seq(from = y.range[1],
                                                                                      to = y.range[2], by = 0.00904))  # expand points to grid
coordinates(grd) <- ~x + y
gridded(grd) <- TRUE
plot(grd, cex = 1.5, col = "grey")

points(estonia_air_temperature_2_test, pch = 1, col = "red", cex = 1)
idw <- idw(formula = pm2.5 ~ 1, locations = estonia_air_temperature_2_test,
           newdata = grd)
idw.output = as.data.frame(idw)  # output is defined as a data table
names(idw.output)[1:3] <- c("long", "lat", "var1.pred")  # give names to the modelled variables


idwcount<-nrow(idw.output)


for (a in c(1:idwcount)) {  #idw.output in tc
  lonn<-idw.output$long[a]   #要判斷的點經緯度
  latt<-idw.output$lat[a]
  
  for (b in c(0:313)) {
    l<-7+b*9
    r<-3+b*9
    left_lon<-shape@polygons[[l]]@Polygons[[1]]@coords[1,1]
    left_lat<-shape@polygons[[l]]@Polygons[[1]]@coords[1,2]   
    right_lon<-shape@polygons[[r]]@Polygons[[1]]@coords[3,1]
    right_lat<-shape@polygons[[r]]@Polygons[[1]]@coords[3,2]
    
    if(left_lon<lonn&&lonn<right_lon&&left_lat<latt&&latt<right_lat){ #若IDW在某大格內(確認四個點)
      rr<-r-2
      ll<-l+2
      for (c in c(rr:ll)) {
        left_lon<-shape@polygons[[c]]@Polygons[[1]]@coords[1,1]
        left_lat<-shape@polygons[[c]]@Polygons[[1]]@coords[1,2]   
        right_lon<-shape@polygons[[c]]@Polygons[[1]]@coords[3,1]
        right_lat<-shape@polygons[[c]]@Polygons[[1]]@coords[3,2]
        
        if(left_lon<lonn&&lonn<right_lon&&left_lat<latt&&latt<right_lat){ #若IDW在某格內(確認四個點)
          count$idw_value[c]<-round(idw$var1.pred[a])
          break
        }
      }
      break
    }
  }
}


#merge to shape＠data 
#shape <- merge(shape, count, by = "Id") 
shape_new <- merge(shape_new,count,by="Id")
return(shape_new)
#
})


getdata_airbox<-reactive({ 
  invalidateLater(300000, session = NULL) 
  shape <- readOGR("/home/hpc/1x1_WGS4326", "taichungmap_1x1")
  shape_new <- readOGR("/home/hpc/1x1_0830","1x1")
  #get airbox json
  airbox<-fromJSON(getURL("https://data.lass-net.org/data/last-all-airbox.json"))
  dfairbox1<-data.frame(device_id=airbox$feeds$device_id, lon=airbox$feeds$gps_lon, lat=airbox$feeds$gps_lat,pm2.5=airbox$feeds$s_d0,stringsAsFactors = FALSE)
  
  
  
  #準備要idw的data
  idw_data <-data.frame(stringsAsFactors = FALSE)
  
  
  #地圖格內資訊表
  count<-read.csv("/home/hpc/air_2826.csv")
  count$idw_value<-count$count
  #初步篩選台中範圍測站
  check <- dfairbox1[dfairbox1$lon <= 121.457 & dfairbox1$lon >= 120.447 & dfairbox1$lat <= 24.443 & dfairbox1$lat >= 23.983 , c("device_id","lon","lat","pm2.5")]
  
  #check新增id欄位
  tmp<-"0"
  for (x in c(1:256)) {
    y<-",0"
    paste(tmp,y)
  }
  id<-c(tmp)
  check$id<-tmp
  
  airbox_in_tc<-data.frame(stringsAsFactors = FALSE)
  
  
  cc<-nrow(check)
  
  #3x3=314   1x1=2826
  idnums<-2826
  
  #cc為初步篩選的空氣盒子資料 與所有格子比對
  for (a in c(1:cc)) {
    for (b in c(1:idnums)) {
      lonn<-check$lon[a]
      latt<-check$lat[a]
      left_lon<-shape@polygons[[b]]@Polygons[[1]]@coords[1,1]
      left_lat<-shape@polygons[[b]]@Polygons[[1]]@coords[1,2]
      right_lon<-shape@polygons[[b]]@Polygons[[1]]@coords[3,1]
      right_lat<-shape@polygons[[b]]@Polygons[[1]]@coords[3,2]
      
      
      if(left_lon<lonn&&lonn<right_lon&&left_lat<latt&&latt<right_lat){ #若在某格內(確認四個點)
        
        check$id[a]<-shape@data$Id[b]
        count$count[b]<-count$count[b]+1
        count$total[b]<-count$total[b]+check$pm2.5[a]
        
        #加進台中範圍內資料表格
        airbox_in_tc<-rbind(airbox_in_tc,data.frame(device_id=check$device_id[a],
                                                    lon=check$lon[a],
                                                    lat=check$lat[a],
                                                    pm2.5=check$pm2.5[a],
                                                    id=check$id[a],stringsAsFactors = FALSE))
        #加進idw資料表格
        idw_data<-rbind(idw_data,data.frame(name=check$device_id[a],
                                            lon=check$lon[a],
                                            lat=check$lat[a],
                                            pm2.5=check$pm2.5[a],
                                            stringsAsFactors = FALSE))
      }
    }
  }
  airbox_in_tc$id<-as.integer(airbox_in_tc$id)
  
  count$pm2.5<-as.integer(count$pm2.5)
  
  #idw
  
  estonia_air_temperature_2_test <- idw_data  # duplicate air temp. data file
  estonia_air_temperature_2_test$x <- estonia_air_temperature_2_test$lon  # define x & y as longitude and latitude
  estonia_air_temperature_2_test$y <- estonia_air_temperature_2_test$lat
  coordinates(estonia_air_temperature_2_test) = ~x + y
  plot(estonia_air_temperature_2_test)
  x.range <- as.numeric(c(120.4533, 121.45765))  # min/max longitude of the interpolation area
  y.range <- as.numeric(c(23.990715, 24.451395))  # min/max latitude of the interpolation area
  
  #3X3 0.0295 0.02709
  #1X1 0.00985 0.00904
  grd <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = 0.00985), y = seq(from = y.range[1],
                                                                                        to = y.range[2], by = 0.00904))  # expand points to grid
  coordinates(grd) <- ~x + y
  gridded(grd) <- TRUE
  plot(grd, cex = 1.5, col = "grey")
  
  points(estonia_air_temperature_2_test, pch = 1, col = "red", cex = 1)
  idw <- idw(formula = pm2.5 ~ 1, locations = estonia_air_temperature_2_test,
             newdata = grd)
  idw.output = as.data.frame(idw)  # output is defined as a data table
  names(idw.output)[1:3] <- c("long", "lat", "var1.pred")  # give names to the modelled variables
  
  
  idwcount<-nrow(idw.output)
  
  
  for (a in c(1:idwcount)) {  #idw.output in tc
    lonn<-idw.output$long[a]   #要判斷的點經緯度
    latt<-idw.output$lat[a]
    
    for (b in c(0:313)) {
      l<-7+b*9
      r<-3+b*9
      left_lon<-shape@polygons[[l]]@Polygons[[1]]@coords[1,1]
      left_lat<-shape@polygons[[l]]@Polygons[[1]]@coords[1,2]   
      right_lon<-shape@polygons[[r]]@Polygons[[1]]@coords[3,1]
      right_lat<-shape@polygons[[r]]@Polygons[[1]]@coords[3,2]
      
      if(left_lon<lonn&&lonn<right_lon&&left_lat<latt&&latt<right_lat){ #若IDW在某大格內(確認四個點)
        rr<-r-2
        ll<-l+2
        for (c in c(rr:ll)) {
          left_lon<-shape@polygons[[c]]@Polygons[[1]]@coords[1,1]
          left_lat<-shape@polygons[[c]]@Polygons[[1]]@coords[1,2]   
          right_lon<-shape@polygons[[c]]@Polygons[[1]]@coords[3,1]
          right_lat<-shape@polygons[[c]]@Polygons[[1]]@coords[3,2]
          
          if(left_lon<lonn&&lonn<right_lon&&left_lat<latt&&latt<right_lat){ #若IDW在某格內(確認四個點)
            count$idw_value[c]<-round(idw$var1.pred[a])
            break
          }
        }
        break
      }
    }
  }
  
  
  #merge to shape＠data 
  #shape <- merge(shape, count, by = "Id") 
  shape_new <- merge(shape_new,count,by="Id")
  return(shape_new)
  #
})

airbox_data <- reactive({ #當前airbox資料
  invalidateLater(300000, session = NULL)
  invalidateLater(300000, session = NULL) 
  shape <- readOGR("/home/hpc/1x1_WGS4326", "taichungmap_1x1")
  #get airbox json
  airbox<-fromJSON(getURL("https://data.lass-net.org/data/last-all-airbox.json"))
  dfairbox1<-data.frame(device_id=airbox$feeds$device_id, lon=airbox$feeds$gps_lon, lat=airbox$feeds$gps_lat,pm2.5=airbox$feeds$s_d0,stringsAsFactors = FALSE)
  
  #初步篩選台中範圍測站
  check <- dfairbox1[dfairbox1$lon <= 121.457 & dfairbox1$lon >= 120.447 & dfairbox1$lat <= 24.443 & dfairbox1$lat >= 23.983 , c("device_id","lon","lat","pm2.5")]
  
  #check新增id欄位
  tmp<-"0"
  for (x in c(1:256)) {
    y<-",0"
    paste(tmp,y)
  }
  id<-c(tmp)
  check$id<-tmp
  
  airbox_in_tc<-data.frame(stringsAsFactors = FALSE)
  
  
  cc<-nrow(check)
  
  #3x3=314   1x1=2826
  idnums<-2826
  
  #cc為初步篩選的空氣盒子資料 與所有格子比對
  for (a in c(1:cc)) {
    for (b in c(1:idnums)) {
      lonn<-check$lon[a]
      latt<-check$lat[a]
      left_lon<-shape@polygons[[b]]@Polygons[[1]]@coords[1,1]
      left_lat<-shape@polygons[[b]]@Polygons[[1]]@coords[1,2]
      right_lon<-shape@polygons[[b]]@Polygons[[1]]@coords[3,1]
      right_lat<-shape@polygons[[b]]@Polygons[[1]]@coords[3,2]
      
      
      if(left_lon<lonn&&lonn<right_lon&&left_lat<latt&&latt<right_lat){ #若在某格內(確認四個點)
        
        check$id[a]<-shape@data$Id[b]

        
        #加進台中範圍內資料表格
        airbox_in_tc<-rbind(airbox_in_tc,data.frame(device_id=check$device_id[a],
                                                    lon=check$lon[a],
                                                    lat=check$lat[a],
                                                    pm2.5=check$pm2.5[a],
                                                    id=check$id[a],stringsAsFactors = FALSE))
        
      }
    }
  }
  airbox_in_tc$id<-as.integer(airbox_in_tc$id)
  return(airbox_in_tc)
}) 

#混合IDW
getdata_mix<-reactive({
  invalidateLater(300000, session = NULL) 
  shape <- readOGR("/home/hpc/1x1_WGS4326", "taichungmap_1x1")
  shape_new <- readOGR("/home/hpc/1x1_0830","1x1")
  #get airbox json
  airbox<-fromJSON(getURL("https://data.lass-net.org/data/last-all-airbox.json"))
  dfairbox1<-data.frame(device_id=airbox$feeds$device_id, lon=airbox$feeds$gps_lon, lat=airbox$feeds$gps_lat,pm2.5=airbox$feeds$s_d0,stringsAsFactors = FALSE)
  
  #get gov aqi
  url<-'http://taqm.epb.taichung.gov.tw/TQAMNEWAQITABLE.ASPX'
  data1<-readHTMLTable(url,stringsAsFactors = FALSE,as.data.frame = TRUE,header = T)
  data2<-data1[2]$GridView1[-1,-c(5,7,9,11,13,15)]
  colnames(data2)<-c('Sname','對健康影???等???','AQI','??????污???物','二氧???硫(SO2)','一氧???碳(CO','??????(O3)','二氧???氮(NO2)','???浮微???(PM10)','細懸浮微???(PM2.5)')
  rownames(data2)<-c('1','2','3','4','5','6','7','8','9','10','11','12','13','14','15','16')
  gov<-data2
  
  
  gov_16 <- read.csv("/home/hpc/16latlong.csv", fileEncoding="big5")
  gov_16 <- merge(gov_16, gov, by = "Sname") 
  
  #idw data
  idw_data <-data.frame(name=gov_16$Sname,lon=gov_16$Slong,lat=gov_16$Slat,pm2.5=gov_16$`細懸浮微???(PM2.5)`,stringsAsFactors = FALSE)
  
  
  #??????格???資???表
  count<-read.csv("/home/hpc/air_2826.csv")
  count$idw_value<-count$count
  #???步篩選???中??????測???
  check <- dfairbox1[dfairbox1$lon <= 121.457 & dfairbox1$lon >= 120.447 & dfairbox1$lat <= 24.443 & dfairbox1$lat >= 23.983 , c("device_id","lon","lat","pm2.5")]
  
  
  tmp<-"0"
  for (x in c(1:256)) {
    y<-",0"
    paste(tmp,y)
  }
  id<-c(tmp)
  check$id<-tmp
  
  airbox_in_tc<-data.frame(stringsAsFactors = FALSE)
  
  
  cc<-nrow(check)
  
  #3x3=314   1x1=2826
  idnums<-2826
  
  for (a in c(1:cc)) {
    for (b in c(1:idnums)) {
      lonn<-check$lon[a]
      latt<-check$lat[a]
      left_lon<-shape@polygons[[b]]@Polygons[[1]]@coords[1,1]
      left_lat<-shape@polygons[[b]]@Polygons[[1]]@coords[1,2]
      right_lon<-shape@polygons[[b]]@Polygons[[1]]@coords[3,1]
      right_lat<-shape@polygons[[b]]@Polygons[[1]]@coords[3,2]
      
      
      if(left_lon<lonn&&lonn<right_lon&&left_lat<latt&&latt<right_lat){ #?????????格???(確??????個???)
        
        check$id[a]<-shape@data$Id[b]
        count$count[b]<-count$count[b]+1
        count$total[b]<-count$total[b]+check$pm2.5[a]
        
        #???進台中??????內資???表???
        airbox_in_tc<-rbind(airbox_in_tc,data.frame(device_id=check$device_id[a],
                                                    lon=check$lon[a],
                                                    lat=check$lat[a],
                                                    pm2.5=check$pm2.5[a],
                                                    id=check$id[a],stringsAsFactors = FALSE))
        #???進idw資???表???
        idw_data<-rbind(idw_data,data.frame(name=check$device_id[a],
                                            lon=check$lon[a],
                                            lat=check$lat[a],
                                            pm2.5=check$pm2.5[a],
                                            stringsAsFactors = FALSE))
      }
    }
  }
  airbox_in_tc$id<-as.integer(airbox_in_tc$id)
  
  
  #算PM2.5??????
  for (b in c(1:idnums)) {
    if(count$count[b]!=0){
      count$pm2.5[b]<-count$total[b]/count$count[b]
    }else{
      count$pm2.5[b]<-0
    }
  }
  
  
  for (a in c(1:16)) {  #16_gov
    for (b in c(1:idnums)) {
      lonn<-gov_16$Slong[a]
      latt<-gov_16$Slat[a]
      left_lon<-shape@polygons[[b]]@Polygons[[1]]@coords[1,1]
      left_lat<-shape@polygons[[b]]@Polygons[[1]]@coords[1,2]
      right_lon<-shape@polygons[[b]]@Polygons[[1]]@coords[3,1]
      right_lat<-shape@polygons[[b]]@Polygons[[1]]@coords[3,2]
      
      
      if(left_lon<lonn&&lonn<right_lon&&left_lat<latt&&latt<right_lat){ #?????????格???(確??????個???)
        
        count$pm2.5[b]<-gov_16$`細懸浮微???(PM2.5)`[a]
        
        
      }
    }
  }
  
  count$pm2.5<-as.integer(count$pm2.5)
  
  #idw
  
  estonia_air_temperature_2_test <- idw_data  # duplicate air temp. data file
  estonia_air_temperature_2_test$x <- estonia_air_temperature_2_test$lon  # define x & y as longitude and latitude
  estonia_air_temperature_2_test$y <- estonia_air_temperature_2_test$lat
  coordinates(estonia_air_temperature_2_test) = ~x + y
  plot(estonia_air_temperature_2_test)
  x.range <- as.numeric(c(120.4533, 121.45765))  # min/max longitude of the interpolation area
  y.range <- as.numeric(c(23.990715, 24.451395))  # min/max latitude of the interpolation area
  
  #3X3 0.0295 0.02709
  #1X1 0.00985 0.00904
  grd <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = 0.00985), y = seq(from = y.range[1],
                                                                                        to = y.range[2], by = 0.00904))  # expand points to grid
  coordinates(grd) <- ~x + y
  gridded(grd) <- TRUE
  plot(grd, cex = 1.5, col = "grey")
  
  points(estonia_air_temperature_2_test, pch = 1, col = "red", cex = 1)
  idw <- idw(formula = pm2.5 ~ 1, locations = estonia_air_temperature_2_test,
             newdata = grd)
  idw.output = as.data.frame(idw)  # output is defined as a data table
  names(idw.output)[1:3] <- c("long", "lat", "var1.pred")  # give names to the modelled variables
  
  
  idwcount<-nrow(idw.output)
  
  
  for (a in c(1:idwcount)) {  #idw.output in tc
    lonn<-idw.output$long[a]   #要判????????????緯???
    latt<-idw.output$lat[a]
    
    for (b in c(0:313)) {
      l<-7+b*9
      r<-3+b*9
      left_lon<-shape@polygons[[l]]@Polygons[[1]]@coords[1,1]
      left_lat<-shape@polygons[[l]]@Polygons[[1]]@coords[1,2]   
      right_lon<-shape@polygons[[r]]@Polygons[[1]]@coords[3,1]
      right_lat<-shape@polygons[[r]]@Polygons[[1]]@coords[3,2]
      
      if(left_lon<lonn&&lonn<right_lon&&left_lat<latt&&latt<right_lat){ #???IDW??????大??????(確??????個???)
        rr<-r-2
        ll<-l+2
        for (c in c(rr:ll)) {
          left_lon<-shape@polygons[[c]]@Polygons[[1]]@coords[1,1]
          left_lat<-shape@polygons[[c]]@Polygons[[1]]@coords[1,2]   
          right_lon<-shape@polygons[[c]]@Polygons[[1]]@coords[3,1]
          right_lat<-shape@polygons[[c]]@Polygons[[1]]@coords[3,2]
          
          if(left_lon<lonn&&lonn<right_lon&&left_lat<latt&&latt<right_lat){ #???IDW??????格???(確??????個???)
            count$idw_value[c]<-round(idw$var1.pred[a])
            break
          }
        }
        break
      }
    }
  }
  
  
  #merge to shape＠data 
  #shape <- merge(shape, count, by = "Id") 
  shape_new <- merge(shape_new,count,by="Id")
  return(shape_new)
  #
})



gov_16 <- reactive({ #當前政府16資料
  invalidateLater(300000, session = NULL)
  gov_15 <- read.csv("/home/hpc/16latlong.csv", fileEncoding="big5")
  count<-read.csv("/home/hpc/air_2826.csv")
  url<-'http://taqm.epb.taichung.gov.tw/TQAMNEWAQITABLE.ASPX'
  #reactive({
  #invalidateLater(3600000, session = NULL)
  #get airbox json
  #dfairbox1<-data.frame(device_id=airbox$feeds$device_id, lon=airbox$feeds$gps_lon, lat=airbox$feeds$gps_lat,pm2.5=airbox$feeds$s_d0,stringsAsFactors = FALSE)
  #get gov aqi
  
  data1<-readHTMLTable(url,stringsAsFactors = FALSE,as.data.frame = TRUE,header = T)
  data2<-data1[2]$GridView1[-1,-c(5,7,9,11,13,15)]
  colnames(data2)<-c('Sname','對健康影響等級','AQI','指標污染物','二氧化硫(SO2)','一氧化碳(CO','臭氧(O3)','二氧化氮(NO2)','懸浮微粒(PM10)','細懸浮微粒(PM2.5)')
  rownames(data2)<-c('1','2','3','4','5','6','7','8','9','10','11','12','13','14','15','16')
  gov<-data2
  gov_15 <- merge(gov_15, gov, by = "Sname") 
  return(gov_15)
})  

wind<- reactive({ #當前風向
  invalidateLater(300000, session = NULL)
  airbox<-fromJSON(getURL("https://opendata.cwb.gov.tw/api/v1/rest/datastore/O-A0001-001?authorizationkey=CWB-3B0A64B1-262B-4FF9-A733-38B48FA9A24A&locationName=%E8%87%BA%E4%B8%AD,%E6%A2%A7%E6%A3%B2,%E5%A4%A7%E8%82%9A,%E9%9B%AA%E5%B1%B1%E5%9C%88%E8%B0%B7,%E7%9F%B3%E5%B2%A1,%E4%B8%AD%E5%9D%91,%E5%AF%A9%E9%A6%AC%E9%99%A3A,%E5%8D%97%E6%B9%96%E5%9C%88%E8%B0%B7A,%E6%9D%B1%E5%8B%A2,%E6%A2%A8%E5%B1%B1,%E5%A4%A7%E7%94%B2,%E5%A4%A7%E5%9D%91,%E4%B8%AD%E7%AB%B9%E6%9E%97,%E7%A5%9E%E5%B2%A1,%E5%A4%A7%E5%AE%89,%E5%90%8E%E9%87%8C,%E8%B1%90%E5%8E%9F,%E5%A4%A7%E9%87%8C,%E6%BD%AD%E5%AD%90,%E6%B8%85%E6%B0%B4,%E5%A4%96%E5%9F%94,%E9%BE%8D%E4%BA%95,%E7%83%8F%E6%97%A5,%E8%A5%BF%E5%B1%AF,%E5%8D%97%E5%B1%AF,%E6%96%B0%E7%A4%BE,%E5%A4%A7%E9%9B%85(%E4%B8%AD%E7%A7%91%E5%9C%92%E5%8D%80),%E6%A1%83%E5%B1%B1A,%E9%9B%AA%E5%B1%B1%E6%9D%B1%E5%B3%B0A,%E8%8A%B1%E5%8D%9A%E8%B1%90%E5%8E%9F,%E8%8A%B1%E5%8D%9A%E5%A4%96%E5%9F%94,%E8%8A%B1%E5%8D%9A%E5%90%8E%E9%87%8C,%E6%AD%A6%E9%99%B5,%E7%A8%AE%E8%8B%97%E7%B9%81%E6%AE%96&elementName=WDIR,WDSD&sort=%7Bsort%7D&parameterName=CITY"))
  dfairbox1<-data.frame(lat=airbox$records$location$lat, 
                        lon=airbox$records$location$lon, 
                        locationName=airbox$records$location$locationName,
                        stationId=airbox$records$location$stationId,
                        Stime=airbox$records$location$time,
                        stringsAsFactors = FALSE)
  tmp<-""
  dfairbox1$WDIR<-c(tmp)
  dfairbox1$WDSD<-c(tmp)
  dfairbox1$WDIR_2<-c(tmp)
  rowcount<-nrow(dfairbox1)
  dfairbox1<-dfairbox1[-rowcount,]
  rowcount<-nrow(dfairbox1)
  
  for (a in c(1:rowcount)) {
    dfairbox1$WDIR[a]<-airbox$records$location$weatherElement[[a]][1,2]
    dfairbox1$WDSD[a]<-airbox$records$location$weatherElement[[a]][2,2]
    dfairbox1$WDIR<-as.numeric(dfairbox1$WDIR)
    if(dfairbox1$WDIR[a]>348.75){
      dfairbox1$WDIR_2[a]<-"N"
    }else if(dfairbox1$WDIR[a]<=11.25){
      dfairbox1$WDIR_2[a]<-"N"
    }else if(dfairbox1$WDIR[a]>11.25 & dfairbox1$WDIR[a]<=33.75){
      dfairbox1$WDIR_2[a]<-"NNE"
    }else if(dfairbox1$WDIR[a]>33.75 & dfairbox1$WDIR[a]<=56.25){
      dfairbox1$WDIR_2[a]<-"NE"
    }else if(dfairbox1$WDIR[a]>56.25 & dfairbox1$WDIR[a]<=78.75){
      dfairbox1$WDIR_2[a]<-"ENE"
    }else if(dfairbox1$WDIR[a]>78.75 & dfairbox1$WDIR[a]<=101.25){
      dfairbox1$WDIR_2[a]<-"E"
    }else if(dfairbox1$WDIR[a]>101.25 & dfairbox1$WDIR[a]<=123.75){
      dfairbox1$WDIR_2[a]<-"ESE"
    }else if(dfairbox1$WDIR[a]>123.75 & dfairbox1$WDIR[a]<=146.25){
      dfairbox1$WDIR_2[a]<-"SE"
    }else if(dfairbox1$WDIR[a]>146.25 & dfairbox1$WDIR[a]<=168.75){
      dfairbox1$WDIR_2[a]<-"SSE"
    }else if(dfairbox1$WDIR[a]>168.75 & dfairbox1$WDIR[a]<=191.25){
      dfairbox1$WDIR_2[a]<-"S"
    }else if(dfairbox1$WDIR[a]>191.25 & dfairbox1$WDIR[a]<=213.75){
      dfairbox1$WDIR_2[a]<-"SSW"
    }else if(dfairbox1$WDIR[a]>213.75 & dfairbox1$WDIR[a]<=236.25){
      dfairbox1$WDIR_2[a]<-"SW"
    }else if(dfairbox1$WDIR[a]>236.25 & dfairbox1$WDIR[a]<=258.75){
      dfairbox1$WDIR_2[a]<-"WSW"
    }else if(dfairbox1$WDIR[a]>258.75 & dfairbox1$WDIR[a]<=281.25){
      dfairbox1$WDIR_2[a]<-"W"
    }else if(dfairbox1$WDIR[a]>281.25 & dfairbox1$WDIR[a]<=303.75){
      dfairbox1$WDIR_2[a]<-"WNW"
    }else if(dfairbox1$WDIR[a]>303.75 & dfairbox1$WDIR[a]<=326.25){
      dfairbox1$WDIR_2[a]<-"NW"
    }else if(dfairbox1$WDIR[a]>326.25 & dfairbox1$WDIR[a]<=348.75){
      dfairbox1$WDIR_2[a]<-"NNW"
    }
    
  }
  dfairbox1$lat<-as.numeric(dfairbox1$lat)
  dfairbox1$lon<-as.numeric(dfairbox1$lon)
  return(dfairbox1)
})





forecast_7_pop<-reactive({
  invalidateLater(3600000, session = NULL) 
  nowtime_7<-Sys.time()+25200
  nowtime_7 <- substr(nowtime_7, 1, 13)
  nowtime_7 <- paste0(nowtime_7,":00:00")
  name<-paste0("/srv/shiny-server/sample-apps/gov/forecast_data/",nowtime_7,".csv")
  data_7 <- read.csv(name, fileEncoding="big5")
  paste0("<strong>Date: </strong>", data_7$Time, "<br>" ,"<strong>NAME: </strong>", data_7$Sname, "<br>", "<strong>PM2.5: </strong>", data_7$pm2.5)
})

# forecast_4_pop<-reactive({
#   paste0("<strong>Date: </strong>", forecast_4_data()$Time, "<br>" ,"<strong>NAME: </strong>", forecast_4_data()$Sname, "<br>", "<strong>PM2.5: </strong>", forecast_4_data()$pm2.5)
# })
# 
# forecast_2_pop<-reactive({
#   paste0("<strong>Date: </strong>", forecast_2_data()$Time, "<br>" ,"<strong>NAME: </strong>", forecast_2_data()$Sname, "<br>", "<strong>PM2.5: </strong>", forecast_2_data()$pm2.5)
# })
# 
# forecast_1_pop<-reactive({
#   paste0("<strong>Date: </strong>", forecast_1_data()$Time, "<br>" ,"<strong>NAME: </strong>", forecast_1_data()$Sname, "<br>", "<strong>PM2.5: </strong>", forecast_1_data()$pm2.5)
# })






#最新airbox popup 
i_popup_airbox<-reactive({ 
  invalidateLater(300000, session = NULL) 
  shape <- readOGR("/home/hpc/1x1_WGS4326", "taichungmap_1x1")
  #get airbox json
  airbox<-fromJSON(getURL("https://data.lass-net.org/data/last-all-airbox.json"))
  dfairbox1<-data.frame(device_id=airbox$feeds$device_id, lon=airbox$feeds$gps_lon, lat=airbox$feeds$gps_lat,pm2.5=airbox$feeds$s_d0,stringsAsFactors = FALSE)
  
  #初步篩選台中範圍測站
  check <- dfairbox1[dfairbox1$lon <= 121.457 & dfairbox1$lon >= 120.447 & dfairbox1$lat <= 24.443 & dfairbox1$lat >= 23.983 , c("device_id","lon","lat","pm2.5")]
  
  #check新增id欄位
  tmp<-"0"
  for (x in c(1:256)) {
    y<-",0"
    paste(tmp,y)
  }
  id<-c(tmp)
  check$id<-tmp
  
  airbox_in_tc<-data.frame(stringsAsFactors = FALSE)
  
  
  cc<-nrow(check)
  
  #3x3=314   1x1=2826
  idnums<-2826
  
  #cc為初步篩選的空氣盒子資料 與所有格子比對
  for (a in c(1:cc)) {
    for (b in c(1:idnums)) {
      lonn<-check$lon[a]
      latt<-check$lat[a]
      left_lon<-shape@polygons[[b]]@Polygons[[1]]@coords[1,1]
      left_lat<-shape@polygons[[b]]@Polygons[[1]]@coords[1,2]
      right_lon<-shape@polygons[[b]]@Polygons[[1]]@coords[3,1]
      right_lat<-shape@polygons[[b]]@Polygons[[1]]@coords[3,2]
      
      
      if(left_lon<lonn&&lonn<right_lon&&left_lat<latt&&latt<right_lat){ #若在某格內(確認四個點)
        
        check$id[a]<-shape@data$Id[b]
       
        #加進台中範圍內資料表格
        airbox_in_tc<-rbind(airbox_in_tc,data.frame(device_id=check$device_id[a],
                                                    lon=check$lon[a],
                                                    lat=check$lat[a],
                                                    pm2.5=check$pm2.5[a],
                                                    id=check$id[a],stringsAsFactors = FALSE))
        
      }
    }
  }
  airbox_in_tc$id<-as.integer(airbox_in_tc$id)
  
  pop <- paste0("<strong>NAME: </strong>", airbox_in_tc$device_id, "<br>", "<strong>PM2.5: </strong>", airbox_in_tc$pm2.5) 
  return(pop)
  #
})


#最新政府資料popup 
i_popup_gov <- reactive({
invalidateLater(300000, session = NULL)
url<-'http://taqm.epb.taichung.gov.tw/TQAMNEWAQITABLE.ASPX'
data1<-readHTMLTable(url,stringsAsFactors = FALSE,as.data.frame = TRUE,header = T)
data2<-data1[2]$GridView1[-1,-c(5,7,9,11,13,15)]
colnames(data2)<-c('Sname','對健康影響等級','AQI','指標污染物','二氧化硫(SO2)','一氧化碳(CO','臭氧(O3)','二氧化氮(NO2)','懸浮微粒(PM10)','細懸浮微粒(PM2.5)')
rownames(data2)<-c('1','2','3','4','5','6','7','8','9','10','11','12','13','14','15','16')
gov<-data2

aaa<-read_html(url)
aaa<-aaa%>%html_nodes(".auto-style1 span") %>% html_text()
govtime<-substr(aaa,17,31)

gov_16 <- read.csv("/home/hpc/16latlong.csv", fileEncoding="big5")
gov_16 <- merge(gov_16, gov, by = "Sname") 
pop <- paste0("<strong>Date: </strong>", govtime, "<br>" ,"<strong>NAME: </strong>", gov_16$Sname, "<br>", "<strong>PM2.5: </strong>", gov_16$`細懸浮微粒(PM2.5)`) 
return(pop)
})


leafIcons<-iconList( #風向圖
  E = makeIcon(iconUrl = "/home/hpc/windd/E.png",iconWidth = 45, iconHeight = 45,iconAnchorX = 0, iconAnchorY = 0),
  ENE = makeIcon(iconUrl = "/home/hpc/windd/ENE.png",iconWidth = 45, iconHeight = 45,iconAnchorX = 0, iconAnchorY = 0),
  ESE = makeIcon(iconUrl = "/home/hpc/windd/ESE.png",iconWidth = 45, iconHeight = 45,iconAnchorX = 0, iconAnchorY = 0),
  N = makeIcon(iconUrl = "/home/hpc/windd/N.png",iconWidth = 45, iconHeight = 45,iconAnchorX = 0, iconAnchorY = 0),
  NE = makeIcon(iconUrl = "/home/hpc/windd/NE.png",iconWidth = 45, iconHeight = 45,iconAnchorX = 0, iconAnchorY = 0),
  NNE = makeIcon(iconUrl = "/home/hpc/windd/NNE.png",iconWidth = 45, iconHeight = 45,iconAnchorX = 0, iconAnchorY = 0),
  NNW = makeIcon(iconUrl = "/home/hpc/windd/NNW.png",iconWidth = 45, iconHeight = 45,iconAnchorX = 0, iconAnchorY = 0),
  NW = makeIcon(iconUrl = "/home/hpc/windd/NW.png",iconWidth = 45, iconHeight = 45,iconAnchorX = 0, iconAnchorY = 0),
  S = makeIcon(iconUrl = "/home/hpc/windd/S.png",iconWidth = 45, iconHeight = 45,iconAnchorX = 0, iconAnchorY = 0),
  SE = makeIcon(iconUrl = "/home/hpc/windd/SE.png",iconWidth = 45, iconHeight = 45,iconAnchorX = 0, iconAnchorY = 0),
  SSE = makeIcon(iconUrl = "/home/hpc/windd/SSE.png",iconWidth = 45, iconHeight = 45,iconAnchorX = 0, iconAnchorY = 0),
  SSW = makeIcon(iconUrl = "/home/hpc/windd/SSW.png",iconWidth = 45, iconHeight = 45,iconAnchorX = 0, iconAnchorY = 0),
  SW = makeIcon(iconUrl = "/home/hpc/windd/SW.png",iconWidth = 45, iconHeight = 45,iconAnchorX = 0, iconAnchorY = 0),
  W = makeIcon(iconUrl = "/home/hpc/windd/W.png",iconWidth = 45, iconHeight = 45,iconAnchorX = 0, iconAnchorY = 0),
  WNW = makeIcon(iconUrl = "/home/hpc/windd/WNW.png",iconWidth = 45, iconHeight = 45,iconAnchorX = 0, iconAnchorY = 0),
  W = makeIcon(iconUrl = "/home/hpc/windd/WSW.png",iconWidth = 45, iconHeight = 45,iconAnchorX = 0, iconAnchorY = 0)
)

#popup shapefile 點格子內
i_popup2 <-reactive({ 
paste0("<strong>ID: </strong>", getdata_gov()$Id, "<br>", "<strong>pm2.5: </strong>", getdata_gov()$idw_value)
})  

#popup shapefile 點格子內
i_popup3 <-reactive({ 
  paste0("<strong>ID: </strong>", getdata_airbox()$Id, "<br>", "<strong>pm2.5: </strong>", getdata_airbox()$idw_value)
})

#mix點格子內
i_popup2_mix <-reactive({ 
  paste0("<strong>ID: </strong>", getdata_mix()$Id, "<br>", "<strong>pm2.5: </strong>", getdata_mix()$idw_value)
}) 


popup_wind<- reactive({
  paste0("<strong>站名: </strong>", wind()$locationName, "<br>",
                "<strong>觀測時間: </strong>", wind()$obsTime, "<br>",
                "<strong>風向(角度): </strong>", wind()$WDIR, "<br>", 
                "<strong>風向: </strong>", wind()$WDIR_2, "<br>", 
                "<strong>風力(m/s): </strong>", wind()$WDSD)
})  

#i_popup2 <- paste0("<strong>ID: </strong>", shape$Id, "<br>", "<strong>pm2.5: </strong>", shape$pm2.5)

#popup airbox
#i_popup1 <- paste0("<strong>Date: </strong>", airbox$feeds$date[1], "<br>","<strong>Time: </strong>", airbox$feeds$time[1], "<br>", "<strong>Name: </strong>", airbox_in_tc$device_id, "<br>", "<strong>pm2.5: </strong>", airbox_in_tc$pm2.5) 

#i_popup_idw <- paste0("<strong>ID: </strong>", shape$Id, "<br>", "<strong>pm2.5: </strong>", idw.output$var1.pred)

pal <- colorNumeric(c("#9DFF9C","#31FF00","#31CF00","#FFFF00","#FFCF00","#FF9A00","#FE6464","#FE0000","#990100","#CE30FF"),c(0,12,24,36,42,48,54,59,65,71,100))
pal_2 <- colorBin(c("#9DFF9C","#31FF00","#31CF00","#FFFF00","#FFCF00","#FF9A00","#FE6464","#FE0000","#990100","#CE30FF"),bins=c(0,12,24,36,42,48,54,59,65,71,100))


#draw map
shinyServer(function(input,output){
  #invalidateLater(120000, session=NULL)
  #AQIdata <- reactiveFileReader(120000, session=NULL, "/home/lepg5487/R/730/air_2826.csv", read.csv)
  #count <- reactiveFileReader(120000, '/home/lepg5487/R/730/air_2826.csv', read.csv)
  
  
  observe({
    colorBy <- input$Forecast
    leafletProxy("map")%>% clearControls()%>%clearMarkers()%>%clearShapes()
    
    if (colorBy == "0hour"){  #當前時間
      #政府資料
      data_1<-gov_16()

      #點標籤
      gov16pop<-i_popup_gov()
      #點地圖
      mappop<-i_popup2()
      #shapefile
      gov16data<-getdata_gov()
      
      leafletProxy("map", data = gov16data) %>%setView(120.845134, 24.190975, zoom = 11) %>%
        
        addPolygons(fillColor = ~pal(gov16data$idw_value),fillOpacity = 0.7, color = "#000000", weight = 1,popup = mappop)%>%
        addMarkers(data = wind(),~lon, ~lat, icon=~leafIcons[wind()$WDIR_2],popup = popup_wind(),group ='風向')%>%
        addMarkers(data = data_1,~Slong, ~Slat, popup = gov16pop,group ='政府測站')%>%
        addLegend("bottomright",pal = pal,title ="pm2.5",values = ~gov16data$idw_value, opacity = 1)%>%
        addLayersControl(overlayGroups =  c('政府測站','風向'),options = layersControlOptions(collapsed=F))%>%
        addResetMapButton()%>%
        addSearchOSM(options = searchOptions(autoCollapse = TRUE))
      
    }else if (colorBy == "1hour"){
      time_1<-Sys.time()+3600
      time_1 <- substr(time_1, 1, 13)
      time_1 <- paste0(time_1,":00:00")
      #政府資料
      name_data<-paste0("/srv/shiny-server/sample-apps/gov/forecast_data/",time_1,".csv")
      #IDW count資料表
      name_idw<-paste0("/srv/shiny-server/sample-apps/gov/forecast_data/",time_1,"_idw.csv")
      data_1<-read.csv(name_data, fileEncoding="utf-8")
      data_idw<-read.csv(name_idw, fileEncoding="utf-8")
      shape_1 <- readOGR("/home/hpc/1x1_0830","1x1")
      shape_1 <- merge(shape_1,data_idw,by="Id")
      #點標籤
      gov16pop<-paste0("<strong>Date: </strong>", data_1$Time, "<br>" ,"<strong>NAME: </strong>", data_1$Sname, "<br>", "<strong>PM2.5: </strong>", data_1$pm2.5)
      #點地圖
      mappop<-paste0("<strong>ID: </strong>", shape_1$Id, "<br>", "<strong>pm2.5: </strong>", shape_1$idw_value)
      #shapefile
      gov16data_1<-shape_1
      gov16data<-getdata_gov()
      leafletProxy("map", data = gov16data) %>%setView(120.845134, 24.190975, zoom = 11) %>%
        addPolygons(fillColor = ~pal(gov16data_1$idw_value),fillOpacity = 0.7, color = "#000000", weight = 1,popup = mappop)%>%
        addMarkers(data = data_1,~Slong, ~Slat, popup = gov16pop,group ='政府測站')%>%
        addLegend("bottomright",pal = pal,title ="pm2.5",values = ~gov16data_1$idw_value, opacity = 1)%>%
        addLayersControl(overlayGroups =  c('政府測站'),options = layersControlOptions(collapsed=F))%>%
        addResetMapButton()%>%
        addSearchOSM(options = searchOptions(autoCollapse = TRUE))
      
    }else if (colorBy == "2hour"){
      time_1<-Sys.time()+7200
      time_1 <- substr(time_1, 1, 13)
      time_1 <- paste0(time_1,":00:00")
      name_data<-paste0("/srv/shiny-server/sample-apps/gov/forecast_data/",time_1,".csv")
      name_idw<-paste0("/srv/shiny-server/sample-apps/gov/forecast_data/",time_1,"_idw.csv")
      data_1<-read.csv(name_data, fileEncoding="utf-8")
      data_idw<-read.csv(name_idw, fileEncoding="utf-8")
      shape_1 <- readOGR("/home/hpc/1x1_0830","1x1")
      shape_1 <- merge(shape_1,data_idw,by="Id")
      #點標籤
      gov16pop<-paste0("<strong>Date: </strong>", data_1$Time, "<br>" ,"<strong>NAME: </strong>", data_1$Sname, "<br>", "<strong>PM2.5: </strong>", data_1$pm2.5)
      #點地圖
      mappop<-paste0("<strong>ID: </strong>", shape_1$Id, "<br>", "<strong>pm2.5: </strong>", shape_1$idw_value)
      #shapefile
      gov16data<-shape_1
      leafletProxy("map", data = gov16data) %>%setView(120.845134, 24.190975, zoom = 11) %>%
        addPolygons(fillColor = ~pal(gov16data$idw_value),fillOpacity = 0.7, color = "#000000", weight = 1,popup = mappop)%>%
        addMarkers(data = data_1,~Slong, ~Slat, popup = gov16pop,group ='政府測站')%>%
        addLegend("bottomright",pal = pal,title ="pm2.5",values = ~gov16data$idw_value, opacity = 1)%>%
        addLayersControl(overlayGroups =  c('政府測站'),options = layersControlOptions(collapsed=F))%>%
        addResetMapButton()%>%
        addSearchOSM(options = searchOptions(autoCollapse = TRUE))
      
    }else if (colorBy == "4hour"){
      time_1<-Sys.time()+14400
      time_1 <- substr(time_1, 1, 13)
      time_1 <- paste0(time_1,":00:00")
      name_data<-paste0("/srv/shiny-server/sample-apps/gov/forecast_data/",time_1,".csv")
      name_idw<-paste0("/srv/shiny-server/sample-apps/gov/forecast_data/",time_1,"_idw.csv")
      data_1<-read.csv(name_data, fileEncoding="utf-8")
      data_idw<-read.csv(name_idw, fileEncoding="utf-8")
      shape_1 <- readOGR("/home/hpc/1x1_0830","1x1")
      shape_1 <- merge(shape_1,data_idw,by="Id")
      #點標籤
      gov16pop<-paste0("<strong>Date: </strong>", data_1$Time, "<br>" ,"<strong>NAME: </strong>", data_1$Sname, "<br>", "<strong>PM2.5: </strong>", data_1$pm2.5)
      #點地圖
      mappop<-paste0("<strong>ID: </strong>", shape_1$Id, "<br>", "<strong>pm2.5: </strong>", shape_1$idw_value)
      #shapefile
      gov16data<-shape_1
      leafletProxy("map", data = gov16data) %>% setView(120.845134, 24.190975, zoom = 11) %>%
        addPolygons(fillColor = ~pal(gov16data$idw_value),fillOpacity = 0.7, color = "#000000", weight = 1,popup = mappop)%>%
        addMarkers(data = data_1,~Slong, ~Slat, popup = gov16pop,group ='政府測站')%>%
        addLegend("bottomright",pal = pal,title ="pm2.5",values = ~gov16data$idw_value, opacity = 1)%>%
        addLayersControl(overlayGroups =  c('政府測站'),options = layersControlOptions(collapsed=F))%>%
        addResetMapButton()%>%
        addSearchOSM(options = searchOptions(autoCollapse = TRUE))
      
    }else if (colorBy == "7hour"){
      time_1<-Sys.time()+25200
      time_1 <- substr(time_1, 1, 13)
      time_1 <- paste0(time_1,":00:00")
      name_data<-paste0("/srv/shiny-server/sample-apps/gov/forecast_data/",time_1,".csv")
      name_idw<-paste0("/srv/shiny-server/sample-apps/gov/forecast_data/",time_1,"_idw.csv")
      data_1<-read.csv(name_data, fileEncoding="utf-8")
      data_idw<-read.csv(name_idw, fileEncoding="utf-8")
      shape_1 <- readOGR("/home/hpc/1x1_0830","1x1")
      shape_1 <- merge(shape_1,data_idw,by="Id")
      #點標籤
      gov16pop<-paste0("<strong>Date: </strong>", data_1$Time, "<br>" ,"<strong>NAME: </strong>", data_1$Sname, "<br>", "<strong>PM2.5: </strong>", data_1$pm2.5)
      #點地圖
      mappop<-paste0("<strong>ID: </strong>", shape_1$Id, "<br>", "<strong>pm2.5: </strong>", shape_1$idw_value)
      #shapefile
      gov16data<-shape_1
      leafletProxy("map", data = gov16data) %>% setView(120.845134, 24.190975, zoom = 11) %>%
        addPolygons(fillColor = ~pal(gov16data$idw_value),fillOpacity = 0.7, color = "#000000", weight = 1,popup = mappop)%>%
        addMarkers(data = data_1,~Slong, ~Slat, popup = gov16pop,group ='政府測站')%>%
        addLegend("bottomright",pal = pal,title ="pm2.5",values = ~gov16data$idw_value, opacity = 1)%>%
        addLayersControl(overlayGroups =  c('政府測站'),options = layersControlOptions(collapsed=F))%>%
        addResetMapButton()%>%
        addSearchOSM(options = searchOptions(autoCollapse = TRUE))
    }
    
  })
  
  
  
  #政府1
  output$map <- renderLeaflet({
    leaflet() %>% addTiles() %>% setView(120.845134, 24.190975, zoom = 11) %>% 
      #addPolygons(fillColor = ~pal(getdata_gov()$idw_value),fillOpacity = 0.7, color = "#000000", weight = 1,popup = i_popup2())%>% 
      # addCircleMarkers(lng = idw.output$long
      #                  ,lat = idw.output$lat
      #                  ,popup = i_popup_idw
      #                  ,col = "blue"
      #                  ,fillOpacity = (idw.output$var1.pred/50)
      #                  ,stroke = FALSE
      #                  ,group = "CircleMarkers")%>%
      #addLegend("bottomright",pal = pal,title ="pm2.5",values = ~getdata_gov()$idw_value, opacity = 1)%>%
      addResetMapButton()%>%
      addSearchOSM(options = searchOptions(autoCollapse = TRUE))#%>%
      #addMarkers(data=gov_16(),~Slong,~Slat, popup = i_popup_gov(),group ='政府測站')%>%
      #addMarkers(data = wind(),~lon, ~lat, icon=~leafIcons[wind()$WDIR_2],popup = popup_wind(),group ='風向')%>% 
      #addLayersControl(overlayGroups =  c('政府測站','風向'),options = layersControlOptions(collapsed=F))
      #addSearchGoogle(apikey = Sys.getenv("AIzaSyBT5Koy0A7Ysc3acwDVWddUFT0p56nHRxg"))%>%
      #addControlGPS(options=gpsOptions(position = "topleft", activate = FALSE, autoCenter = FALSE,maxZoom = NULL, setView = TRUE))
    #addMarkers(data=gov_16, lng=~Slong, lat=~Slat, popup=i_popup_gov)%>%
    #addMarkers(~airbox_in_tc$lon, ~airbox_in_tc$lat, popup = i_popup1)
  })
  
  
  
  
  
  
  #airbox 1
  output$map_3 <- renderLeaflet({
    leaflet(getdata_airbox()) %>% addTiles() %>% setView(120.845134, 24.190975, zoom = 11) %>% 
      addPolygons(fillColor = ~pal(getdata_airbox()$idw_value),fillOpacity = 0.7, color = "#000000", weight = 1,popup = i_popup3())%>% 
      # addCircleMarkers(lng = idw.output$long
      #                  ,lat = idw.output$lat
      #                  ,popup = i_popup_idw
      #                  ,col = "blue"
      #                  ,fillOpacity = (idw.output$var1.pred/50)
      #                  ,stroke = FALSE
      #                  ,group = "CircleMarkers")%>%
      addLegend("bottomright",pal = pal,title ="pm2.5",values = ~getdata_airbox()$idw_value, opacity = 1)%>%
      addResetMapButton()%>%
      addSearchOSM(options = searchOptions(autoCollapse = TRUE))%>%
      addMarkers(data=airbox_data(),~lon, ~lat, popup = i_popup_airbox(),group ='airbox')%>%
      addMarkers(data = wind(),~lon, ~lat, icon=~leafIcons[wind()$WDIR_2],popup = popup_wind(),group ='風向')%>% 
      addLayersControl(overlayGroups =  c('airbox','風向'),options = layersControlOptions(collapsed=F))
   
  })
  
  #airbox 2
  output$map_4 <- renderLeaflet({
    leaflet(getdata_airbox()) %>% addTiles() %>% setView(120.845134, 24.190975, zoom = 11) %>% 
      addPolygons(fillColor = ~pal_2(getdata_airbox()$idw_value),fillOpacity = 0.7, color = "#000000", weight = 1,popup = i_popup3())%>% 
      # addCircleMarkers(lng = idw.output$long
      #                  ,lat = idw.output$lat
      #                  ,popup = i_popup_idw
      #                  ,col = "blue"
      #                  ,fillOpacity = (idw.output$var1.pred/50)
      #                  ,stroke = FALSE
      #                  ,group = "CircleMarkers")%>%
      addLegend("bottomright",pal = pal_2,title ="pm2.5",values = ~getdata_airbox()$idw_value, opacity = 1)%>%
      addResetMapButton()%>%
      addSearchOSM(options = searchOptions(autoCollapse = TRUE))%>%
      addMarkers(data=airbox_data(),~lon, ~lat, popup = i_popup_airbox(),group ='airbox')%>%
      addMarkers(data = wind(),~lon, ~lat, icon=~leafIcons[wind()$WDIR_2],popup = popup_wind(),group ='風向')%>% 
      addLayersControl(overlayGroups =  c('airbox','風向'),options = layersControlOptions(collapsed=F))
    
  })
  
  
  #mix 1 
  
  output$map_mix_1 <- renderLeaflet({
    leaflet(getdata_mix()) %>% addTiles() %>% setView(120.845134, 24.190975, zoom = 11) %>% 
      addPolygons(fillColor = ~pal(getdata_mix()$idw_value),fillOpacity = 0.7, color = "#000000", weight = 1,popup = i_popup2_mix())%>% 
      addLegend("bottomright",pal = pal,title ="pm2.5",values = ~getdata_mix()$idw_value, opacity = 1)%>%
      addResetMapButton()%>%
      addSearchOSM(options = searchOptions(autoCollapse = TRUE))%>%
      addMarkers(data=gov_16(),~Slong,~Slat, popup = i_popup_gov(),group ='政府測站')%>%
      addMarkers(data = wind(),~lon, ~lat, icon=~leafIcons[wind()$WDIR_2],popup = popup_wind(),group ='風向')%>% 
      addLayersControl(overlayGroups =  c('政府測站','風向'),options = layersControlOptions(collapsed=F))
   
  })
  
  
  #mix 2
  
  output$map_mix_2 <- renderLeaflet({
    leaflet(getdata_mix()) %>% addTiles() %>% setView(120.845134, 24.190975, zoom = 11) %>% 
      addPolygons(fillColor = ~pal(getdata_mix()$idw_value),fillOpacity = 0.7, color = "#000000", weight = 1,popup = i_popup2_mix())%>% 
      addLegend("bottomright",pal = pal_2,title ="pm2.5",values = ~getdata_mix()$idw_value, opacity = 1)%>%
      addResetMapButton()%>%
      addSearchOSM(options = searchOptions(autoCollapse = TRUE))%>%
      addMarkers(data=gov_16(),~Slong,~Slat, popup = i_popup_gov(),group ='政府測站')%>%
      addMarkers(data = wind(),~lon, ~lat, icon=~leafIcons[wind()$WDIR_2],popup = popup_wind(),group ='風向')%>% 
      addLayersControl(overlayGroups =  c('政府測站','風向'),options = layersControlOptions(collapsed=F))
    
  })
  
  
  
})
