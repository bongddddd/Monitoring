library(sf)
library(readxl)
library(tidyverse)
library(terra)
library(png)
library(sp)



# 1 shp 파일 불러오기기

shpp <- "C:/Users/User/Downloads/T_CLFM_FRM_MAP_경남11/T_CLFM_FRM_MAP_48_202306.shp"
rudska <- st_read(shpp)  # sf 파일로 불러옴(multipolygon)

mjd <- read_excel("C:/Users/User/Downloads/다운로드_20231025092954.xlsx") # 팜맵에서 다운받은 바운더리 정보보

rn_Mjd <- rudska %>%     # ID를 기준으로 같은 값을 가져옴
  filter(ID %in% mjd$ID) # %>% filter(FL_CD == "01") / 2022년 자료라  논 타입이 안맞길레 걍 가져옴옴

mjd_w <- st_transform(mjd, crs = st_crs("EPSG:4326")) # crs를 WSG84로 변경해줌

st_write(mjd_w,"C:/Users/User/OneDrive/종합/Drone/Data/farmpolygon/mjd.shp") # 변경되 shp를 외부로 보내서 저장함 


# 거창

rjckd <- read_excel("C:/Users/User/Downloads/다운로드_20230901151911.xlsx")
class(rjckd$ID[1])

rn_gch <- rudska %>%     # ID를 기준으로 같은 값을 가져옴
    filter(ID %in% rjckd$ID) %>% filter(FL_CD == "01")

gch <- st_geometry(rn_gch)

gch_w <- st_transform(gch, crs = st_crs("EPSG:4326"))

st_write(gch_w,"C:/Users/User/OneDrive/종합/Drone/Data/farmpolygon/gch.shp")
plot(gch_w)


# 거창 조

rjckd_jo <- read_excel("C:/Users/User/Downloads/다운로드_20231027134234.xlsx")

gch_jo <- rudska %>%     # ID를 기준으로 같은 값을 가져옴
  filter(ID %in% rjckd_jo$ID)

gch_jo <- st_geometry(gch_jo)

gch_jo_w <- st_transform(gch_jo, crs = st_crs("EPSG:4326"))

st_write(gch_jo_w,"C:/Users/User/OneDrive/종합/Drone/Data/farmpolygon/gch_jo.shp")








mjd <- read_excel("C:/Users/User/Downloads/다운로드_20231025092954.xlsx")
shpp <- "C:/Users/User/Downloads/T_CLFM_FRM_MAP_경남11/T_CLFM_FRM_MAP_48_202306.shp"
mjd_Png <- "E:/Uiry/Pix4Dfields/Data/6b6f05a4-f838-40b2-8e36-0e7e1ad30d3a_snapshot.png"

rudska <- st_read(shpp)
names(rudska)
names(mjd)

# rn_Mjd <- rudska %>% inner_join(mjd, by = "ID" )  # mjd과 id, fd_cd가 일치하는 값만 선별별

rn_Mjd <- rudska %>%
  filter(ID %in% mjd$ID) # %>% filter(FL_CD == "01")

# coord.ref없음 ㅇㅇㅇ
plot(st_geometry(rn_Mjd))
plot(rn_Mjd["FL_AR"])


nrow(rn_Mjd)
mjd_png <- readPNG(mjd_Png) # PNG는 레이어의 정보를 단일한 행렬로 처리함
mjd_ras <- rast(mjd_Png)
dim(mjd_png)
str(mjd_png)
plot(mjd_png)
plotRGB(mjd_ras)

# 위성 맵, 그리고 mjd의 팜맵 정보도 가져옴, 이제 좌표 정보만 입력력하면 됨 

rn_Mjd
mjd_ras


co_mjd <- st_coordinates(rn_Mjd)
nrow(co_mjd)


# Korean 1985 / Central Belt 좌표 시스템의 예시 좌표 (경도와 위도)
korean_coords <- data.frame(x = co_mjd[,1], y = co_mjd[,2])

# 좌표 변환: Korean 1985 / Central Belt에서 WGS 84로
korean_crs <- CRS("+proj=tmerc +lat_0=38 +lon_0=127.5 +k=0.9996 +x_0=1000000 +y_0=2000000 +datum=WGS84 +units=m +no_defs")
wgs84_crs <- CRS("+proj=longlat +datum=WGS84 +no_defs")
korean_coords_sp <- SpatialPoints(korean_coords, proj4string = korean_crs)
wgs84_coords_sp <- spTransform(korean_coords_sp, wgs84_crs)

# 변환된 좌표 확인
print(wgs84_coords_sp@coords)
wgs84_coords_sp@proj4string


co_mjd[,1]

# rn_mjd에 변환된 좌표 추가
rn_Mjd$WGS84_Longitude <- wgs84_coords_sp@coords[, 1]
rn_Mjd$WGS84_Latitude <- wgs84_coords_sp@coords[, 2]
rn_Mjd$
# 결과 확인
head(rn_mjd)  # 처음 몇 행 확인

mjd <- st_geometry(rn_Mjd)
plot(mjd)
names(mjd)
mjd[1]

st_coordinates(rn_Mjd)

co_mjd
mjd_w <- st_transform(mjd, crs = st_crs("EPSG:4326"))  # coords 변경 
mjd_w
mjd_ras
aaa <- crop(mjd_ras,mjd_w)
aaa

mjd_ras
st_write(mjd_w,"C:/Users/User/OneDrive/종합/Drone/Data/farmpolygon/mjd.shp")

