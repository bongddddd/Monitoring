library(sf)             
library(terra)
library(ggmap)
library(tidyverse)
library(FNN)
library(cluster)
library(openxlsx)


# 필지 파일 불러오기
ccord <- read.xlsx("C:/Users/User/Desktop/캡스톤/Uiry/Uiry_cluster.xlsx")   # it brings coordinate
cor <- ccord[,c(1,5,6)] # 필요 없는값 제거거

# 구글 맵으로 확인
register_google(key = "your key") # 키입력
cen <- c(mean(cor[,2]),mean(cor[,3])) # 중심 좌표
cen
map <- get_googlemap(center = cen,           # 구글 로드맵 정보 저장장
                     maptype = "satellite",  # "terrain", "satellite", "roadmap", "hybrid"
                     zoom = 14,
                     markers = cor) 
gmap <- ggmap(map)
gmap +
  geom_text(data = cor,     # 확인인
            aes(x = lon, y = lat),
            size = 3, label = cor$NO)



# 위치를 기준으로 가까운 5개 그룹 만들기기
# 시드를 고정
set.seed(42)
k <- 5
km <- kmeans(cor[,c("lon","lat")], centers = k) # Use "lon", "lat" to perform clustering

# 5개로 분리된 cluster번호를 넣어줌줌
uiry <- cor
#cluster_data <- as.data.frame(ccord[,c("lon","lat")]) # Create a variable with lon, latur 
uiry$cl <- as.factor(km$cluster) # add cluster
uiry

# 결과 확인 
gmap +  
  geom_point(data = uiry, aes(x = lon, y = lat, color = cl)) +
  coord_equal() + theme_minimal() +
  geom_text(data = uiry, aes(x = lon, y = lat, label = NO,hjust = 0.5, vjust = -1))  # make plot 



# 엑셀값으로 저장장
wb <- createWorkbook() # create sheet

uiry <- order(ccord$cluster) # Arrange the cluster to match the correct numbers
uiry1 <- ccord[uiry,]  # Use "Uiri" to properly align "ccord"

uiry_scale <- split(uiry1, uiry1$cluster)  #  divide "uiry1" by the same number
uiry_scale


for (cluster_name in names(uiry_scale)) {    # "names" returns string vector
  df <- uiry_scale[[cluster_name]]    # save date split-ed by cluster number
  addWorksheet(wb, sheetName = cluster_name) # add work sheet 
  writeData(wb, sheet = cluster_name, x = df)  
}   

# ?????? ?????? ??????
saveWorkbook(wb, "Uiry.xlsx",overwrite = TRUE)

##########################################################################################
# 각각의 cluster에 대한 접근


# 점들이 흩어져있는 3번과 5번의 일부분은 촬영하지 않음 
# cluster 기준으로 재 정렬 하고 지도에 표시

uiry <- arrange(uiry,cl) # 정렬

me1 <- c(mean(uiry[uiry$cl==1,2]),mean(uiry[uiry$cl==1,3]))
me1
map1 <- get_googlemap(center = me1,           # 구글 로드맵 정보 저장장
                      maptype = "satellite",  # "terrain", "satellite", "roadmap", "hybrid"
                      zoom = 15,
                      markers = uiry[,c(2,3)]) 

# 해당 좌표를 포함하는 도형 생성
ppp <- st_multipoint((cbind(uiry[uiry$cl==1,"lon"], uiry[uiry$cl==1,"lat"])))
class(ppp)
# 최외곽을 연결하여 폴리곤을 만듭니다
up <- st_convex_hull(ppp)  # 최 외각점들
up1 <- st_coordinates(up)  # 죄표값 지정  # 형태가 행렬로 변경 됨
updf <- as.data.frame(up1) # ggplot에 사용할 수 있도록 data.frame로 변경

gmap1 <- ggmap(map1)
# 아까의 반복복
gmap1 +  
  geom_point(data = uiry, aes(x = lon, y = lat)) +
  coord_equal() + theme_minimal() +
  geom_text(data = uiry, aes(x = lon, y = lat, label = NO,hjust = 0.5, vjust = -1)) +
  geom_path(data = updf, aes(x= X, y = Y), color = "red")



# 예제 multipoint 데이터 생성
class(up)
up
plot(up1)
# multipoint를 polygon으로 변환
up_sf <- st_sfc(st_polygon(list(as.matrix(up))))
up_sf <- st_set_crs(up_sf, "+proj=longlat +datum=WGS84")
plot(up_sf)

st_write(up_sf, "C:/Users/User/OneDrive/종합/Drone/Data/farmpolygon/dh.shp")
















