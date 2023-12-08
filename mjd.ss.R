library(sf)             
library(terra)
library(tidyverse)
library(raster)
# 목표 - 논문 구현
# 파일 경로를 저장하는 벡터 생성
file_paths <- c("D:\\Uiry\\rice\\Unnamed project 1\\2023-06-16, Boundary.data.tif",
                "D:\\Uiry\\rice\\Unnamed project 1\\2023-06-23, Boundary.data.tif",
                "D:\\Uiry\\rice\\Unnamed project 1\\2023-07-31, Boundary.data.tif",
                "D:\\Uiry\\rice\\Unnamed project 1\\2023-08-02, Boundary.data.tif",
                "D:\\Uiry\\rice\\Unnamed project 1\\2023-08-07, Boundary.data.tif",
                "D:\\Uiry\\rice\\Unnamed project 1\\2023-08-15, Boundary.data.tif",
                "D:\\Uiry\\rice\\Unnamed project 1\\2023-09-09, Boundary.data.tif",
                "D:\\Uiry\\rice\\Unnamed project 1\\2023-09-11, Boundary.data.tif",
                "D:\\Uiry\\rice\\Unnamed project 1\\2023-09-19, Boundary.data.tif",
                "D:\\Uiry\\rice\\Unnamed project 1\\2023-09-24, Boundary.data.tif",
                "D:\\Uiry\\rice\\Unnamed project 1\\2023-09-29, Boundary.data.tif",
                "D:\\Uiry\\rice\\Unnamed project 1\\2023-10-01, Boundary.data.tif"
                
)
ttt <- c("06-16","06-23","07-31","08-02","08-07","08-15",'09-09',"09-11","09-19","09-24","09-29","10-01")
# ""06-16","06-23","07-31","08-02","08-07","08-15",'09-09',"09-11","09-19","09-24","09-29","10-01""
# 벡터를 사용하여 라스터 객체 생성
gch <- lapply(file_paths, rast)
gch_s <- list()
# 돌려서 고정 시켜주기기
for (i in 1:12){
  gch_1 <- rectify(gch[[i]])
  #필요없는 layer제거 red 123,NarrowNIR, , alpha 
  gch_s[[i]] <-gch_1[[-c(4,5,6,8,11)]]
  # 이름 너무 길어서 좀 줄임
  names(gch_s[[i]]) <- c("B", "G", "R", "N", "S1", "S2")  }
#필요없는 layer제거 확인
plot(gch_s[[1]])


# ( Qiu et al., 2015 ) 에서는 토지 피복 방식 사용, 근데 한국 피복은 2021년이 최신이라 
# 해당 방식은 2022년 기준 팜맵의 논의 바운더리를 가져오는 방식임
#bb 불러오기
gch_shp <- st_read("C:/Users/User/OneDrive/종합/Drone/Data/farmpolygon/mjd.shp")
# bb의 crs정렬
gch_shp_c <- st_transform(gch_shp, crs = "+init=epsg:32652")
# 셀 값 추출을 위해 sf형식인 mjd_shp를 spotVector로 변경해줌
gch_shp_c_s <- vect(gch_shp_c)
# 1~3번 폴리곤 필요없음 제거거
gch_shp_c_s <- gch_shp_c_s[c(17,25,24,14,23,13),]
plot(gch_shp_c_s)
# bb와 일치하는곳만 
gch_sb <-list()
gch_sbb <-list()
# 13개의 layer에서 crop 해줌
for (i in 1:12) {
  gch_sb[[i]] <- mask(gch_s[[i]], gch_shp_c_s)
  gch_sbb[[i]] <- crop(gch_sb[[i]], gch_shp_c_s)}
plot(gch_sbb[[1]])

# evi - wi2015 / evi2 - lwsi 
# 논의 평균값을 기준으로 ㄱ 

# 단위로 벼필지 파악 or 픽셀 단위로?

# 이후 논, 그외 셀로 구분 함
# 1 LSWI 모내기 기간, 1번 파일 사용 , R, N, SW1 만 사용용http://127.0.0.1:29855/graphics/plot_zoom_png?width=1200&height=900
rns <- gch_sbb[[1]][[-c(1,2,6)]]
rns

# na값이 제외된 속성값이 셀 위치정보와 같이 저장됨 
df <- as.data.frame(rns)

# 읽어보기 ㅇㅇ 이해 ㄱ 162행에 적어둠, 열 > 1행으로 변환해서 한번에 봄

de <- data.frame()
#LSWI 기반 방법에 대한 자세한 내용은 Xiao et al. (2005) . 
# 너무 뒤의 날짜 6/23로 잡은듯 6월 중순 ㄱㄱ
# 총 rast의 경우 총 셀이 79000개 정도 되는데 그중 3만개 정도는 빈 셀이고 df 시키면 없어짐
# 근데 df는 총 갯수를 행 *열로 잡아서 대충 11만개 정도 나옴옴
for(i in 1:(ncell(df)/3)) {             # df로 변경 시키면서 3개의 열이 하나로 결합됨됨  
  de[i,1] <- (df[i,2]-df[i,1])/(df[i,2]+df[i,1])
  de[i,2] <-(df[i,2]-df[i,3])/(df[i,2]+df[i,3])
  de[i,3] <- ifelse(de[i,2] +0.17  > de[i,1], 1, 0)
} # 1픽셀 = 논, 0 픽셀 = 그외
# df는 위치정보를 가지고 있고 여기 순서에 맞춰서 걍 de 삽입, 
df_gch <- cbind(df,t = de$V3)
head(df_gch)
# 해당 속성정보를 raster에 삽입후 보기, 이걸 spatraster에 속성정보로 잘만 넣으면 됨
# 0인 부분 속성값 다 날리면됨됨  - 난중에 ㄱ ㅋㅋ,,,
# 이거 인덱스값 살릴려고 이런 방식으로 함
# 핼 정보 유지 / 행정보 = 공간정보임 ㅇㅇㅇ
new_df_gch <- rownames_to_column(df_gch, var = "i")

head(new_df_gch)
# gch_i <- new_df_gch[, c("i", "t")]
# t가 1인 행을 필터링
ii <- which(new_df_gch$t == 1) # which - 조건 충족하는 인덱스 번호 찾아서 저장 

gch_t <- new_df_gch[ii,] # LSWI가 NDVI보다 작은값 삭제 6/8일 기준준

as.numeric(gch_t$i)
# rns 셀 순서가 gch_t$i랑 일치하는 셀의 값만 유지, 나머지는 NA  # 난중에 ㄱㄱ
# 원래의 rns 데이터셋의 값만을 추출
rns_values <- values(rns) # 행렬형태태
rns_values[-as.numeric(gch_t$i)] <- NA  # V1이 0인 셀을 NA로 설정
# NA를 제거하고 새로운 SpatRaster 생성
rns_values
rns_new <- rns
values(rns_new) <- rns_values
plot(rns_new$R)
rns_new$R
### RGB가져와서 비교

grgb <- rast("D:\\Uiry\\rice\\Unnamed project 1\\2023-06-16, Boundary.rgb.tif") 

grgb <- rectify(grgb)
plotRGB(grgb)
grgb
gm <- rns_new$R
grgb <- mask(grgb, gch_shp_c_s)
grgb <- crop(grgb, gch_shp_c_s)
plot(grgb)
grgb1 <- mask(grgb, rns_new$R)
# 확인
ext(gm)
plotRGB(grgb1)
# lswi + T > ndvi를 통해 나온 값 적용
gm <- rns_new$R
gch_d <- list()
for (i in 1:12) {
  gch_d[[i]] <- mask(gch_sbb[[i]],gm)
}
for (i in 1:13) {   # 에러나서 확인 # 마지막에 문제, 근데 저때 수확 끝났을때라 걍 제외함
  print(ext(gch_sbb[[i]]))
}

# 494개의 shp내의 위치정보
gli <- list()
glist <- list()
# 총 74018개의 픽셀로 구성됨됨(gch기준, mjd은 몇개없음)
# 각 구역들의 밴드값 추출 / 도형 유지 필요 없음 ㅇㅇㅇ
for (j in 1:length(gch_d)) {
  for (i in 1:length(gch_shp_c_s)) {
    me <- mask(gch_sbb[[j]],gch_shp_c_s[i,])
    gli[[i]] <- rownames_to_column(as.data.frame(me),var = "i")  # df로 변환 시키면 순서값 달아주는데 그거 열로 만듬듬(공간정보임임)
  } 
  glist[[j]] <- gli
}
# 결과
glist 
# 원하는 값 계산

g <- list()
res <- list()
for (i in 1:length(gch_sbb)) {
  for(j in 1:length(gch_shp_c_s)){
    ndvi = (glist[[i]][[j]]$N - glist[[i]][[j]]$R)/(glist[[i]][[j]]$N + glist[[i]][[j]]$R)
    lwsi = (glist[[i]][[j]]$N- glist[[i]][[j]]$S1)/(glist[[i]][[j]]$N + glist[[i]][[j]]$S1)
    wi2015 = 1.7204 + 171*glist[[i]][[j]]$B + 3*glist[[i]][[j]]$G - 70*glist[[i]][[j]]$R - 45*glist[[i]][[j]]$N - 71*glist[[i]][[j]]$S2
    evi = 2.5*(glist[[i]][[j]]$N - glist[[i]][[j]]$R)/(glist[[i]][[j]]$N + 6*glist[[i]][[j]]$R - 7.5*glist[[i]][[j]]$B+ 1)
    evi2 = 2.5*(glist[[i]][[j]]$N - glist[[i]][[j]]$R)/(glist[[i]][[j]]$N + 2.4*glist[[i]][[j]]$R+1)
    msi = (glist[[i]][[j]]$S1/glist[[i]][[j]]$N)
    savi = (glist[[i]][[j]]$N - glist[[i]][[j]]$R)/(glist[[i]][[j]]$N + glist[[i]][[j]]$R + 0.5)*1.5
    res[[j]] = cbind("ndvi" = mean(ndvi),"lswi" = mean(lwsi), "wi2015" =mean(wi2015),"evi"= mean(evi),"msi" = mean(msi),"evi2" = mean(evi2), "savi" = mean(savi))
  } # 필지의 평균값값
  g[[i]] <- res
}
g
seq_along(g)
me <- list()
# 리스트 내의 모든 행렬을 행방향으로 합치기
for (i in seq_along(g)) {
  me[[i]] <- do.call(rbind, g[[i]])}
#df로 변환환
df_gch <- lapply(me, as.data.frame)
# 시간 열 추가
df_list <- lapply(seq_along(df_gch), function(i) {
  df_gch[[i]]$time<- rep(i, nrow(df_gch[[i]]))
  return(df_gch[[i]])})
# df로 변환환
df_list <- bind_rows(df_list) # 걍 행들 다 묶어줌
df_list

ggplot(df_list) +
  geom_boxplot(aes(x = as.factor(time), y = wi2015)) +
  labs(x = "time", y = "wi2015") +
  theme_minimal()

mean_values <- aggregate(. ~ time, data = df_list, mean) # df에서 평균으로 통합

# 시간에 따른 선형 그래프 LSWI + T > NDVI용
mean_values <- df_list %>%
  group_by(time) %>%
  summarize(ndvi = mean(ndvi,na.rm = TRUE),
            lswi = mean(lswi, na.rm = TRUE) + 0.17)
mean_values

# 시간에 따른 선형 그래프
ggplot(mean_values, aes(x = time)) +
  geom_line(aes(y = evi2, color = "evi2"), linetype = "solid", size = 1) +
  geom_line(aes(y = lswi, color = "LSWI"), linetype = "solid", size = 1) +
#  geom_line(aes(y = wi2015, color = "WI2015"), linetype = "solid", size = 1) +
  # 다른 변수들에 대해서도 위와 같이 추가할 수 있습니다.
  labs(title = "EVI2,LSWI", x = "time", y = "평균값") +
  scale_color_manual(values = c("evi2" = "green", "LSWI" = "blue")) +  # 선 색상 지정
  theme_minimal() +
 theme(legend.position = "right")  # 범례 위치 조정



       