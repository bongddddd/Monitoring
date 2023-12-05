# raster 패키지 불러오기
library(raster)
library(sf)          
library(sp)
library(terra)
library(tidyverse)
install.packages("smoothr")
library(smoothr)

# 예제 raster 데이터 생성
mjd <- rast("D:/Uiry/rice/Uiry-MJD - Boundary.data.tif") # 7/10
mjd2 <- rast("D:/Uiry/rice/MJD_2 - Boundary.data.tif") # 8/3
mjd3 <- rast("D:/Uiry/rice/MJD3 - Boundary.data.tif") # 10/12
rgbd <- rast("D:/Uiry/rice/MJD_2 - Boundary.rgb.tif")
plotRGB(rgbd)
rgbd <- mask(rgbd,mjd_shp_c_s[2,])
rgbd <- crop(rgbd, mjd_shp_c_s[2,])
plotRGB(rgbd)
# 논/ 배경 / 그외를 분리하기 위한 nir
plot(mjd)
# shp 파일일
mjd_shp <- st_read("C:/Users/User/OneDrive/종합/Drone/Data/farmpolygon/mjd.shp")
# bb의 crs정렬
mjd_shp_c <- st_transform(mjd_shp, crs = "+init=epsg:32652")
# 셀 값 추출을 위해 sf형식인 mjd_shp를 spotVector로 변경해줌
mjd_shp_c_s <- vect(mjd_shp_c)
# 1~3번 폴리곤 필요없음 제거거
mjd_shp_c_s <- mjd_shp_c_s[c(17,25,24,14,23,13),] # c(17,25,24,14,23,13)
plot(mjd_shp_c_s)
# alpha층 제거
mjd <- mjd[[-6]]
plot(mjd)# 도로 논 포함 층
# 필요한 구역만 잘라내기
mjd <- mask(mjd,mjd_shp_c_s)
mjd <- crop(mjd, mjd_shp_c_s)

# Exg를 사용해 벼, 배경 분리
#exg <- 2*mjd$Green - mjd$Blue - mjd$Red
#plot(exg) # 반사율 문제 때문에 정규화된 지수로 변경하여 사용함 # 반사율만 사용해서 TS- DT 사용시 잘 되서 걍 사용함
# C.v와 mean값 사용해서 이물질 제거 
# 위성값이랑 반사율을 비교했을때 드론의 반사율이 매우 약한데 날씨의 영향 + 패널 등으로 보정을 안해서 그런듯
# Exg의 경우도 매우 낮은 값을 보여  NDVI를 사용함 # NIR 다시 사용함
# 6구역에 대한 논 추출출
m1 <- list()
m2 <- list()
for(i in 1:6){
  m1[[i]] <- mask(mjd, mjd_shp_c_s[i,])
  m2[[i]] <- crop(m1[[i]], mjd_shp_c_s[i,])
}
# 벼 탐지에 사용할 NIR만 추출
plot(m2[[2]])
mm <- m2[[1]][[5]]
plot(mm)

# 전체 평균 C.V값 계산 na제거
# EXG, NIR 사용 23.23 / 15.15 window
mn <- c(1:6)
c.v <- c(1:6)
# CV평균값 생성
for(i in 1:6) {
  ndf <- as.data.frame(m2[[i]][[5]], na.rm = TRUE) # [[5]] NIR 값만 계산
#  ndf # nir 반사율 평균이이  0.06765654이라는데 넘 낮은거 아닌가? 물때문인가? / 7/10일 촬영
  mn[i] <- mean(ndf[,1]) # df로 변환되면서 1열이 됨, [,1] 안하면 numeric 대신 df로 받음음
  c.v[i] <- sd(ndf[,1])/mn[i] }
c.v
mcv <- mean(c.v)
mcv# 사용할 cv값


# 일단 cv 값 임계점 설정 # 제일 구분 잘되는 값 찾기
c_f_pre <- function(x,cv) {
  sd_value <- sd(x)
  mean_value <- mean(x)
  re <- sd_value / mean_value
  
  if(is.na(re)){
    return(1)
  }  else if(re <= 0.3*cv){
    return(2)
  }  else{
    return(3)
  }
}

# 외각, 순수, 믹스 픽셀 구분 임계값 0.3C.V

# c_f <- function(x, cv) {
#   j <- x[(length(x) + 1) / 2] # 중간 픽셀의 값 저장
#   if (is.na(j)) {
#     return(4)
#   } else{
# #    j <- x[(length(x) + 1) / 2] # 중간 픽셀의 값 저장
#     sdv <- sd(x[!is.na(x)])  # na값은 제외하고 값을 구함
#     me <- mean(x[!is.na(x)])
#     re <- (sdv / me)
#     if (re <= 0.3 * cv) {  # pure class
#       if (j > 0.1) {
#         return(1)   # 높을 시 도로 or 물 or 잡초
#       } else {      # 벼의 전체적인 NIR 반사율이 낮아서 반사율이 높은 잡초가 제거됨, 왜 낮은지는 나중에 알아볼 것
#         return(2)   # 낮으면 벼벼
#       }
#     } else {    # mixed class
#       if (me > j) {
#         return(3)   # 평균값이 픽셀 값보다 보다 높으면 땅
#       } else {
#         return(2)   # 낮으면 벼
#       }
#     } }
#   
# }

c_f <- function(x, cv){
  j <- x[(length(x) + 1) / 2] # 중간 픽셀의 값 저장
  if (is.na(j)) {
    return(4)
  } else{
    #    j <- x[(length(x) + 1) / 2] # 중간 픽셀의 값 저장
    sdv <- sd(x[!is.na(x)])  # na값은 제외하고 값을 구함
    me <- mean(x[!is.na(x)])
    re <- (sdv / me)
    if (re > 0.3 * cv) {  # # mixed class
      if (me > j) {
        return(2)   # 높을 시 도로 or 물 or 잡초
      } else {      # 벼의 전체적인 NIR 반사율이 낮아서 반사율이 높은 잡초가 제거됨, 왜 낮은지는 나중에 알아볼 것
        return(3)   # 낮으면 벼벼 # 
      }
    } else {    # pure class
      if (j > 0.1) {
        return(1)   # 평균값이 픽셀 값보다 보다 높으면 땅
      } else {
        return(2)   # 낮으면 배경, 순수 쌀 픽셀이 없음
      }
    } }
  
}


re6 <- list()
re66 <- list()
for (i in 1:6) {
tryCatch(   # 에러를 잡기 위함 
  
  re6[[i]] <- focal(m2[[i]][[5]], w=matrix(1,15,15), fun=function(x) c_f(x, mcv)),  
  error = function(e) {
    print("Error occurred:")
    print(e)
  }) }

plot(re6[[2]]) 
plot(m2[[1]][[5]])

ncol(m2[[1]][[5]])
ncol()
ncol(re6[[1]])
# df 으로 변경 시키고 벼, 배경과 같은 위치를 가지는 픽셀값만 분류해 표시 
# B, G, R, RE, NIR ,NDVI 
# 벼부분의 값 / 배경 값  
###########################################################
# 표로 뽑아서 정리하고 Knn 하기
# re6의 경우 새로 만들어진 rast라서 id순서도 새로 생성됨 == 공간정보 상실 / 그래서 새로 달아줄거임 / 
# 4번값을 가지는 행들은 전부 삭제 == 그럼 순서도 같아지는거 아닌가? 걍 넣어도 되는거 아님?

# 반사율이 너무 낮게 나와서 정규화된 지수들만 사용함
# 분리 하는게 효과가 있는지?
# 어느 지수가 실측값, 위성값과 가장 연관있는지
# 지수는 4개만만

lili <- lapply(re6, as.data.frame)
names(lili) <- paste("df",1:6)
nrow(lili$`df 2`)
li_r <- list()
li_s <- list()

for (j in 1:6) {
  df <- lili[[j]][lili[[j]]!= 4,]
  #df2 <- lili$`df 2`[lili$`df 2` != 4 ,] # 4(na값 제거된 거)
  nrow(df)
  # df 가져오고 결합 # 공간 정보 더해주는거임임
  sss <- as.data.frame(m2[[j]])#
  nrow(sss)
  n_df <- cbind(sss,df)   # 3은 벼, 2는 배경경
  
  new_df<- rownames_to_column(n_df, var = "i")
  # gch_i <- new_df_gch[, c("i", "t")]
  # t가 1인 행을 필터링
  ii <- which(new_df$df == 1) # 1인값 제거거
  ir <- which(new_df$df == 3) # which - 조건 충족하는 인덱스 번호 찾아서 저장 
  is <- which(new_df$df == 2)
  
  nn <- new_df[ii,]
  mjd_r <- new_df[ir,]
  mjd_s <- new_df[is,] # LSWI가 NDVI보다 작은값 삭제 6/8일 기준준
  
  # rns 셀 순서가 gch_t$i랑 일치하는 셀의 값만 유지, 나머지는 NA  # 난중에 ㄱㄱ
  # 원래의 rns 데이터셋의 값만을 추출
  # rns_values <- as.data.frame(m2[[1]])# 행렬형태태
  rns_values_r <- values(m2[[j]])
  rns_values_s <- values(m2[[j]])
  # 여기서 행열이 유지가 안됨됨, 해결함 행값에 넣어주면 됨됨
  rns_values_r[-as.numeric(mjd_r$i),] <- NA # 벼 외의 셀에 NA
  rns_values_r[as.numeric(nn$i),] <- NA  # 기타등에 NA
  
  rns_values_s[-as.numeric(mjd_s$i),] <- NA 
  rns_values_s[as.numeric(nn$i),] <- NA 
  # NA를 제거하고 새로운 SpatRaster 생성
  rns_newr <- m2[[j]]
  values(rns_newr) <- rns_values_r
  li_r[[j]] <- rns_newr
  rns_news <- m2[[j]]
  values(rns_news) <- rns_values_s
  li_s[[j]] <- rns_news
}
plot(li_r[[2]])
plot(li_s[[2]])
plot(m2[[2]]$`Red edge`)


la <- list(m2[[2]],li_r[[2]],li_s[[2]]) # 1 전체, 2 벼,3 배경

ee <- list()
for (i in 1:3) {
ndvi <- (la[[i]]$NIR - la[[i]]$Red)/(la[[i]]$NIR + la[[i]]$Red)
evi <- 2.5*(la[[i]]$NIR - la[[i]]$Red)/(la[[i]]$NIR + 6*la[[i]]$Red - 7.5*la[[i]]$Blue + 1)
ndwi <-(la[[i]]$NIR - la[[i]]$Green)/(la[[i]]$NIR + la[[i]]$Green)
ndre <- (la[[i]]$NIR - la[[i]]$`Red edge`)/(la[[i]]$NIR + la[[i]]$`Red edge`)
ee[[i]] = cbind("ndvi" = ndvi,"ndwi" = ndwi, "ndre" = ndre ,"evi"= evi)
}
# 1,2,3 다 넣음 반볻문 쓰기 싫어서
zz <- c(ee[[2]][[1]],ee[[2]][[4]], ee[[2]][[3]],ee[[2]][[2]] )
names(zz) <- c("NDVI" ,"EVI", "NDRE", "NDWI")
names(zz)
plot(zz)

la <- list(as.data.frame(m2[[2]]),as.data.frame(li_r[[2]]),as.data.frame(li_s[[2]]))
edf <- list()
for (i in 1:3) {
  ndvi <- (la[[i]]$NIR - la[[i]]$Red)/(la[[i]]$NIR + la[[i]]$Red)
  evi <- 2.5*(la[[i]]$NIR - la[[i]]$Red)/(la[[i]]$NIR + 6*la[[i]]$Red - 7.5*la[[i]]$Blue + 1)
  ndwi <-(la[[i]]$NIR - la[[i]]$Green)/(la[[i]]$NIR + la[[i]]$Green)
  ndre <- (la[[i]]$NIR - la[[i]]$`Red edge`)/(la[[i]]$NIR + la[[i]]$`Red edge`)
  edf[[i]] = cbind("NDVI" = ndvi,"NDWI" = ndwi, "NDRE" = ndre ,"EVI"= evi)
}
# 전체, 벼, 땅땅  
ss <- as.data.frame(edf[[3]])


# 데이터 프레임을 "long" 형식으로 변환
ss_long <- tidyr::gather(ss, key = "Variable", value = "Value")

# 밀도 그래프 그리기
ggplot(ss_long, aes(x = Value, fill = Variable)) +
  geom_density(alpha = 0.5) +
  labs(title = "Soil",
       x = "Value",
       y = "Density") +
  theme_minimal()

1+1
# 연습작들



























# new_df_gch <- rownames_to_column(m2[[1]][[5]], var = "i")
lili <- lapply(re66, as.data.frame)
names(lili) <- paste("df",1:6)
nrow(lili$`df 1`)
df1 <- lili$`df 1`[lili$`df 1` != 4 ,] # 4(na값 제거된 거)
# df 가져오고 결합 # 공간 정보 더해주는거임임
sss <- as.data.frame(m2[[1]])
nrow(sss)
n_df1 <- cbind(sss,df1)   # 2은 벼, 3는 배경경
head(n_df6)
n_df1$df1
new_df<- rownames_to_column(n_df1, var = "i")

head(new_df)
# gch_i <- new_df_gch[, c("i", "t")]
# t가 1인 행을 필터링
ir <- which(new_df$df1 == 2) # which - 조건 충족하는 인덱스 번호 찾아서 저장 
is <- which(new_df$df1 == 3)

mjd_r <- new_df[ir,]
mjd_s <- new_df[is,] # LSWI가 NDVI보다 작은값 삭제 6/8일 기준준
mjd_r
mjd_s
as.numeric(mjd_r$i)
# rns 셀 순서가 gch_t$i랑 일치하는 셀의 값만 유지, 나머지는 NA  # 난중에 ㄱㄱ
# 원래의 rns 데이터셋의 값만을 추출
# rns_values <- as.data.frame(m2[[1]])# 행렬형태태
rns_values <- values(m2[[1]])
# 여기서 행열이 유지가 안됨됨, 해결함 행값에 넣어주면 됨됨
rns_values[-as.numeric(mjd_s$i),] <- NA  # V1이 0인 셀을 NA로 설정
rns_values
# NA를 제거하고 새로운 SpatRaster 생성
rns_values
rns_new <- m2[[1]]
values(rns_new) <- rns_values
values(rns_new)
plot(rns_new)  # 블루만 나오나? # goruf











# gch_i <- new_df_gch[, c("i", "t")]
# t가 1인 행을 필터링
is <- which(n_df1$df1 ==2) # which - 조건 충족하는 인덱스 번호 찾아서 저장 / 땅
ir <- which(n_df1$df1 ==3) # which - 조건 충족하는 인덱스 번호 찾아서 저장 / 벼벼
is
sdf <- n_df1[is,] # 
rdf <- n_df1[ir,]
sdf
rdf
is
ir
# rns 셀 순서가 gch_t$i랑 일치하는 셀의 값만 유지, 나머지는 NA  # 난중에 ㄱㄱ
# 원래의 rns 데이터셋의 값만을 추출
mm <- values(m2[[1]]) # 행렬형태태
mm
mm[- as.numeric(is)] <- NA # V1이 0인 셀을 NA로 설정
# NA를 제거하고 새로운 SpatRaster 생성
mm
rns_new <- m2[[1]]
values(rns_new) <- mm
plot(rns_new)


ii <- which(ss$NIR == 4) # which - 조건 충족하는 인덱스 번호 찾아서 저장 
ii
aa <- sss[ii,]
aa


