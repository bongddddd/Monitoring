# raster 패키지 불러오기
library(sf)          
library(terra)
library(tidyverse)
install.packages("smoothr")
library(smoothr)

# 예제 raster 데이터 생성
file_paths <- c("D:/Uiry/rice/Uiry-MJD - Boundary.data.tif",  # 7/10
                "D:/Uiry/rice/MJD_2 - Boundary.data.tif", # 8/3
                "D:/Uiry/rice/MJD3 - Boundary.data.tif") # 10/12
mjd <- lapply(file_paths, rast)
mjd
file_rgb <- c("D:/Uiry/rice/Uiry-MJD - Boundary.rgb.tif",
              "D:/Uiry/rice/MJD_2 - Boundary.rgb.tif")
rgb <- lapply(file_rgb, rast)

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
mjd <- list(mjd[[1]][[-6]],mjd[[2]][[-6]])
mjd
# 도로 논 포함 층
# 필요한 구역만 잘라내기


# Exg를 사용해 벼, 배경 분리
#exg <- 2*mjd$Green - mjd$Blue - mjd$Red
#plot(exg) # 반사율 문제 때문에 정규화된 지수로 변경하여 사용함 # 반사율만 사용해서 TS- DT 사용시 잘 되서 걍 사용함
# C.v와 mean값 사용해서 이물질 제거 
# 위성값이랑 반사율을 비교했을때 드론의 반사율이 매우 약한데 날씨의 영향 + 패널 등으로 보정을 안해서 그런듯
# Exg의 경우도 매우 낮은 값을 보여  NDVI를 사용함 # NIR 다시 사용함
# 6구역에 대한 논 추출출
length(mjd_shp_c_s)
# 7/10, 8/3일에 대한 6구역의 논 추출출
# list() 초기화 해줘야함함
m1 <-  vector("list", length = length(mjd))  # 길이 정해줘야 되는듯
m2 <-  vector("list", length = length(mjd))
for(j in 1:2){
for(i in 1:length(mjd_shp_c_s)){    #mjd[[j]][[i]] 안되는거 이해함/ 이제는 될듯
  m1[[j]][[i]] <- mask(mjd[[j]], mjd_shp_c_s[i,])
  m2[[j]][[i]] <- crop(m1[[j]][[i]], mjd_shp_c_s[i,])
} }##################################
# 확인
plot(m2[[2]][[2]])
# alpha 제거후 RGB사진 분리 , 분리 결과 확인을 위한거
rgb <- list(rgb[[1]][[-4]],rgb[[2]][[-4]])
g1 <-  vector("list", length = length(rgb))  # 길이 정해줘야 되는듯
g2 <-  vector("list", length = length(rgb))
for(j in 1:2){       # 
for(i in 1:length(mjd_shp_c_s)){
  g1[[j]][[i]] <- mask(rgb[[j]], mjd_shp_c_s[i,])
  g2[[j]][[i]] <- crop(g1[[j]][[i]], mjd_shp_c_s[i,])
}}
# 확인 
plotRGB(g2[[1]][[1]])
# m1[[j]][[i]] 안되서 걍 2개 씀



# 전체 평균 C.V값 계산 na제거
# EXG, NIR 사용 23.23 / 15.15 window
mn <- c(1:6)
c.v <- c(1:6)
# CV평균값 생성
for(i in 1:6) {
  ndf <- as.data.frame(m2[[1]][[i]][[5]], na.rm = TRUE) # [[5]] NIR 값만 계산
#  ndf # nir 반사율 평균이이  0.06765654이라는데 넘 낮은거 아닌가? 물때문인가? / 7/10일 촬영
  mn[i] <- mean(ndf[,1]) # df로 변환되면서 1열이 됨, [,1] 안하면 numeric 대신 df로 받음음
  c.v[i] <- sd(ndf[,1])/mn[i] }
c.v
mcv <- mean(c.v)
mcv# 사용할 cv값   0.6496956
# # 둥글게 깍기
# library(smoothr)
# mjd_shp_c_ss <- smooth(mjd_shp_c_s,  method = "ksmooth", smoothness = 2.5)
# 
# m1 <- list()
# m2 <- list()
# 
# for(i in 1:6){
#   m1[[i]] <- mask(mjd, mjd_shp_c_ss[i,])
#   m2[[i]] <- crop(m1[[i]], mjd_shp_c_ss[i,])
# }

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
# 7/10일 용
# 
# c_f <- function(x, cv){
#   j <- x[(length(x) + 1) / 2] # 중간 픽셀의 값 저장
#   if (is.na(j)) {
#     return(4)
#   } else{
#     #    j <- x[(length(x) + 1) / 2] # 중간 픽셀의 값 저장
#     sdv <- sd(x[!is.na(x)])  # na값은 제외하고 값을 구함
#     me <- mean(x[!is.na(x)])
#     re <- (sdv / me)
#     if (re > 0.3 * cv) {  # # mixed class # 한번 하면 계속 유효함
#       if (me > j) {
#         return(2)   # 높을 시 도로 or 물 or 잡초
#       } else {      # 벼의 전체적인 NIR 반사율이 낮아서 반사율이 높은 잡초가 제거됨, 왜 낮은지는 나중에 알아볼 것
#         return(3)   # 낮으면 벼벼 #
#       }
#     } else {    # pure class
#       if (j > 0.1) {   # 7/10일은 0.1
#         return(1)   # 평균값이 픽셀 값보다 보다 높으면 땅
#       } else {
#         return(2)   # 낮으면 배경, 순수 쌀 픽셀이 없음 7/10에는 2
#       }             # 8/10에는 순수 쌀 픽셀이 생겨서 3
#     } }
# 
# }

# 8/3일용
c_f <- function(x, cv){
  j <- x[(length(x) + 1) / 2] # 중간 픽셀의 값 저장
  if (is.na(j)) {
    return(4)
  } else{
    #    j <- x[(length(x) + 1) / 2] # 중간 픽셀의 값 저장
    sdv <- sd(x[!is.na(x)])  # na값은 제외하고 값을 구함
    me <- mean(x[!is.na(x)])
    re <- (sdv / me)
    if (re > 0.3 * cv) {  # # mixed class # 한번 하면 계속 유효함
      if (me > j) {
        return(2)   # 높을 시 도로 or 물 or 잡초
      } else {      # 벼의 전체적인 NIR 반사율이 낮아서 반사율이 높은 잡초가 제거됨, 왜 낮은지는 나중에 알아볼 것
        return(3)   # 낮으면 벼벼 #
      }
    } else {    # pure class
      if (j > 0.1) {   # 7/10일은 0.1
        return(3)   # 평균값이 픽셀 값보다 보다 높으면 땅 1
      } else {
        return(2)   # 낮으면 배경, 순수 쌀 픽셀이 없음 7/10에는 2
      }             # 8/10에는 순수 쌀 픽셀이 생겨서 3 
    } }

}
re <- list()
re2 <- list()

for (i in 1:6) {   # 1번, 2번 조건을 다르게 해서 함 #mcv가 0.3CV 하는걸 고정인거지? 값이 고정인듯듯 0.6496956
tryCatch(   # 에러를 잡기 위함 
  re2[[i]] <- focal(m2[[2]][[i]][[5]], w=matrix(1,15,15), fun=function(x) c_f(x, mcv)),  
  error = function(e) {
    print("Error occurred:")
    print(e)
  }) }
# 객체값 저장
plot(re[[1]])
plot(re2[[2]])
plotRGB(g2[[2]][[6]])
# # 저장하기 # 저장이 안됨 이유는 ㅁㄹ?
# save.image("C:/Users/User/OneDrive/종합/Drone/사진에서 나온 값/새 폴더/TS.DT.RData")
# save(qjs1, file = "C:/Users/User/OneDrive/종합/Drone/사진에서 나온 값/새 폴더/file710.RData")
# save(re66, file = "C:/Users/User/OneDrive/종합/Drone/사진에서 나온 값/새 폴더/file803.RData")

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
# re, re2 
lili <- lapply(re2, as.data.frame)
names(lili) <- paste("df",1:6)
nrow(lili$`df 2`)
li_r <- list()
li_s <- list()

# df 변환후 행의 수 확인인

for (j in 1:6) {  # RGB확인할때 m을g로 ex) m[[2]][[j]] > 8/3일 multi 확인 / g[[1]][[j]] 7/10 rgb
  df <- as.data.frame(lili[[j]][lili[[j]]!= 4,])
  names(df) <- "df"     # 행 이름 변경
  #df2 <- lili$`df 2`[lili$`df 2` != 4 ,] # 4(na값 제거된 거)
  nrow(df)
  # df 가져오고 결합 # 공간 정보 더해주는거임임
  sss <- as.data.frame(m2[[2]][[j]])#왜 행이 안 맞을까? 
  nrow(sss)
  n_df <- cbind(sss,df)   # 3은 벼, 2는 배경경
  print(head(n_df))
  
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
  rns_values_r <- values(m2[[2]][[j]])
  rns_values_s <- values(m2[[2]][[j]])
  # 여기서 행열이 유지가 안됨됨, 해결함 행값에 넣어주면 됨됨
  rns_values_r[-as.numeric(mjd_r$i),] <- NA # 벼 외의 셀에 NA
  rns_values_r[as.numeric(nn$i),] <- NA  # 기타등에 NA
  
  rns_values_s[-as.numeric(mjd_s$i),] <- NA 
  rns_values_s[as.numeric(nn$i),] <- NA 
  # NA를 제거하고 새로운 SpatRaster 생성
  rns_newr <- m2[[2]][[j]]
  values(rns_newr) <- rns_values_r
  li_r[[j]] <- rns_newr
  rns_news <- m2[[2]][[j]]
  values(rns_news) <- rns_values_s
  li_s[[j]] <- rns_news
}
plotRGB(g2[[2]][[2]])
plotRGB(li_r[[2]])
plotRGB(li_s[[2]])
1+1
li_s[[2]]

la <- list(m2[[2]][[2]],li_r[[2]],li_s[[2]]) # 1 전체, 2 벼,3 배경

ee <- list()
for (i in 1:3) {
ndvi <- (la[[i]]$NIR - la[[i]]$Red)/(la[[i]]$NIR + la[[i]]$Red)
evi <- 2.5*(la[[i]]$NIR - la[[i]]$Red)/(la[[i]]$NIR + 6*la[[i]]$Red - 7.5*la[[i]]$Blue + 1)
ndwi <-(la[[i]]$NIR - la[[i]]$Green)/(la[[i]]$NIR + la[[i]]$Green)
ndre <- (la[[i]]$NIR - la[[i]]$`Red edge`)/(la[[i]]$NIR + la[[i]]$`Red edge`)
ee[[i]] = cbind("ndvi" = ndvi,"ndwi" = ndwi, "ndre" = ndre ,"evi"= evi)
}
# 1,2,3 다 넣음 반볻문 쓰기 싫어서
zz <- c(ee[[3]][[1]],ee[[3]][[4]], ee[[3]][[3]],ee[[3]][[2]] )
names(zz) <- c("NDVI" ,"EVI", "NDRE", "NDWI")
names(zz)
plot(zz)
##############################################
# 그래프용 df
la <- list(as.data.frame(m2[[1]][[2]]),as.data.frame(li_r[[2]]),as.data.frame(li_s[[2]]))
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
  theme_minimal() +
  xlim(-0.5, 1)

save.image("C:\\Users\\User\\OneDrive\\종합\\Drone\\사진에서 나온 값\\새 폴더\\file1.RData")

# 둥글게 깍기
# library(smoothr)
# mjd_shp_c_ss <- smooth(mjd_shp_c_s,  method = "ksmooth", smoothness = 2.5)
# 
# sm1 <-  vector("list", length = length(mjd))  # 길이 정해줘야 되는듯
# sm2 <-  vector("list", length = length(mjd))
# 
# 
# for (j in 1:2) {
# for(i in 1:6){
#   sm1[[j]][[i]] <- mask(m2[[j]][[i]], mjd_shp_c_ss[i,])
#   sm2[[j]][[i]] <- mask(m2[[j]][[i]], mjd_shp_c_ss[i,], inverse = TRUE)
# }}
# plot(sm1[[2]][[2]])
# plot(sm2[[2]][[2]])
# 1+1
# # 연습작들
# 
# ######  RGB에 넣어서 제대로 된건지 확인 ㄱ

























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



