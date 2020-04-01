---
title: "5장"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(fig.align = "center")
knitr::opts_chunk$set(warning = FALSE)
knitr::opts_chunk$set(message = FALSE)
```

```{r}
# sp 패키지를 설치한다
install.packages("sp")
library(sp)
```

```{r}
# 우리나라 전체 지도 (<그림 5-1>)  gadm이라는 데이터프레임이 자동으로 생성됨
load("./data/KOR_adm0.RData")
plot(gadm)
```

```{r}
# 우리나라 시도별 지도 (<그림 5-2>)
load("./data/KOR_adm1.RData")
plot(gadm)
```
```{r}
# 우리나라 시군구별 지도 (<그림 5-3>)
kor <- load("./data/KOR_adm2.RData")
plot(gadm)
```

```{r}
# 서울의 구별 지도색을 초록색으로 지정
seoul <- gadm[gadm$NAME_1=="Seoul",]
plot(seoul, col="green")
```


```{r}
install.packages("maps")
library(maps)
map()   # 해상도가 낮은 세계지도 그리기
```

```{r}
# 우리나라 지도 그리기
map(database="world",region="South Korea")
```


```{r}
# 동아시아 지도 그리기
eastasia <- c("South Korea", "North Korea", "China", "Japan" )
map(database="world",region=eastasia)

```

```{r}
# install.packages(“ggplot2”)
library(ggplot2)
# ggplot2를 이용한 세계 지도 그리기
wrld <- map_data("world")
qplot(long, lat, data = wrld, geom = "polygon", group= group)
```


```{r}
# 동아시아 지역 데이터 설정
wrld <- map_data("world")
east <- wrld$region %in% c("South Korea", "North Korea", "China", "Japan" )
eastasia <- wrld[east,]

# 동아시아 지도 그리기
qplot (long, lat, data = eastasia, geom = "polygon", fill=region, group= group)
```

```{r}
# 구글맵을 활용한 우리나라 지도
# 반드시 구글가 가서 지도 API 인증키를 받아야 함
# 참고 https://mrkevinna.github.io/R-%EC%8B%9C%EA%B0%81%ED%99%94-3/
library(ggmap)
library(ggplot2)
register_google(key = 'Your API')

# 도로지도 선택
qmap('서울',   maptype = 'roadmap')
qmap('동숭동', maptype = 'roadmap', zoom=16)
qmap('동숭동', maptype = 'toner', zoom=16)

# 위성지도 선택
qmap('서울', maptype = 'satellite')

# 혼합지도 선택
qmap('부산', maptype = 'hybrid')

geocode("서울", output='latlona')
# 127.  37.6 seoul, south korea

seoul <- get_map(location = "서울", color = "bw",
                zoom = 11, maptype = "toner", source = "google")

set.seed(12345)
foo <- data.frame(long = runif(200, 126.8, 127.2),
                   lat = runif(200, 37.4, 37.7))

ggmap(seoul) +
  geom_point(data = foo, aes(x = long, y = lat), color = "red")

```


```{r}
library(sp)

# 시도별 행정지도 데이터 불러와 지도를 그림, gadm 데이터프레임이 자동으로 생성됨
load("./data/KOR_adm1.RData")
plot(gadm, col="grey75")

# 시도 오존 데이터 불러옴
pollution <- read.table("./data/pollution.txt",header=T)

# 도시명을 넣은 사각형 크리를 위한 값 설정
pollution$broadth<-2/5
pollution$height<-0.1
pollution$space<-0.1
spaceDif<-0.05

# 지도위에 도시의 점 그리기
for (i in 1:dim(pollution)[1]) {
  coords <-  SpatialPoints(data.frame(cbind(pollution$x경도[i], pollution$y위도[i])), 
                           proj4string = CRS("+proj=longlat"))
  plot(coords, col ="red3", pch = 20, cex = 1.5,    add = TRUE)
}

# 도시 옆에 이름 넣을 사각형 그리기
for (i in 1:dim(pollution)[1]) {

  a<-c(pollution$x경도[i]-pollution$broadth[i],
        pollution$x경도[i]+pollution$broadth[i],
        pollution$x경도[i]+pollution$broadth[i],
        pollution$x경도[i]-pollution$broadth[i])

  b<-c(pollution$y위도[i]+pollution$space[i]-pollution$height[i]+spaceDif,
       pollution$y위도[i]+pollution$space[i]-pollution$height[i]+spaceDif,
       pollution$y위도[i]+pollution$space[i]+pollution$height[i]+spaceDif,
       pollution$y위도[i]+pollution$space[i]+pollution$height[i]+spaceDif)

  polygon(x=a,y=b, col="white")  

}

library(stringr)
cityLabels<-str_c(pollution$시도,pollution$오존)
cityCoord<-matrix(c(t(pollution$x경도),t(pollution$y위도+pollution$space+spaceDif)),
                dim(pollution)[1])
text(cityCoord, labels = cityLabels, cex=0.6, bg="white")
text(128,38.6, labels="도시별 오존농도", cex=2)
```

```{r}
# sp 패키지의 설치와 라이브러리 불러옴
library(sp)

# 시도별 행정지도 데이터 불러옴, gadm 데이터프레임이 자동으로 생성됨
load("./data/KOR_adm1.RData")

# 시도인구 데이터 불러옴, 코드별로 정렬
population <- read.table("./data/city_pop.txt",header=T)
population_sort <- population[order(population$Code),]

# 인구를 구별하기 위한 7개의 구간 설정하고 각 시도 인구를 이 구간 데이터로 변환
# (100,200], (200,300], (200,300], (300,400], (400,900], (900,1100], (1100, 1300]
interval <- c(0,100,200,300,400,900,1100,1300)
population_cut <- cut(population_sort$Y2010,breaks=interval)
gadm$population <- as.factor(population_cut)

# 각 구간의 색을 무지개 색으로 할당하고 지도를 그림
col = rainbow(length(levels(gadm$population)))
spplot(gadm, "population", col.regions=col, main="2010 Distribution of population")
```
```{r}
# sp  패키지의 설치와 라이브러리 불러옴
# install.packages('sp') 
library(sp) 

# 시도별 행정지도 데이터 불러옴, gadm 데이터프레임이 자동으로 생성됨
load("./data/KOR_adm1.RData")

# 시도인구 데이터 불러옴, 코드별로 정렬
population <- read.table("./data/city_pop.txt",header=T)
population_sort <- population[order(population$Code),]

# 인구를 구별하기 위한 7개의 구간 설정하고 각 시도 인구를 이 구간 데이터로 변환
# (100,200], (200,300], (200,300], (300,400], (400,900], (900,1100], (1100, 1300]
interval <- c(0,100,200,300,400,900,1100,1300)
population_cut <- cut(population_sort$Y2010,breaks=interval)
gadm$population <- as.factor(population_cut)

# 각 구간의 색을 무지개 색으로 할당하고 지도를 그림
col = rainbow(length(levels(gadm$population)))
spplot(gadm, "population", col.regions=col, main="2010년 시도별 인구분포")

# 2010년 시도별 인구분포의 객체를 만든다.
p1 <- spplot(gadm, "population", col.regions=col, main="2010년 시도별 인구분포")

# 2000년도 시도별 인구분포의 객체를 만든다.
population_cut <- cut(population_sort$Y2000,breaks=interval)
gadm$population <- as.factor(population_cut)
col = rainbow(length(levels(gadm$population))) # 각 구간의 색 할당
p2 <- spplot(gadm, "population", col.regions=col, main="2000년 시도별 인구분포")

# 1990년도 시도별 인구분포의 객체를 만든다.
population_cut <- cut(population_sort$Y1990,breaks=interval)
gadm$population <- as.factor(population_cut)
col = rainbow(length(levels(gadm$population))) # 각 구간의 색 할당
p3 <- spplot(gadm, "population", col.regions=col, main="1990년 시도별 인구분포")

# 1980년도 시도별 인구분포의 객체를 만든다.
population_cut <- cut(population_sort$Y1980,breaks=interval)
gadm$population <- as.factor(population_cut)
col = rainbow(length(levels(gadm$population))) # 각 구간의 색 할당
p4 <- spplot(gadm, "population", col.regions=col, main="1980년 시도별 인구분포")

print(p4, pos=c(0,0.5,0.5,1),more=TRUE)
print(p3, pos=c(0.5,0.5,1,1),more=TRUE)
print(p2, pos=c(0,0,0.5,0.5),more=TRUE)
print(p1, pos=c(0.5,0,1,0.5),more=TRUE)
```

