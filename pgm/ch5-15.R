library(sp)
gadm <- readRDS("./data/gadm36_KOR_1_sp.rds")

#library(raster) 
#gadm<-getData('GADM', country='KOR', level=1)  
#plot(gadm)      

# 시도인구 데이터 불러옴, 코드별로 정렬
population      <- read.csv("./data/pop_S.csv",header=T)
population_sort <- population[order(population$Code),]

# 구간 데이터로 변환
# 각 구간의 색을 할당하고 지도를 그림
# col = rainbow(length(levels(gadm$population)))
interval = c(0,5, 10, 15, 20, 25, 30, 35, 40, 45)
col = hcl.colors(13, "Set 3")[5:13]

gadm$pop1980 <- as.factor(cut(population_sort$Y1980,breaks=interval))
gadm$pop2040 <- as.factor(cut(population_sort$Y2040,breaks=interval))

# 각 구간의 색을 무지개 색으로 할당하고 지도를 그림
spplot(gadm, "pop1980", col.regions=col, main="1980 Distribution of population")
spplot(gadm, "pop2040", col.regions=col, main="2040 Distribution of population")
