---
title: "7장"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(fig.align = "center")
knitr::opts_chunk$set(warning = FALSE)
knitr::opts_chunk$set(message = FALSE)

```


## chapter 7

#### 7.1 별그림

```{r}
baseball <- read.csv("프로야구20060602.csv", header=T)
x <- baseball[,2:4]

x[,1] <- 0.2 + 0.8*(x[,1]-min(x[,1]))/(max(x[,1])-min(x[,1]))
x[,2] <- 0.2 + 0.8*(x[,2]-min(x[,2]))/(max(x[,2])-min(x[,2]))
x[,3] <- 0.2 + 0.8*(x[,3]-min(x[,3]))/(max(x[,3])-min(x[,3]))

rownames(x) <- baseball[,1]

stars(x, scale=F, key.loc = c(7,2), col.stars=2:9)

stars(x, scale=F, draw.segments = T, full=F, key.loc = c(7,2))

```


#### 7.2 산점도 행렬

```{r}
data(iris)
attach(iris)
pairs(iris[1:4], main="Iris Data")

pairs(iris[1:4], main="Iris Data", pch=21, bg=c("red", "green3", "blue")[unclass(iris$Species)])
```

#### 7.3 조건부 플롯

```{r}
# equal.count(z, number=k1, overlap=k2)

library(lattice)
data("quakes")
str(quakes)
attach(quakes)
xyplot(lat ~ long, main="Earthquakes in Fiji", pch="*", cex=2)

# xyplot(y ~ x | z)

depth.grp <- equal.count(depth, number=4, overlap=0)
xyplot(lat ~ long | depth.grp, main="Earthquakes in Fiji", pch="*")
```

#### 7.4 평행좌표 플롯과 변수 순서화

```{r}
library(lattice)

data(iris)
parallelplot(~iris[,1:4])
parallelplot(~iris[,c(1,3,2,4)])
parallelplot(~iris[,1:4] | iris$Species)
parallelplot(~iris[,c(1,3,2,4)] | iris$Species)
```

#### 7.4.1 끝잇기 알고리즘

```{r}
# install.packages("gclus")
library(gclus)

round(cor(iris[,1:4]),2)
order <- order.endlink(cor(iris[,1:4]))
order

round(cor(iris[,order]), 2)
parallelplot(~iris[,c(order)])
```

#### 7.5 차원 축소에 의한 시각화 : 주성분분석

```{r}

# princomp(x, cor=T)

protein <- read.table("protein.txt", header=T, sep="\t")
protein

pca <- princomp(protein[,2:10], cor=T)
pca
names(pca)

pca$loadings[,1:2]
pca$scores[,1:2]
attach(pca)

plot(scores[,2] ~ scores[,1], main="Principal Component Space", xlim=c(-5,5), ylim=c(-5,5))
text(y=scores[,2], x=scores[,1],label=protein$Country, cex=0.8)
plot(loadings[,2] ~ loadings[,1], main="Principal Component Loadings", xlim=c(-1,1), ylim=c(-1,1))
text(y=loadings[,2], x=loadings[,1],label=colnames(protein[,2:10]), cex=0.8)

for(i in 1:9){
  arrows(0,0,0.8*loadings[i,1], 0.8*loadings[i,2], length=0.1)
}
```


#### 7.5 공간자료의 표현

```{r}

x <- 10*(1:87)
y <- 10*(1:61)
contour(x,y,volcano, main="Maunga Whau")
image(x,y, volcano, main="Maunga Whau")
filled.contour(x,y,volcano, main="Maunga Whau")
persp(x,y,volcano, phi=30, theta=30, scale=F, main="Maunga Whau")
```

