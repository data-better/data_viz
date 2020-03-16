---
title: "3장"
output: html_document
---

#### RMarkdown Option

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(fig.align = "center")
knitr::opts_chunk$set(warning = FALSE)
knitr::opts_chunk$set(message = FALSE)
```


#### 3.1 산점도

```{r}
exam <- read.table("./data/ch3/exam_scores_2012.txt", header=T)
str(exam)
attach(exam)
plot(mid,final)
```


```{r}
summary(exam)
mid[is.na(mid)] <- 0
final[is.na(final)] <- 0
plot(mid,final,pch=20,xlim=c(-5,40),ylim=c(-5,40),col="blue",
   xlab="중간시험",ylab="기말시험",main="통계적 사고")
```


```{r}
set.seed(12); 
n <- length(mid)
plot(mid+runif(n,-0.5,0.5),final+runif(n,-0.5,0.5),pch=20,col=rainbow(5),method="square",xlim=c(-5,40),ylim=c(-5,40),
     xlab="중간시험",ylab="기말시험",main="통계적 사고")
```


#### 3.2 이변량 밀도

```{r}
library(KernSmooth)
exam <- exam[!is.na(exam$mid) & !is.na(exam$final),]
plot(exam$mid,exam$final,pch=20,xlim=c(-5,40),ylim=c(-5,40),col="blue",
   xlab="중간시험",ylab="기말시험",main="통계적 사고")
density <- bkde2D(exam,bandwidth=c(2.5,2.5))
par(new=T); 
contour(density$x1,density$x2,density$fhat,xlim=c(-5,40),ylim=c(-5,40), col=heat.colors(7)[7:1], nlevels=7, lwd=2)
```


#### 3.3 큰 자료의 산점도

```{r}
library(ggplot2)
data(diamonds)
str(diamonds)
attach(diamonds)

plot(carat,price,main="diamonds",xlim=c(-0.5,5.5),ylim=c(-1000,21000))
plot(carat,sqrt(price),main="diamonds",xlim=c(-0.5,5.5),ylim=c(0,160))
```

 

```{r}
library(hexbin)

hexbinplot(sqrt(price)~carat,data=diamonds,main="diamonds",xlim=c(-0.5,5.5),ylim=c(0,160),
    xbins=25,aspect=1,colorkey=F) 
hexbinplot(sqrt(price)~carat,data=diamonds,main="diamonds",xlim=c(-0.5,5.5),ylim=c(0,160),
    xbins=100,aspect=1,colorkey=F,colramp=function(n) magent(n,225,25))
```



#### 3.4 회귀적 관계

```{r}
plot(exam$mid,exam$final,pch=20,xlim=c(-5,40),ylim=c(-5,40),col="blue",
   xlab="중간시험",ylab="기말시험",main="통계적 사고")
abline(lm(exam$final~exam$mid),col="red")
diff <- mean(exam$final,na.rm=T)-mean(exam$mid,na.rm=T)
abline(c(diff,1), lty="dotted")
```



```{r}
plot(exam$mid,exam$final,pch=21,xlim=c(-5,40),ylim=c(-5,40),col="blue",
   xlab="중간시험",ylab="기말시험",main="통계적 사고")
lines(lowess(exam[!is.na(exam$mid)&!is.na(exam$final),],f=0.5),lwd=2)
abline(c(0,1), lty="dotted")
```



```{r}
diamonds$sqrt.price <- sqrt(price)
plot(sqrt(price) ~ carat,col="gray",main="diamonds",xlim=c(-0.5,5.5),ylim=c(0,160))
lines(lowess(diamonds$sqrt.price~diamonds$carat, f=0.1),lwd=2,col="blue")
lines(lowess(diamonds$sqrt.price~diamonds$carat, f=0.25),lwd=2,col="red",lty="dotted")
```



```{r}
plot(price ~ carat,col="gray",main="diamonds",xlim=c(-0.5,5.5),ylim=c(-1000,21000))
lines(lowess(diamonds$price~diamonds$carat, f=0.1),lwd=2,col="blue")
lines(lowess(diamonds$price~diamonds$carat, f=0.25),lwd=2,col="red",lty="dotted")
```


#### 3.5 모자이크 플롯

```{r}
data(Titanic)
str(Titanic)
addmargins(apply(Titanic,c(1,4),sum))
```

```{r}
mosaicplot(~ Class+Survived,data=Titanic,color=c("grey","red"))
```


```{r}
par(mfrow=c(1,2))
mosaicplot(~ Sex+Survived,data=Titanic,color=c("grey","red"))
mosaicplot(~ Age+Survived,data=Titanic,color=c("grey","red"))
```


```{r}
par(mfrow=c(1,2))
mosaicplot(~ Class+Survived,data=as.table(Titanic[,"Male","Adult",]),color=c("grey","red"),main="Male+Adult")
mosaicplot(~ Class+Survived,data=as.table(Titanic[,"Female","Adult",]),color=c("grey","red"),main="Female+Adult")
```


```{r}
mosaicplot(~ Class+Sex+Survived,dir=c("v","v","h"),data=Titanic[,,"Adult",],off=c(1,2),color=c("grey","red"),main="Adult")
```


#### 3.6 나무지도

```{r}
library(treemap)

GNI.2010 <- read.table("./data/ch3/GNI_2010.txt", header=T)[1:104,]
str(GNI.2010)
```


```{r}
treemap(GNI.2010,index=c("sector", "item"),vSize="principal",vColor="yield",
    type="value",bg.labels="yellow",title="Portpolio Evaluation GNI.2010 [1:104]")

```


```{r}
GNI.2010$yield.total <- GNI.2010$principal*as.numeric(GNI.2010$yield)
GNI.2010.a <- aggregate(GNI.2010[,3:5],by=list(GNI.2010$sector),sum)
GNI.2010.a$yield.avg <- GNI.2010.a$yield.total/GNI.2010.a$principal

treemap(GNI.2010.a,index=c("Group.1"),vSize="principal",vColor="yield.avg",
    type="value",bg.labels="yellow",title="Portpolio Evaluation GNI.2010 [1:104]")
```


