---
title: "1장"
output: html_document
---


#### RMarkdown Option

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(fig.align = "center")
knitr::opts_chunk$set(warning = FALSE)
knitr::opts_chunk$set(message = FALSE)
```


#### p11 프로그램

```{r}
kings <- read.table("./data/ch1/chosun_kings.txt", header=T)
str(kings)
head(kings)
attach(kings)
median(Life)
median(Period)
```


#### p12, 13 프로그램

```{r}
hist(Period, right=F) 
# savePlot("figure 3.png", type="png")
hist(Period, xlim=c(0,70), ylim=c(0,10), nclass=14, right=F, main="조선 왕조", xlab="재위기간(년)", ylab="빈도")
# savePlot("figure 4.png", type="png")
hist(Period, xlim=c(0,70), ylim=c(0,10), nclass=14, right=F, main="조선 왕조", xlab="재위기간(년)", ylab="빈도", col=c(rep("lightblue",2),rep("royalblue",6),rep("navyblue",6)))
# savePlot("figure 5.png", type="png")
```


#### p20-23 프로그램

```{r}
plot(c(0,0),c(28,60),type="n",xlim=c(0,28),ylim=c(-5,65),xlab="순서",ylab="재위기간",main="조선 왕조")
points(1:27,Period,pch=15,col="red")
segments(1:27,rep(0,27),1:27,Period,lwd=3,col="red")
abline(h=c(0,mean(Period)),lty="dotted",lwd=1,col="blue")
```


#### p23 프로그램

```{r}
plot(1:27,Period,pch=15,col="blue",xlim=c(0,28),ylim=c(-5,65),xlab="순서",ylab="재위기간",main="조선 왕조")
par(new=T)
plot(1:27,rep(0,27),pch=15,col="blue",xlim=c(0,28),ylim=c(-5,65),xlab="",ylab="",main="")
abline(h=c(0,mean(Period)),lty="dotted",lwd=1,col="blue")
segments(1:27,rep(0,27),1:27,Period,lwd=3,col="blue")
```


#### p24 프로그램


```{r}
plot(c(0,25),c(0,500),col="blue",type="n",xlab="",ylab="",main="")
rect(5,100,10,200,col="royalblue",border="royalblue")
rect(20,400,22.5,450,col="royalblue",border="royalblue")
```


#### p26 프로그램

```{r}
P <- cumsum(Period)
plot(1:27,P,type="n",xlab="순서",ylab="누적년수",main="조선 왕조")
rect(0,0,1,P[1],col="royalblue",border="royalblue")
for (i in 2:27) rect(i-1,P[i-1],i,P[i],col="royalblue",border="royalblue")
segments(0,0,27,518,lty="dotted")
```

```{r}
P <- cumsum(Period)
plot(1:27,P,type="n",xlab="순서",ylab="누적년수",main="조선 왕조")
polygon(c(0,0,1,1),c(0,P[1],P[1],0),col=rainbow(27)[1])
for (i in 2:27) polygon(c(i-1,i-1,i,i),c(P[i-1],P[i],P[i],P[i-1]),col=rainbow(27)[i])
segments(0,0,27,518,lty="dotted")
```

