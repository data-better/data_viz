---
title: "2장"
output: html_document
---

### RMarkdown Option

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(fig.align = "center")
knitr::opts_chunk$set(warning = FALSE)
knitr::opts_chunk$set(message = FALSE)
```
#### <프로그램 2-1>

```{r}

# 원그래프 그리기

혈액형 = c("A", "B", "B", "A", "A", "O", "A", "AB", "O", "O", 
        "O", "A", "A", "B", "AB", "A", "O", "B", "A", "B", 
        "B", "A", "B", "A", "B", "AB", "B", "A", "O", "AB", 
        "O", "B", "A", "B", "A", "O", "B", "A", "A", "A",
        "A", "O", "A", "O", "O", "B", "B", "O", "AB", "A",
        "B", "AB", "B", "O", "O", "O", "AB", "O", "O", "B", 
        "A", "A", "O", "A", "B", "O", "A", "O", "B", "O",
        "A", "B", "O", "AB", "B", "B", "A", "O", "B", "A",
        "B", "B", "O", "AB", "B", "A", "AB", "A", "B", "A",
        "A", "O", "O", "A", "A", "O", "AB", "A", "A", "O")

혈액형

정렬.혈액형 = sort(table(혈액형), decreasing = T)
정렬.혈액형
slices = c("red", "blue", "yellow", "green")
pie(정렬.혈액형, col=slices, radius=1, main="원그래프")
```

#### <프로그램 2-2>, <프로그램 2-3>

```{r}
# 원그래프 그리기
require(grDevices)
pie.vote <- c(0.5067, 0.0167, 0.0100, 0.0433, 0.4233)
names(pie.vote) <- c("새누리 152명", "선진 5명", "무 3명", "진보 13명", "민주 127명")
par(mfrow=c(1,2))
pie(pie.vote, col=c("red3", "blue", "green3", "magenta", "yellow"), 
    main="19대 국회의원 선거")

pie(pie.vote, col=c("red3", "blue", "green3", "magenta", "yellow"), 
    main="19대 국회의원 선거")
par(new=TRUE)
pie(c(152, 127, 13, 5, 3), radius = 0.5, col="white", label=NA, border=NA)
text(0,0,"총 300석")

``` 

#### <프로그램 2-4>

```{r}
# 자료입력
혈액형 = c("A", "B", "B", "A", "A", "O", "A", "AB", "O", "O", 
        "O", "A", "A", "B", "AB", "A", "O", "B", "A", "B", 
        "B", "A", "B", "A", "B", "AB", "B", "A", "O", "AB", 
        "O", "B", "A", "B", "A", "O", "B", "A", "A", "A",
        "A", "O", "A", "O", "O", "B", "B", "O", "AB", "A",
        "B", "AB", "B", "O", "O", "O", "AB", "O", "O", "B", 
        "A", "A", "O", "A", "B", "O", "A", "O", "B", "O",
        "A", "B", "O", "AB", "B", "B", "A", "O", "B", "A",
        "B", "B", "O", "AB", "B", "A", "AB", "A", "B", "A",
        "A", "O", "O", "A", "A", "O", "AB", "A", "A", "O")

혈액형

정렬.혈액형 = sort(table(혈액형), decreasing = T)
정렬.혈액형
slices = c("red", "blue", "yellow", "green")

# 막대그래프 그리기 
barplot(정렬.혈액형, col=slices, main="혈액형별 막대그래프")
```

#### <프로그램 2-5>


```{r}
# 막대그래프 그리기
require(grDevices)
pie.vote <- c(0.5067, 0.0167, 0.0100, 0.0433, 0.4233)
names(pie.vote) <- c("새누리 152명", "선진 5명", "무 3명", "진보 13명", "민주 127명")

barplot(pie.vote, col=c("red3", "blue", "green3", "magenta", "yellow"), 
        main="정당별 막대그래프")
```

#### <프로그램 2-6> : class1 = seq(30, 150, by=10) 추가
 
```{r}
# 외부 파일을 읽어 데이터 프레임 만들기
담즙과포화비율 = read.table("./data/ch2/health.txt", header=T)
attach(담즙과포화비율)
str(담즙과포화비율)

# 담즙과포화비율 - 자료의 크기
n = length(담즙의과포화비율)
n

# 담즙과포화비율 - 자료의 정렬
sort(담즙의과포화비율)
sort(담즙의과포화비율, decreasing = T)

class1 = seq(30, 150, by=10)
# 담즙과포화비율 히스토그램과 개별 자료
par(mfrow=c(2,1))
hist(담즙의과포화비율, breaks=class1, main=NULL)
rug(jitter(담즙의과포화비율))

hist(담즙의과포화비율, breaks=class1, right=F, main=NULL)
rug(jitter(담즙의과포화비율))
#detach(담즙과포화비율)
```

#### <프로그램 2-7>

```{r}

class1 = seq(30, 150, by=10)
# 담즙과포화비율 히스토그램과 개별 자료
par(mfrow=c(2,1))
hist(담즙의과포화비율, breaks=class1, main=NULL)
rug(jitter(담즙의과포화비율))
hist(담즙의과포화비율, breaks=class1, right=F, main=NULL)
rug(jitter(담즙의과포화비율))

# 상대도수밀도히스토그램, 커털밀도추정량과 개별 자료
m = matrix(c(1,3,2,3), ncol=2, byrow=T)
layout(mat=m)
hist(담즙의과포화비율, prob=T, breaks=class1, right=F, ylab="상대도수", main=NULL)
lines(density(담즙의과포화비율, bw=5), col="red")
rug(담즙의과포화비율, col="blue")
detach(담즙과포화비율)
```

#### <프로그램 2-8>

```{r}
require(stats)
data("faithful")

eruption.length <- faithful$eruptions

# 원자료의 크기
n = length(eruption.length)
n

# 원자료의 범위
range(eruption.length)

# 원자료에 대한 정렬
sort(eruption.length)

# 원자료에 대한 평균과 분산
mean(eruption.length)
var(eruption.length)

class1 = seq(1.6, 5.1, by=0.5)
class2 = seq(1.85, 4.85, by=0.5)


# 계급의 폭을 0.5로 하고 제1계급의 하한값(원점이라고도 함)을 1.6으로 하는 도수분포표
cat.class1 = cut(eruption.length, breaks=class1)
t1 = table(cat.class1)
t1

# 계급의 폭을 0.5로 하고 제1계급의 하한값(원점이라고도 함)을 1.85으로 하는 도수분포표
cat.class2 = cut(eruption.length, breaks=class2)
t2 = table(cat.class2)
t2

# class1 도수분포표를 이용하여 구하는 평균과 분산
m1 = (class1 + 0.25)[-length(class1)]
f1 = as.vector(t1)

mean1 = sum(m1*f1)/sum(f1)
mean1

var1 = sum((m1-mean1)^2*f1)/sum(f1)
var1

# class2 도수분포표를 이용하여 구하는 평균과 분산
m2 = (class2 + 0.25)[-length(class2)]
f2 = as.vector(t2)

mean2 = sum(m2*f2)/sum(f2)
mean2

var2 = sum((m2-mean2)^2*f2)/sum(f2)
var2

# 히스토그램
class1 = class1
class2 = c(1.35, class2, 5.35)
par(mfrow=c(1,2))
hist(eruption.length, breaks=class1, main="간헐온천 지속시간에 대한 히스토그램 \n origin : 1.6", 
     xlab="간헐온천 지속시간")

hist(eruption.length, breaks=class2, main="간헐온천 지속시간에 대한 히스토그램 \n origin : 1.85", 
     xlab="간헐온천 지속시간")

```

#### <프로그램 2-9>

```{r}

# 각각 제1계급의 하한값을 1.6과 1.85로 하는 히스토그램에서 계급의 폭 변동하기

hist.func2 = function(n){
  par(mfrow=c(n,2))
  for(i in 1:n){
    class1 = seq(1.1, 5.1, by=0.5/(2*i - 1))
    class2 = seq(1.35, 5.35, by=0.5/(2*i - 1))
    hist(eruption.length, breaks=class1, prob=T, main="origin:1.6", xlab=NULL)
    hist(eruption.length, breaks=class2, prob=T, main="origin:1.85", xlab=NULL)
  }
}

hist.func2(3)

# 도수분포표(계급의 폭 : 0.1, 0.2, 0.3, 0.4, 0.5)
w = c(0.1, 0.2, 0.3, 0.4, 0.5)
for(i in 1:5){
  class1 = seq(1.1, 5.5, by=w[i])
  cat.class1 = cut(eruption.length, breaks = class1)
  table(cat.class1)
  cat("계급의 폭 = ", w[i], "\n")
  print(table(cat.class1))
}

```

#### <프로그램 2-10>

```
# 임의의 수 생성
runif(n, min, max)
```

```{r}
# 균일분포로부터 난수생성 자료의 분포 히스토그램
x <- runif(400, -1, 1)
hist(x)
```

#### <프로그램 2-11>

```
# 정규분포로부터의 임의의 수 생성
rnorm(n, mean, sd)
```

#### <프로그램 2-12>

```{r}
# 정규분포로부터 난수생성 자료의 분포 히스토그램
x <- rnorm(400)
hist(x)
```

#### <프로그램 2-13>

```{r}
# 이항분포 임의의 수 생성
rbinom(n, size, prob)
```

#### <프로그램 2-14>
```{r}
# 포아송분포 임의의 수 생성
rpois(n lambda)
```
#### <프로그램 2-15>

```{r}
# 이항분포로부터 난수생성 자료의 분포 히스토그램
z <- rbinom(1000, 10, 0.5)
table(z)

hist(z)

# 포아송분포로부터 난수생성 자료의 분포 히스토그램
z <- rpois(1000, 5)
table(z)

hist(z)
```

#### <프로그램 2-16>

```{r}
# 상자그림 작성
담즙과포화비율 = read.table("./data/ch2/health.txt", header=T)
attach(담즙과포화비율)
boxplot(담즙의과포화비율, col="yellow", horizontal=T, main=NULL)
rug(담즙의과포화비율, col="blue")
```

#### <프로그램 2-17>

```{r}
# 상자그림과 바이올린그림

# install.packages("vioplot")
require(vioplot)
attach(담즙과포화비율)
par(mfrow=c(1,2))
boxplot(담즙의과포화비율 ~ 성별, notch=T, col="yellow", main=NULL)
male <- 담즙의과포화비율[성별=="남자"]
female <- 담즙의과포화비율[성별=="여자"]
vioplot(male, female)
```

#### <프로그램 2-18>

####  한글 이름 파일명을 영문으로 변경

```{r}
# 외부파일 읽어 상자그림 작성
국회의원 = read.csv("./data/ch2/Assembly_voter.csv", header=T)
attach(국회의원)
str(국회의원)
boxplot(선거인.수, col="yellow", horizontal = T, main=NULL)
```

#### <프로그램 2-19>

```{r}
electorate <- read.csv("./data/ch2/Assembly_voter.csv", header=T)
str(electorate); attach(electorate)
summary(선거인.수)

boxplot(선거인.수, col="yellow", ylim=c(0,250000), ylab="전국", xlab="선거인.수", horizontal = T)

시도.순서 <- reorder(시도, 시도번호)
bp <- boxplot(선거인.수 ~ 시도.순서)
order <- rank(-bp$stats[3,])
boxplot(선거인.수 ~ 시도.순서, col=heat.colors(16)[order], ylim=c(0,250000), ylab="선거인.수",
           main="우리나라 18대 국회의원 선거구의 선거인 수 분포")
```
#### <프로그램 2-20>

```{r}
exam1 <- read.table("./data/ch2/exam1.txt", header=T)
str(exam1)
attach(exam1)
stem(score)
stem(score, scale=2)
```










