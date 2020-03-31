---
title: "8장"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(fig.align = "center")
knitr::opts_chunk$set(warning = FALSE)
knitr::opts_chunk$set(message = FALSE)
```

#### <프로그램8-1>


```
library(zoo)
library(ggvis)
# 데이터 읽기
gdp_1    <- read.csv("gdp.csv",header=TRUE)
연도     <- seq(as.Date("1970-01-01"), as.Date("2013-10-01"), "3 months")
gdp_kr   <- cbind(연도, gdp_1)
# 그래프 그리기
gdp_kr %>% ggvis(~연도) %>%
     layer_lines(y=~gdp/1000,  opacity := input_slider(0, 1, label="Original series")) %>% 
     layer_lines(y=~gdpsa/1000, stroke := input_select(
       c("Red" = "red", "Blue" = "blue", "Green" = "green"), label = "SA Color")) %>%
     add_axis("x", title = "") %>%
     add_axis("y", title = "GDP")
```

#### <프로그램 8-2>

```
library(ggvis)
library(quantmod)
# Yahoo! Finance에서 종합주가지수 불러오기
kospi1 <- getSymbols("^KS11", auto.assign = FALSE)[, 4]
 kospi1$rate <- (kospi1$KS11.Close - lag(kospi1$KS11.Close,1))/lag(kospi1$KS11.Close,1) *100
kospi = as.data.frame(kospi1)
# 종합주가지수 변동률의 확률밀도함수 추정
kospi %>% ggvis(~na.omit(rate)) %>% 
     layer_densities(
      adjust = input_slider(.1, 5, value = 1, step = .1, label = "Bandwidth adjustment"),
      kernel = input_select( c("Gaussian" = "gaussian",
          "Epanechnikov" = "epanechnikov", "Rectangular" = "rectangular",
          "Triangular" = "triangular", "Biweight" = "biweight",
          "Cosine" = "cosine", "Optcosine" = "optcosine"), label = "Kernel"))
```

#### <프로그램 8-3>

```
shinyUI(pageWithSidebar(
  headerPanel('붓꽃 k-means 군집화'),
  sidebarPanel(
    selectInput('xcol', 'X 변수', names(iris)),
    selectInput('ycol', 'Y 변수', names(iris),
                selected=names(iris)[[2]]),
    numericInput('clusters', '군집 수', 3,
                 min = 1, max = 9)
  ),
  mainPanel(
    plotOutput('plot1')
  )
))
```


#### <프로그램 8-4>

```
palette(rainbow(9))
#names(iris) <-c("꽃받침길이", "꽃받침넓이","꽃잎길이", "꽃잎넓이", "종")
  
shinyServer(function(input, output, session) {
  selectedData <- reactive({
    iris[, c(input$xcol, input$ycol)]
  })
  clusters <- reactive({
    kmeans(selectedData(), input$clusters)
  })
  output$plot1 <- renderPlot({
    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedData(),
         col = clusters()$cluster,
         pch = 20, cex = 3)
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
  })
})
```

#### <프로그램 8-6>

```
# install.packages("googleVis")
library(googleVis)
x   = c(2.6,31.3,2.1,5.7,58.2)
x_n = c("농림어업", "광공업", "전기가스수도업", "건설업", "서비스업")
Encoding(x_n) <-"utf-8"
dd = data.frame(x_n,x)
Pie2 = gvisPieChart(dd, options=list(is3D=TRUE))
plot(Pie2)
print(Pie2, file='PieChart.html')
```

#### <프로그램 8-7>

```
library(googleVis)

## 세계 인구 데이터 설정
WorldPopulation=data.frame(Country=Population$Country,
                           Population.in.millions=round(Population$Population/1e6,0),
                           Rank=paste(Population$Country, "Rank:", Population$Rank))
## 그래프 그리기
G5 <- gvisGeoMap(WorldPopulation, "Country", "Population.in.millions", "Rank", options=list(dataMode="regions", width=600, height=300))
plot(G5)

```


#### <프로그램 8-8>

```
library(googleVis)

region_stat = read.csv("region1.csv", header=TRUE, sep=",")

G1 <-  gvisGeoMap(region_stat, locationvar='Region' ,numvar='소비자물가상승률', 
        options=list(region='KR', height=310, width=300, dataMode='markers',
                     colorAxis.minValue=0.0, colorAxis.maxValue=5.0) )

G2 <-  gvisGeoMap(region_stat, locationvar='Region' ,numvar='성장률', 
        options=list(region='KR', height=310, width=300, dataMode='markers',
                     colorAxis.minValue=0.0, colorAxis.maxValue=5.0) )

GG <- gvisMerge(G1,G2, horizontal=TRUE) 

T <- gvisTable(region_stat[,c(1,3,4,5)], options=list(width=620, height=200))
GT <- gvisMerge(GG, T, horizontal=FALSE) 
plot(GT)
```


#### <프로그램 8-9>


```
# install.packages("WDI")
library(WDI)
# 데이터 불러오기
inds <- c('NY.GDP.MKTP.CD', 'NY.GDP.PCAP.CD', 'NY.GDP.PCAP.PP.CD','NY.GDP.MKTP.KD.ZG',
          'SP.POP.65UP.TO.ZS', 'SP.RUR.TOTL.ZS', 'SP.POP.GROW')

indnams <- c("GDP", "1인당 국민소득", "1인당 국민소득(PPP)", "경제성장률",  "65세 이상 인구비중",  "비도시인구비중", "인구 증가율")

wdiData <- WDI(country=c("KR", "US", "JP", "KE", "AR", "PT", "PH", "IR"), indicator=inds, start=1960,
 end=format(Sys.Date(), "%Y"), extra=TRUE)
colnum <- match(inds, names(wdiData))
names(wdiData)[colnum] <- indnams

#  모션차트의 작성
library(googleVis)

WorldBank <- droplevels(subset(wdiData, !region %in% "Aggregates"))
M <- gvisMotionChart(WorldBank,
                      idvar="country", timevar="year",
                      xvar="65세 이상 인구비중", yvar="1인당 국민소득",
                      colorvar="region", sizevar="GDP",
                      options=list(width=700, height=600))
# 브라우저에서 그리기
plot(M)

```

#### <프로그램 8-11>

```
# 패키지 다운 받기
library(devtools)
# install_github("ramnathv/rCharts")
# install.packages("reshape)
library(rCharts)
library(reshape)

# 데이터 불러와서 변형
gdp_s <- read.csv("gdp_s.csv", header=TRUE)
 colnames(gdp_s)[2:8] <- substr(colnames(gdp_s), 2, 5)[2:8]
 gdp_s1 = melt(gdp_s, id="Industry")
 colnames(gdp_s1)[2:3] <- c("Year", "GDP Share")
# 그래프 작성하기
 hPlot(x="Industry", y="GDP Share", group="Year", type="column", data=gdp_s1) 
```


#### <프로그램 8-12>

```
library(rCharts)
source('https://raw.githubusercontent.com/walkerke/teaching-with-datavis/master/pyramids/rcharts_pyramids.R')
dPyramid('KS', seq(2000, 2050, 10), colors = c('black', 'red'))
```


