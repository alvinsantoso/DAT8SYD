---
title: "Exploratory Data Analysis Melbourne Housing Market"
author: "Bukun"
output:
  html_document:
    number_sections: true
    toc: true
    fig_width: 7
    fig_height: 4.5
    theme: readable
    highlight: tango
---

#Introduction
Thanks for providing the wonderful dataset.The analysis has been structured as outlined in the index.

**Stay Tuned** more upates to come. If you like the preliminary EDA, please **upvote** the kernel.

## Where is the code ?

Please go to the code tab of the kernel . All the R code is present there. The code is in small blocks and uses **dplyr , plyr , ggplot2 packages** extensively.


```{r, message=FALSE, warning=FALSE,echo=FALSE}
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(caret)
library(stringr)
library(DT)


rm(list=ls())

fillColor = "#FFA07A"
fillColorRoom = "#F1C40F"

melData = read.csv("../input/Melbourne_housing_extra_data.csv")
```

# What's in the data ?

```{r, result='asis', echo=FALSE}
datatable(head(melData), style="bootstrap", class="table-condensed", options = list(dom = 'tp',scrollX = TRUE))
```

#Suburb Price Analysis

##Suburb Price Analysis (table format){.tabset}

### Top 20 Most Costliest Suburbs

```{r, result='asis', echo=FALSE}
suburbData = melData %>% filter(!is.na(Price)) %>%
             group_by(Suburb) %>% 
             summarise(AvgPricePerSuburb = round(median(Price),0)) %>%
             arrange(desc(AvgPricePerSuburb))

suburbData$AvgPriceSuburb = scales::dollar(suburbData$AvgPricePerSuburb)

suburbDataFull =suburbData %>% select(Suburb,AvgPriceSuburb)

datatable(head(suburbDataFull,n =20), style="bootstrap", class="table-condensed", options = list(dom = 'tp',scrollX = TRUE))
```

### Top 20 Least Costliest Suburbs

```{r, result='asis', echo=FALSE}
datatable(tail(suburbDataFull,n =20), style="bootstrap", class="table-condensed", options = list(dom = 'tp',scrollX = TRUE))
```

## Graph of Top 20 Most Costliest Suburbs 

```{r, message=FALSE, warning=FALSE,echo=FALSE}
suburbData = suburbData[1:20,]

suburbData$AvgPricePerSuburb2 = scales::dollar(suburbData$AvgPricePerSuburb)

ggplot(suburbData, aes(x = reorder(Suburb, AvgPricePerSuburb), 
                     y = AvgPricePerSuburb)) +
  geom_bar(stat='identity',colour="white", fill = fillColor) +
  geom_text(aes(x = Suburb, y = 1, label = paste0("(",AvgPriceSuburb,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Suburb', y = 'Price', title = 'Price and  Suburb') +
  coord_flip() + 
  theme_bw()

```

## Distribution of Prices of Top 10 Suburbs

```{r, message=FALSE, warning=FALSE,echo=FALSE}

suburbData2 = suburbData[1:10,]

TopSuburbData = inner_join(melData,suburbData2,by = "Suburb")

TopSuburbData = TopSuburbData %>% arrange(desc(AvgPricePerSuburb))

ggplot(TopSuburbData, aes(x = Suburb,y = Price)) +
  geom_boxplot(aes(fill=as.factor(Suburb))) +
  labs(x = 'Suburb', y = 'Price', title = 'Price and  Suburb') +
  coord_flip() + 
  theme_bw()

```

## Distribution of Prices of Top Next 10 Suburbs

```{r, message=FALSE, warning=FALSE,echo=FALSE}

suburbData2 = suburbData[11:20,]

TopSuburbData = inner_join(melData,suburbData2,by = "Suburb")

TopSuburbData = TopSuburbData %>% arrange(desc(AvgPricePerSuburb))

ggplot(TopSuburbData, aes(x = Suburb,y = Price)) +
  geom_boxplot(aes(fill=as.factor(Suburb))) +
  labs(x = 'Suburb', y = 'Price', title = 'Price and  Suburb') +
  coord_flip() + 
  theme_bw()

```


#Rooms and House Price Analysis

Price of the house increases with the **number of rooms** in the house.

```{r, result='asis', echo=FALSE}

roomData = melData %>% filter(!is.na(Price)) %>%
  group_by(Rooms) %>% 
  summarise(AvgPricePerRoom = round(median(Price),0)) %>%
  arrange(desc(AvgPricePerRoom))

roomData$AvgPriceRoom = scales::dollar(roomData$AvgPricePerRoom)

roomDataFull =roomData %>% select(Rooms,AvgPriceRoom)

colnames(roomDataFull) = c("No.of.Rooms","Price")

datatable(roomDataFull, style="bootstrap", class="table-condensed", options = list(dom = 'tp',scrollX = TRUE))
```

```{r, message=FALSE, warning=FALSE,echo=FALSE}
ggplot(roomData, aes(x = Rooms,y = AvgPricePerRoom)) +
  scale_x_continuous(breaks=c(0,2,4,6,8,10,12,14,16,18,20)) +
  geom_bar(stat='identity',colour="white", fill = fillColorRoom) +
  labs(x = 'No. of.Room(s)', y = 'Price', title = 'Price and  Rooms') +
  theme_bw()

```

#BedRooms and House Price Analysis

**Observations**

1. Price is highest at 6 bedrooms.
2. Price decreases after 6 bedrooms till 10 bedrooms.
3. There is an outlier at 20 bedrooms.

```{r, result='asis', echo=FALSE}

bedData = melData %>% filter(!is.na(Price)) %>%
  group_by(Bedroom2) %>% 
  summarise(AvgPricePerBed = round(median(Price),0)) %>%
  arrange(desc(AvgPricePerBed))

bedData$AvgPriceBed = scales::dollar(bedData$AvgPricePerBed)

bedDataFull =bedData %>% select(Bedroom2,AvgPriceBed)

colnames(bedDataFull) = c("No.of.Bedrooms","Price")

datatable(bedDataFull, style="bootstrap", class="table-condensed", options = list(dom = 'tp',scrollX = TRUE))

bedData$Bedroom2 = as.integer(bedData$Bedroom2)

```

```{r, message=FALSE, warning=FALSE,echo=FALSE}
ggplot(bedData, aes(x = Bedroom2,y = AvgPricePerBed)) +
  scale_x_continuous(breaks=c(0,2,4,6,8,10,12,14,16,18,20)) +
  geom_bar(stat='identity',colour="white", fill = fillColorRoom) +
  labs(x = 'No. of.Bed Room(s)', y = 'Price', title = 'Price and  Bedrooms') +
  theme_bw()

```

# House Price Influencers

We would like to investigate which factors influence the **Price** of the house. For this we build a **XGBoost** model on the data so as to find the importance of the various factors on **Price**.


```{r message=FALSE, warning=FALSE}

melData <- melData %>% 
  mutate(month_of_year = month(dmy(Date)))

melData <- melData %>% 
  mutate(yearOfSale = year(dmy(Date)))

transformdata = melData %>% filter(!is.na(Price)) %>%
                select(-c(Lattitude,Longtitude,Date,Address))

features <- colnames(transformdata)

for (f in features) {
  if ((class(transformdata[[f]])=="factor") || (class(transformdata[[f]])=="character")) {
    levels <- unique(transformdata[[f]])
    transformdata[[f]] <- as.numeric(factor(transformdata[[f]], levels=levels))
  }
}

formula = Price ~ .

fitControl <- trainControl(method="none")

xgbGrid <- expand.grid(nrounds = 500,
                       max_depth = 3,
                       eta = .05,
                       gamma = 0,
                       colsample_bytree = .8,
                       min_child_weight = 1,
                       subsample = 1)


set.seed(13)

melDataXGB = train(formula, data = transformdata,
                       method = "xgbTree",trControl = fitControl,
                       tuneGrid = xgbGrid,na.action = na.pass,metric="RMSE")

importance = varImp(melDataXGB)



varImportance <- data.frame(Variables = row.names(importance[[1]]), 
                            Importance = round(importance[[1]]$Overall,2))

# Create a rank variable based on importance
rankImportance <- varImportance %>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))

rankImportancefull = rankImportance

ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance)) +
  geom_bar(stat='identity',colour="white", fill = fillColor) +
  geom_text(aes(x = Variables, y = 1, label = Rank),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Variables', title = 'Relative Variable Importance') +
  coord_flip() + 
  theme_bw()


```

The graph shows the factors which affect the House Price.

All the factors affecting the **House Price** along with their ranks is provided below

```{r, result='asis', echo=FALSE}

datatable(rankImportancefull, style="bootstrap", class="table-condensed", options = list(dom = 'tp',scrollX = TRUE))
```


#Distance from CBD and Price analysis

The **House Price** decreases with the *increase in distance* from the CBD.

```{r, message=FALSE, warning=FALSE,echo=FALSE}

ggplot(melData)+
  geom_point(aes(x=Distance,y=Price))+
  stat_smooth(aes(x=Distance,y=Price),method="lm", color="red")+
  theme_bw()+
  theme(axis.title = element_text(size=16),axis.text = element_text(size=14))+
  xlab("(Distance from CBD)")+
  ylab("Price")


```

#Type and House Price analysis

```{r, message=FALSE, warning=FALSE,echo=FALSE}

TypeData = melData %>% filter(!is.na(Price)) %>%
  group_by(Type) %>% 
  summarise(AvgPricePerType  = round(median(Price),0)) %>%
  arrange(desc(AvgPricePerType ))

TypeData$AvgPriceType = scales::dollar(TypeData$AvgPricePerType )

TypeDataFull =TypeData %>% select(Type,AvgPriceType)

```

```{r, result='asis', echo=FALSE}
datatable(TypeDataFull, style="bootstrap", class="table-condensed", options = list(dom = 'tp',scrollX = TRUE))
```

```{r, message=FALSE, warning=FALSE,echo=FALSE}
ggplot(TypeData, aes(x = Type,y = AvgPricePerType )) +
  geom_bar(stat='identity',colour="white", fill = fillColorRoom) +
  labs(x = 'Type', y = 'Price', title = 'Price and  Type') +
  theme_bw()
```

**House prices** are clearly dependent on the **Type** of the house.

Type         | Description
--------------|-------------
h             | house,cottage,villa, semi,terrace
t             | townhouse
u             | unit


#Building Area and Price Analysis

```{r, message=FALSE, warning=FALSE,echo=FALSE}

melData$BuildingArea = as.numeric(melData$BuildingArea)

melData %>% filter(!is.na(Price)) %>% filter(!is.na(BuildingArea)) %>% 
ggplot(aes(x=BuildingArea,y=Price))+
  geom_point()+
  scale_x_continuous(limits=c(0,650)) +
  stat_smooth(aes(x=BuildingArea,y=Price),method="lm", color="red")+
  theme_bw()+
  theme(axis.title = element_text(size=16),axis.text = element_text(size=14))+
  xlab("(BuildingArea)")+
  ylab("Price")
```

The **House Price** increases with the **increase in Building Area**.

#Bathroom and Price Analysis

```{r, message=FALSE, warning=FALSE,echo=FALSE}

bathroomData = melData %>% filter(!is.na(Price)) %>%
  group_by(Bathroom) %>% 
  summarise(AvgPricePerRoom = round(median(Price),0)) %>%
  arrange(desc(AvgPricePerRoom))

bathroomData$AvgPriceRoom = scales::dollar(bathroomData$AvgPricePerRoom)

bathroomDataFull =bathroomData %>% select(Bathroom,AvgPriceRoom)

colnames(bathroomDataFull) = c("Bathroom","Average House Price")
```

```{r, result='asis', echo=FALSE}
datatable(bathroomDataFull, style="bootstrap", class="table-condensed", options = list(dom = 'tp',scrollX = TRUE))
```

```{r, message=FALSE, warning=FALSE,echo=FALSE}

bathroomData$Bathroom = as.integer(bathroomData$Bathroom)

ggplot(bathroomData, aes(x = Bathroom,y = AvgPricePerRoom)) +
  scale_x_continuous(breaks=c(0,2,4,6,8,10)) +
  geom_bar(stat='identity',colour="white", fill = fillColorRoom) +
  labs(x = 'No. of.Bathroom(s)', y = 'Price', title = 'Price and  Bath Rooms') +
  theme_bw()
```

#Which 2 bedroom Unit should I buy ?{.tabset}

##UpWard Trend for 2 bedroom Units

```{r, message=FALSE, warning=FALSE,echo=FALSE}
TwoBedroomUnit = melData %>% filter(!is.na(Price)) %>%
  filter(Bedroom2 == 2) %>%
  filter(Type == 'u') %>%
  group_by(Suburb,yearOfSale) %>%
  summarise(MedianPrice = median(Price)) 

TwoBedroomUnit2016 = TwoBedroomUnit %>% filter(yearOfSale == 2016)
TwoBedroomUnit2017 = TwoBedroomUnit %>% filter(yearOfSale == 2017)

TwoBedroomUnit2016 = TwoBedroomUnit2016 %>% select(Suburb,MedianPrice)
TwoBedroomUnit2017 = TwoBedroomUnit2017 %>% select(Suburb,MedianPrice)

colnames(TwoBedroomUnit2016) = c("Suburb","2016MedianPrice")
colnames(TwoBedroomUnit2017) = c("Suburb","2017MedianPrice")

TwoBedroomUnitOverall = inner_join(TwoBedroomUnit2016,TwoBedroomUnit2017,by="Suburb") 

TwoBedroomUnitOverall = TwoBedroomUnitOverall %>% arrange(desc(`2017MedianPrice`))

TwoBedroomUnitOverall$Difference = TwoBedroomUnitOverall$`2017MedianPrice` - TwoBedroomUnitOverall$`2016MedianPrice`


TwoBedroomUnitOverallUp = TwoBedroomUnitOverall %>% filter(Difference > 0)
TwoBedroomUnitOverallDown = TwoBedroomUnitOverall %>% filter(Difference < 0)

TwoBedroomUnitOverallUp$Difference = scales::dollar(TwoBedroomUnitOverallUp$Difference)
TwoBedroomUnitOverallDown$Difference = scales::dollar(TwoBedroomUnitOverallDown$Difference)
```


```{r, result='asis', echo=FALSE}
datatable(TwoBedroomUnitOverallUp, style="bootstrap", class="table-condensed", options = list(dom = 'tp',scrollX = TRUE))
```

##Downward Trend for 2 bedroom Units

```{r, result='asis', echo=FALSE}
datatable(TwoBedroomUnitOverallDown, style="bootstrap", class="table-condensed", options = list(dom = 'tp',scrollX = TRUE))
```


## Brighton Trend Analysis Two BedRoom Unit

```{r, message=FALSE, warning=FALSE,echo=FALSE}

#Two BedRoom Unit 

TwoBedroomUnit = melData %>% filter(!is.na(Price)) %>%
  filter(Bedroom2 == 2) %>%
  filter(Type == 'u') %>%
  group_by(Suburb,yearOfSale,month_of_year) %>%
  summarise(MedianPrice = median(Price))

#Brighton
TwoBedroomUnitSuburb = TwoBedroomUnit %>% 
                         filter(Suburb == 'Brighton') %>% 
                         arrange(yearOfSale,month_of_year)


TwoBedroomUnitSuburb = TwoBedroomUnitSuburb %>%
                      mutate(YrMonth = paste0(yearOfSale,"-",month_of_year,sep="")) %>%
                      arrange(yearOfSale,month_of_year)

TwoBedroomUnitSuburb$YrMonth = factor(TwoBedroomUnitSuburb$YrMonth,levels = TwoBedroomUnitSuburb$YrMonth)

ggplot(TwoBedroomUnitSuburb, aes(x = YrMonth,y = MedianPrice)) +
  geom_bar(stat='identity',colour="white", fill = fillColorRoom) +
  labs(x = 'Time', y = 'Price', title = 'Trend Brighton') +
  theme_bw()

```

## Canterbury Trend Analysis Two BedRoom Unit

```{r, message=FALSE, warning=FALSE,echo=FALSE}
TwoBedroomUnitSuburb = TwoBedroomUnit %>% 
                         filter(Suburb == 'Canterbury') %>% 
                         arrange(yearOfSale,month_of_year)


TwoBedroomUnitSuburb = TwoBedroomUnitSuburb %>%
                      mutate(YrMonth = paste0(yearOfSale,"-",month_of_year,sep="")) %>%
                      arrange(yearOfSale,month_of_year)

TwoBedroomUnitSuburb$YrMonth = factor(TwoBedroomUnitSuburb$YrMonth,levels = TwoBedroomUnitSuburb$YrMonth)

ggplot(TwoBedroomUnitSuburb, aes(x = YrMonth,y = MedianPrice)) +
  geom_bar(stat='identity',colour="white", fill = fillColorRoom) +
  labs(x = 'Time', y = 'Price', title = 'Trend Brighton') +
  theme_bw()

```

## Abbotsford Trend Analysis Two BedRoom Unit

```{r, message=FALSE, warning=FALSE,echo=FALSE}
TwoBedroomUnitSuburb = TwoBedroomUnit %>% 
                         filter(Suburb == 'Abbotsford') %>% 
                         arrange(yearOfSale,month_of_year)


TwoBedroomUnitSuburb = TwoBedroomUnitSuburb %>%
                      mutate(YrMonth = paste0(yearOfSale,"-",month_of_year,sep="")) %>%
                      arrange(yearOfSale,month_of_year)

TwoBedroomUnitSuburb$YrMonth = factor(TwoBedroomUnitSuburb$YrMonth,levels = TwoBedroomUnitSuburb$YrMonth)

ggplot(TwoBedroomUnitSuburb, aes(x = YrMonth,y = MedianPrice)) +
  geom_bar(stat='identity',colour="white", fill = fillColorRoom) +
  labs(x = 'Time', y = 'Price', title = 'Trend Brighton') +
  theme_bw()