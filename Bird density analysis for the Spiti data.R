## Setting the path to the desitnation folder
setwd("/Users/Kullu/Dropbox (Snow Leopard Trust)/Kullu_desktop/Git/Bird-repository/Data")
library(Distance)
library(Hmisc)
## Importing necessary dataset
dat<-read.csv("Bird data Kibber 2002-2013 for distance.csv")
dat1<-read.csv("Bird data 2002-2017.csv")
dat1<-subset(dat1, dat1$Tname != "Lossar 1")
## exploratory analysis
names(dat)
names(dat1)
dat1$Effort<-rep(500, length(dat1[,1]))
dat1$Area<-rep(10000, length(dat1[,1]))
colnames(dat1)[9]<-"size"
colnames(dat1)[10]<-"distance"
#Sample.label is a field to be created to indicate the transects uniquely
dat1$Sample.Label<-(paste(dat1$Year, dat1$Month, dat1$Repl, dat1$Tname))
#dat1$Region.Label<- rep("Spiti", length(dat1[,1]) )
dat1$Region.Label<- dat1$Year
## Density analysis
bin<-c(0,10,20,30,40,50,60,70,80,90)
model1 <- ds(dat1, truncation=90, cutpoints=bin,key="hn", transect=c("line"), order=2)
summary(model1)

## Creating a balnk data frame with all the transects so that when I subset there will be information on empty transect in the data

dat.blank<- data.frame(Sno = seq(1,length(unique(dat1$Sample.Label,1))))

dat.blank$Sample.Label <- unique(dat1$Sample.Label)
dat.blank$Area<- rep(10000, length(dat.blank[,1]))
dat.blank$Effort <- rep(500, length(dat.blank[,1]))
dat.blank$distance <- rep(NA, length(dat.blank[,1]))
dat.blank$Region.Label  <- rep(NA, length(dat.blank[,1])) #needs to be same with the rest of the data frame every time   
for(i in 1:length(dat.blank[,1])){
   dat.blank$Region.Label[i] <- (strsplit(dat.blank$Sample.Label, " ")[[i]][1])
   }

## Estimating species density
## Black redstart
dat1.blre <- subset(dat1, dat1$Species == "BLACK REDSTART", 
                    select = c("Sno", "Sample.Label", "Area", "Effort", "distance", "Region.Label"))  
dat1.blre<-rbind(dat1.blre, dat.blank)
model.blackredstart<-(ds(dat1.blre, truncation=90, cutpoints=bin,key="hn", transect=c("line"), order=2))
barplot(model.blackredstart$dht$individuals$D$Estimate*10000, 
        names = model.blackredstart$dht$individuals$D$Label, 
        las = 2, space = 0, ylim = c(0,3), main = "Black Redstart")
errbar(seq(0.5,13.5,1), (model.blackredstart$dht$individuals$D$Estimate*10000),
       model.blackredstart$dht$individuals$D$lcl*10000, 
       model.blackredstart$dht$individuals$D$ucl*10000, add = T)

## Hill pigeon
dat1.hipi <- subset(dat1, dat1$Species == "HILL PIGEON", 
                    select = c("Sno", "Sample.Label", "Area", "Effort", "distance", "Region.Label"))  
dat1.hipi<-rbind(dat1.hipi, dat.blank)
model.hillpigeon<-(ds(dat1.hipi, truncation=90, cutpoints=bin,key="hn", transect=c("line"), order=2))
barplot(model.hillpigeon$dht$individuals$D$Estimate*10000, 
        names = model.hillpigeon$dht$individuals$D$Label, 
        las = 2, space = 0, ylim = c(0,0.3), main = "Hill Pigeon")
errbar(seq(0.5,13.5,1), (model.hillpigeon$dht$individuals$D$Estimate*10000),
       model.hillpigeon$dht$individuals$D$lcl*10000, 
       model.hillpigeon$dht$individuals$D$ucl*10000, add = T)

## House sparrow
dat1.hosp <- subset(dat1, dat1$Species == "HOUSE SPARROW", 
                    select = c("Sno", "Sample.Label", "Area", "Effort", "distance", "Region.Label"))  
dat1.hosp<-rbind(dat1.hosp, dat.blank)
model.housesparrow<-(ds(dat1.hosp, truncation=90, cutpoints=bin,key="hn", transect=c("line"), order=2))
barplot(model.housesparrow$dht$individuals$D$Estimate*10000, 
        names = model.housesparrow$dht$individuals$D$Label, 
        las = 2, space = 0, ylim = c(0,0.3), main = "House Sparrow")
errbar(seq(0.5,13.5,1), (model.housesparrow$dht$individuals$D$Estimate*10000),
       model.housesparrow$dht$individuals$D$lcl*10000, 
       model.housesparrow$dht$individuals$D$ucl*10000, add = T)

## Rock pigeon
dat1.ropi <- subset(dat1, dat1$Species == "ROCK PIGEON", 
                    select = c("Sno", "Sample.Label", "Area", "Effort", "distance", "Region.Label"))  
dat1.ropi<-rbind(dat1.ropi, dat.blank)
model.rockpigeon<-(ds(dat1.ropi, truncation=90, cutpoints=bin,key="hn", transect=c("line"), order=2))
barplot(model.rockpigeon$dht$individuals$D$Estimate*10000, 
        names = model.rockpigeon$dht$individuals$D$Label, 
        las = 2, space = 0, ylim = c(0,0.1), main = "Rock Pigeon")
errbar(seq(0.5,13.5,1), (model.rockpigeon$dht$individuals$D$Estimate*10000),
       model.rockpigeon$dht$individuals$D$lcl*10000, 
       model.rockpigeon$dht$individuals$D$ucl*10000, add = T)

## Snow pigeon
dat1.snpi <- subset(dat1, dat1$Species == "SNOW PIGEON", 
                    select = c("Sno", "Sample.Label", "Area", "Effort", "distance", "Region.Label"))  
dat1.snpi<-rbind(dat1.snpi, dat.blank)
model.snowpigeon<-(ds(dat1.snpi, truncation=90, cutpoints=bin,key="hn", transect=c("line"), order=2))
barplot(model.snowpigeon$dht$individuals$D$Estimate*10000, 
        names = model.snowpigeon$dht$individuals$D$Label, 
        las = 2, space = 0, ylim = c(0,0.2), main = "Snow pigeon")
errbar(seq(0.5,13.5,1), (model.snowpigeon$dht$individuals$D$Estimate*10000),
       model.snowpigeon$dht$individuals$D$lcl*10000, 
       model.snowpigeon$dht$individuals$D$ucl*10000, add = T)

## Himalayan snowcock
dat1.hisn <- subset(dat1, dat1$Species == "HIMALAYAN SNOWCOCK", 
                    select = c("Sno", "Sample.Label", "Area", "Effort", "distance", "Region.Label"))  
dat1.hisn<-rbind(dat1.hisn, dat.blank)
model.himalayansnowcock<-(ds(dat1.hisn, truncation=90, cutpoints=bin,key="hn", transect=c("line"), order=2))
barplot(model.himalayansnowcock$dht$individuals$D$Estimate*10000, 
        names = model.himalayansnowcock$dht$individuals$D$Label, 
        las = 2, space = 0, ylim = c(0,0.2), main = "Himalayan Snowcock")
errbar(seq(0.5,13.5,1), (model.himalayansnowcock$dht$individuals$D$Estimate*10000),
       model.himalayansnowcock$dht$individuals$D$lcl*10000, 
       model.himalayansnowcock$dht$individuals$D$ucl*10000, add = T)

## Horned lark
dat1.hola <- subset(dat1, dat1$Species == "HORNED LARK", 
                    select = c("Sno", "Sample.Label", "Area", "Effort", "distance", "Region.Label"))  
dat1.hola<-rbind(dat1.hola, dat.blank)
model.hornedlark<-(ds(dat1.hola, truncation=90, cutpoints=bin,key="hn", transect=c("line"), order=2))
barplot(model.hornedlark$dht$individuals$D$Estimate*10000, 
        names = model.hornedlark$dht$individuals$D$Label, 
        las = 2, space = 0, ylim = c(0,2), main = "Horned lark")
errbar(seq(0.5,13.5,1), (model.hornedlark$dht$individuals$D$Estimate*10000),
       model.hornedlark$dht$individuals$D$lcl*10000, 
       model.hornedlark$dht$individuals$D$ucl*10000, add = T)

## Desert wheater
dat1.dewh <- subset(dat1, dat1$Species == "DESERT WHEATEAR", 
                    select = c("Sno", "Sample.Label", "Area", "Effort", "distance", "Region.Label"))  
dat1.dewh<-rbind(dat1.dewh, dat.blank)
model.desertwheatear<-(ds(dat1.dewh, truncation=90, cutpoints=bin,key="hn", transect=c("line"), order=2))
barplot(model.desertwheatear$dht$individuals$D$Estimate*10000, 
        names = model.desertwheatear$dht$individuals$D$Label, 
        las = 2, space = 0, ylim = c(0,0.5), main = "Desert Wheatear")
errbar(seq(0.5,13.5,1), (model.desertwheatear$dht$individuals$D$Estimate*10000),
       model.desertwheatear$dht$individuals$D$lcl*10000, 
       model.desertwheatear$dht$individuals$D$ucl*10000, add = T)

## Great Rosefinch
dat1.grro <- subset(dat1, dat1$Species == "GREAT ROSEFINCH", 
                    select = c("Sno", "Sample.Label", "Area", "Effort", "distance", "Region.Label"))  
dat1.grro<-rbind(dat1.grro, dat.blank)
model.greatrosefinch<-(ds(dat1.grro, truncation=90, cutpoints=bin,key="hn", transect=c("line"), order=2))
barplot(model.greatrosefinch$dht$individuals$D$Estimate*10000, 
        names = model.greatrosefinch$dht$individuals$D$Label, 
        las = 2, space = 0, ylim = c(0,0.5), main = "Great Rosefinch")
errbar(seq(0.5,13.5,1), (model.greatrosefinch$dht$individuals$D$Estimate*10000),
       model.greatrosefinch$dht$individuals$D$lcl*10000, 
       model.greatrosefinch$dht$individuals$D$ucl*10000, add = T)

## Humes short-toed lark
dat1.hstl <- subset(dat1, dat1$Species == "HUMES SHORT-TOED LARK", 
                    select = c("Sno", "Sample.Label", "Area", "Effort", "distance", "Region.Label"))  
dat1.hstl<-rbind(dat1.hstl, dat.blank)
model.humeslark<-(ds(dat1.hstl, truncation=90, cutpoints=bin,key="hn", transect=c("line"), order=2))
barplot(model.humeslark$dht$individuals$D$Estimate*10000, 
        names = model.humeslark$dht$individuals$D$Label, 
        las = 2, space = 0, ylim = c(0,0.7), main = "Hume's lark")
errbar(seq(0.5,13.5,1), (model.humeslark$dht$individuals$D$Estimate*10000),
       model.humeslark$dht$individuals$D$lcl*10000, 
       model.humeslark$dht$individuals$D$ucl*10000, add = T)

## Robin accentor
dat1.roac <- subset(dat1, dat1$Species == "ROBIN ACCENTOR", 
                    select = c("Sno", "Sample.Label", "Area", "Effort", "distance", "Region.Label"))  
dat1.roac<-rbind(dat1.roac, dat.blank)
model.robinaccentor<-(ds(dat1.roac, truncation=90, cutpoints=bin,key="hn", transect=c("line"), order=2))
barplot(model.robinaccentor$dht$individuals$D$Estimate*10000, 
        names = model.robinaccentor$dht$individuals$D$Label, 
        las = 2, space = 0, ylim = c(0,0.5), main = "Robin Accentor")
errbar(seq(0.5,13.5,1), (model.robinaccentor$dht$individuals$D$Estimate*10000),
       model.robinaccentor$dht$individuals$D$lcl*10000, 
       model.robinaccentor$dht$individuals$D$ucl*10000, add = T)

## Tibetan snowfinch
dat1.tisn <- subset(dat1, dat1$Species == "TIBETAN SNOWFINCH", 
                    select = c("Sno", "Sample.Label", "Area", "Effort", "distance", "Region.Label"))  
dat1.tisn<-rbind(dat1.tisn, dat.blank)
model.tibetansnowfinch<-(ds(dat1.tisn, truncation=90, cutpoints=bin,key="hn", transect=c("line"), order=2))
barplot(model.tibetansnowfinch$dht$individuals$D$Estimate*10000, 
        names = model.tibetansnowfinch$dht$individuals$D$Label, 
        las = 2, space = 0, ylim = c(0,1), main = "Tibetan Snowfinch")
errbar(seq(0.5,13.5,1), (model.tibetansnowfinch$dht$individuals$D$Estimate*10000),
       model.tibetansnowfinch$dht$individuals$D$lcl*10000, 
       model.tibetansnowfinch$dht$individuals$D$ucl*10000, add = T)

##Brandts mountain finch
dat1.brmf <- subset(dat1, dat1$Species == "BRANDTS MOUNTAIN FINCH", 
                    select = c("Sno", "Sample.Label", "Area", "Effort", "distance", "Region.Label"))  
dat1.brmf<-rbind(dat1.brmf, dat.blank)
model.brandtsfinch<-(ds(dat1.brmf, truncation=90, cutpoints=bin,key="hn", transect=c("line"), order=2))
barplot(model.brandtsfinch$dht$individuals$D$Estimate*10000, 
        names = model.brandtsfinch$dht$individuals$D$Label, 
        las = 2, space = 0, ylim = c(0,0.5), main = "Brandt's Mountain Finch")
errbar(seq(0.5,13.5,1), (model.brandtsfinch$dht$individuals$D$Estimate*10000),
       model.brandtsfinch$dht$individuals$D$lcl*10000, 
       model.brandtsfinch$dht$individuals$D$ucl*10000, add = T)

##Plain mountain finch
dat1.plmf <- subset(dat1, dat1$Species == "PLAIN MOUNTAIN FINCH", 
                    select = c("Sno", "Sample.Label", "Area", "Effort", "distance", "Region.Label"))  
dat1.plmf<-rbind(dat1.plmf, dat.blank)
model.plainfinch<-(ds(dat1.plmf, truncation=90, cutpoints=bin,key="hn", transect=c("line"), order=2))
barplot(model.plainfinch$dht$individuals$D$Estimate*10000, 
        names = model.plainfinch$dht$individuals$D$Label, 
        las = 2, space = 0, ylim = c(0,1), main = "Plain Mountain Finch")
errbar(seq(0.5,13.5,1), (model.plainfinch$dht$individuals$D$Estimate*10000),
       model.plainfinch$dht$individuals$D$lcl*10000, 
       model.plainfinch$dht$individuals$D$ucl*10000, add = T)

##Rock bunting
dat1.robu <- subset(dat1, dat1$Species == "ROCK BUNTING", 
                    select = c("Sno", "Sample.Label", "Area", "Effort", "distance", "Region.Label"))  
dat1.robu<-rbind(dat1.robu, dat.blank)
model.rockbunting<-(ds(dat1.robu, truncation=90, cutpoints=bin,key="hn", transect=c("line"), order=2))
barplot(model.rockbunting$dht$individuals$D$Estimate*10000, 
        names = model.rockbunting$dht$individuals$D$Label, 
        las = 2, space = 0, ylim = c(0,0.5), main = "Rock bunting")
errbar(seq(0.5,13.5,1), (model.rockbunting$dht$individuals$D$Estimate*10000),
       model.rockbunting$dht$individuals$D$lcl*10000, 
       model.rockbunting$dht$individuals$D$ucl*10000, add = T)

##Sulphur bellied warbler
dat1.suwa <- subset(dat1, dat1$Species == "SULPHUR-BELLIED WARBLER" | 
                      dat1$Species == "TICKELLS LEAF WARBLER" |
                      dat1$Species == "WARBLER", 
                    select = c("Sno", "Sample.Label", "Area", "Effort", "distance", "Region.Label"))  
dat1.suwa<-rbind(dat1.suwa, dat.blank)
model.sulphurwarbler<-(ds(dat1.suwa, truncation=90, cutpoints=bin,key="hn", transect=c("line"), order=2))
barplot(model.sulphurwarbler$dht$individuals$D$Estimate*10000, 
        names = model.sulphurwarbler$dht$individuals$D$Label, 
        las = 2, space = 0, ylim = c(0,0.8), main = "Phyloscopus warblers")
errbar(seq(0.5,13.5,1), (model.sulphurwarbler$dht$individuals$D$Estimate*10000),
       model.sulphurwarbler$dht$individuals$D$lcl*10000, 
       model.sulphurwarbler$dht$individuals$D$ucl*10000, add = T)

##Red billed chough
dat1.rebc <- subset(dat1, dat1$Species == "RED-BILLED CHOUGH", 
                    select = c("Sno", "Sample.Label", "Area", "Effort", "distance", "Region.Label"))  
dat1.rebc<-rbind(dat1.rebc, dat.blank)
model.redchough<-(ds(dat1.rebc, truncation=90, cutpoints=bin,key="hn", transect=c("line"), order=2))
barplot(model.redchough$dht$individuals$D$Estimate*10000, 
        names = model.redchough$dht$individuals$D$Label, 
        las = 2, space = 0, ylim = c(0,0.2), main = "Red-billed Chough")
errbar(seq(0.5,13.5,1), (model.redchough$dht$individuals$D$Estimate*10000),
       model.redchough$dht$individuals$D$lcl*10000, 
       model.redchough$dht$individuals$D$ucl*10000, add = T)

##Yellow billed chough
dat1.yebc <- subset(dat1, dat1$Species == "YELLOW-BILLED CHOUGH", 
                    select = c("Sno", "Sample.Label", "Area", "Effort", "distance", "Region.Label"))  
dat1.yebc<-rbind(dat1.yebc, dat.blank)
model.yellowchough<-(ds(dat1.yebc, truncation=90, cutpoints=bin,key="hn", transect=c("line"), order=2))
barplot(model.yellowchough$dht$individuals$D$Estimate*10000, 
        names = model.yellowchough$dht$individuals$D$Label, 
        las = 2, space = 0, ylim = c(0,0.4), main = "Yellow-billed chough")
errbar(seq(0.5,13.5,1), (model.yellowchough$dht$individuals$D$Estimate*10000),
       model.yellowchough$dht$individuals$D$lcl*10000, 
       model.yellowchough$dht$individuals$D$ucl*10000, add = T)

##Common rosefinch
dat1.coro <- subset(dat1, dat1$Species == "COMMON ROSEFINCH", 
                    select = c("Sno", "Sample.Label", "Area", "Effort", "distance", "Region.Label"))  
dat1.coro<-rbind(dat1.coro, dat.blank)
model.commonrosefinch<-(ds(dat1.coro, truncation=90, cutpoints=bin,key="hn", transect=c("line"), order=2))
barplot(model.commonrosefinch$dht$individuals$D$Estimate*10000, 
        names = model.commonrosefinch$dht$individuals$D$Label, 
        las = 2, space = 0, ylim = c(0,0.15), main = "Common Rosefinch")
errbar(seq(0.5,13.5,1), (model.commonrosefinch$dht$individuals$D$Estimate*10000),
       model.commonrosefinch$dht$individuals$D$lcl*10000, 
       model.commonrosefinch$dht$individuals$D$ucl*10000, add = T)

##Wagtails
dat1.wata <- subset(dat1, dat1$Species == "GRAY WAGTAIL" |
                      dat1$Species == "CITRINE WAGTAIL", 
                    select = c("Sno", "Sample.Label", "Area", "Effort", "distance", "Region.Label"))  
dat1.wata<-rbind(dat1.wata, dat.blank)
model.wagtail<-(ds(dat1.wata, truncation=90, cutpoints=bin,key="hn", transect=c("line"), order=2))
barplot(model.wagtail$dht$individuals$D$Estimate*10000, 
        names = model.wagtail$dht$individuals$D$Label, 
        las = 2, space = 0, ylim = c(0,0.15), main = "Wagtails")
errbar(seq(0.5,13.5,1), (model.wagtail$dht$individuals$D$Estimate*10000),
       model.wagtail$dht$individuals$D$lcl*10000, 
       model.wagtail$dht$individuals$D$ucl*10000, add = T)

##Hoopoe
dat1.coho <- subset(dat1, dat1$Species == "COMMON HOOPOE", 
                    select = c("Sno", "Sample.Label", "Area", "Effort", "distance", "Region.Label"))  
dat1.coho<-rbind(dat1.coho, dat.blank)
model.commonhoopoe<-(ds(dat1.coho, truncation=90, cutpoints=bin,key="hn", transect=c("line"), order=2))
barplot(model.commonhoopoe$dht$individuals$D$Estimate*10000, 
        names = model.commonhoopoe$dht$individuals$D$Label, 
        las = 2, space = 0, ylim = c(0,0.15), main = "Common Hoopoe")
errbar(seq(0.5,13.5,1), (model.commonhoopoe$dht$individuals$D$Estimate*10000),
       model.commonhoopoe$dht$individuals$D$lcl*10000, 
       model.commonhoopoe$dht$individuals$D$ucl*10000, add = T)

## Testing the effect of adding the black transects to the data
summary(ds(dat1, truncation=90, cutpoints=bin,key="hn", transect=c("line"), order=2))
dat.test<-subset(dat1, select = c("Sno", "Sample.Label", "Area", "Effort", "distance", "Region.Label"))
dat.test<-rbind(dat.test, dat.blank)
summary(ds(dat.test, truncation=90, cutpoints=bin,key="hn", transect=c("line"), order=2))
