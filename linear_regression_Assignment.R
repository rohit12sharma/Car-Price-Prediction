library(plyr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(MASS)
library(car)
# reading the file 
car_price <- read.csv("CarPrice_Assignment.csv")


View(car_price)


## ------------------ Missing and Duplicated values checking -----------------

#1. check for duplicated values : there are 0
sum(duplicated(car_price))

#2. check for Missing values

# 2.1. check for na values : there are 0
sum(is.na(car_price))

#2.2 check for blank values : no missing values in any column
sapply(car_price, function(x) length(which(x == "")))

#knowing the structure of the data set
#there are 205 observations and 26 variables : 1 dependent , 25 independent
str(car_price)

## ----------------------------- EDA -----------------------------------------

#-----------------------DATA PREPARATION

#we remove column car id because it contains because it contains all unique id's
car_price <- car_price[,-1]

#getting car company names
car_price <- separate(car_price, CarName , into=c("CarBrand"), sep=" ")

#converting all to lower case
car_price$CarBrand <- tolower(car_price$CarBrand)

#correcting spelling errors and abbreviations in CarCompany
car_price$CarBrand <- gsub("maxda" , "mazda" , car_price$CarBrand)
car_price$CarBrand <- gsub("porcshce" , "porsche" , car_price$CarBrand)
car_price$CarBrand <- gsub("vw" , "volkswagen" , car_price$CarBrand)
car_price$CarBrand <- gsub("vokswagen" , "volkswagen" , car_price$CarBrand)
car_price$CarBrand <- gsub("toyouta" , "toyota" , car_price$CarBrand)

#converting CarBrand to factors
car_price$CarBrand <- as.factor(car_price$CarBrand)

str(car_price)

#converting symboling to factors
car_price$symboling <- as.factor(car_price$symboling)

#to have the average mpg of the car we take avg of citympg and highwaympg
car_price$carmpg <- (car_price$citympg + car_price$highwaympg)/2

#we then drop citympg and highwaympg
car_price <- subset(car_price, select = -c(citympg,highwaympg))

#we derive torque from rpm and hp
car_price$torque_max <- round((car_price$horsepower * 5252) / car_price$peakrpm , 2)

#we also derive bear/stroke ratio
car_price$bore_stroke <- round(car_price$boreratio/car_price$stroke , 2)

#looking at the frequency of the factor variables
summary(car_price$symboling)
#-2 -1  0  1  2  3 
#3 22 67 54 32 27 
summary(car_price$carbody)
#convertible     hardtop   hatchback       sedan       wagon 
#6                   8          70          96          25 
summary(car_price$drivewheel)
#4wd fwd rwd 
#9 120  76 
summary(car_price$aspiration)
#std turbo 
#168    37
summary(car_price$doornumber)
#four  two 
#115   90 
summary(car_price$enginelocation)
#front  rear 
#202     3 
summary(car_price$cylindernumber)
#eight   five   four    six  three twelve    two 
#5        11    159     24      1      1      4 
summary(car_price$fuelsystem)
#1bbl 2bbl 4bbl  idi  mfi mpfi spdi spfi 
#11   66    3   20    1   94    9    1 
summary(car_price$enginetype)
#dohc dohcv   l   ohc  ohcf  ohcv rotor 
#12     1    12   148    15    13     4 
summary(car_price$fueltype)
#diesel    gas 
#20        185 

#getting to know the quantiles of numerical values
quantile(car_price$wheelbase)
#0%   25%   50%   75%  100% 
#86.6  94.5  97.0 102.4 120.9 
quantile(car_price$price)
#0%   25%   50%   75%  100% 
#5118  7788 10295 16503 45400 
quantile(car_price$carmpg)
#0%  25%  50%  75% 100% 
#15.0 22.5 27.0 32.0 51.5 
quantile(car_price$torque_max)
#0%    25%    50%    75%   100% 
#49.43  76.59  95.49 126.92 275.20 
quantile(car_price$bore_stroke)
#0%  25%  50%  75% 100% 
#0.77 0.94 0.98 1.07 1.58

#car mean price : 13K
#This confirms that since the Mean ($13K) is higher than the Median ($10K)
#our sample relevant to Price is positively skewed
mean(car_price$price)

#--------------------------vISUALIZATION

#boxplot of car price
ggplot(car_price , aes(x = factor(1) , y = price)) + geom_boxplot()

#histogram of car_price
#we see that around 110 cars out of 205 is priced within $5-10K range (bin),
#40 cars fall within $10-15K, slightly under 40 cars are within the $15-20K range
#and approximately 20 cars in aggregate costs $20K and above.
ggplot(car_price , aes(price)) + geom_histogram(binwidth = 5000 , col = "blue")

#price comparison based on car brands
#this provides more information on car brands and their price range
ggplot(car_price , aes(CarBrand , price)) + geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

#price comparison based on symboling
#1 has the highest number of outliers followed by 0
#also 1 has the highest price
ggplot(car_price , aes(symboling , price)) + geom_boxplot()

#car brands vs symbolling scatterplot
#nteresting to see the brands that have cars represented in the lowest safety rating of 3 
ggplot(car_price, aes(x=CarBrand, y=symboling)) + geom_point() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 


#we already saw the count of diesel through summary that it is very low
#when compared with gas : there are many outliers in gas
ggplot(car_price , aes(fueltype , price)) + geom_boxplot()

#price comparison based on aspiration : there are many outliers in std
#highest price is in std
ggplot(car_price , aes(aspiration , price)) + geom_boxplot()

#price comparison based on doornumber : outliers in both
#doornumber two has a lower median to four but has the highest price
ggplot(car_price , aes(doornumber , price)) + geom_boxplot()

#price comparison based on carbody : from the summary we know hatchback & sedan
#have the highest count , on average hatchback is the least costliest
#hardtop has the highest price
ggplot(car_price , aes(carbody , price)) + geom_boxplot()

#price comparison based on drivewheel : fwd has the least average price
#rwd has the highest price & the highest average price and by some margin
ggplot(car_price , aes(drivewheel , price)) + geom_boxplot()

#price comparison based on doornumber : outliers in both
#doornumber two has a lower median to four but has the highest price
ggplot(car_price , aes(doornumber , price)) + geom_boxplot()

#price comparison based on enginelocation : rear has only 3 values
ggplot(car_price , aes(enginelocation , price)) + geom_boxplot()

#price comparison based on enginetype : ohcv has the highest average price
#ohc has the highest outliers
ggplot(car_price , aes(enginetype , price)) + geom_boxplot()

#price comparison based on cylindernumber : eight has the highest value
#there is a trend of increae in price from four to twelve 
ggplot(car_price , aes(cylindernumber , price)) + geom_boxplot()

#price comparison based on fuelsystem : mpfi has the highest average price
#1bbl & 2bbl have the lowest price on average
ggplot(car_price , aes(fuelsystem , price)) + geom_boxplot()


#boxplot of horsepower : 4 outliers
ggplot(car_price , aes(x = factor(1) , y = horsepower)) + geom_boxplot()

#boxplot of peakrpm : 1 outlier at top
ggplot(car_price , aes(x = factor(1) , y = peakrpm)) + geom_boxplot()

#horsepower-price scatterplot
ggplot(car_price,aes(horsepower , price)) + geom_point()

#scatterplot of hp-peakrpm : not much correlation
ggplot(car_price,aes(horsepower , peakrpm)) + geom_point()

#scatterplot of peakrpm-price 
ggplot(car_price,aes(peakrpm , price)) + geom_point()

#boxplot of enginesize : 6 outliers
ggplot(car_price , aes(x = factor(1) , y = enginesize)) + geom_boxplot()

#boxplot of curbweight : no outliers
ggplot(car_price , aes(x = factor(1) , y = curbweight)) + geom_boxplot()

#boxplot of wheelbase : 2 outliers
ggplot(car_price , aes(x = factor(1) , y = wheelbase)) + geom_boxplot()

#scatterplot of enginesize-price : 3 extreme values
ggplot(car_price,aes(enginesize , price)) + geom_point()

#scatterplot of enginesize-horsepower : highly correlated
ggplot(car_price,aes(enginesize , horsepower)) + geom_point()

#boxplot of enginesize : 2 outliers
ggplot(car_price , aes(x = factor(1) , y = wheelbase)) + geom_boxplot()

#scatterplot of wheelbase-price : random distribution
ggplot(car_price,aes(wheelbase , price)) + geom_point()

#scatterplot of compressionratio-price 
#we see that the points are grouped 
ggplot(car_price,aes(compressionratio , price)) + geom_point()

#compressionratio is given as a numeric variable but it's categorical in nature
ggplot(car_price,aes(factor(round(car_price$compressionratio)) , price)) + geom_point()

#boxplot of carmpg : 3 outliers
ggplot(car_price , aes(x = factor(1) , y = carmpg)) + geom_boxplot()

#scatterplot of carmpg-price : negative relation
#we can see the three outliers at the right hand corner
#The scatterplot shows that the more expensive cars do not get good mileage
ggplot(car_price,aes(carmpg , price)) + geom_point()

#boxplot of carlength : 1 outlier
ggplot(car_price , aes(x = factor(1) , y = carlength)) + geom_boxplot()

#scatterplot of carlength-price : positive relation
ggplot(car_price,aes(carlength , price)) + geom_point()

#boxplot of carwidth : 3 outlier
ggplot(car_price , aes(x = factor(1) , y = carwidth)) + geom_boxplot()

#scatterplot of carwidth-price : positive relation
ggplot(car_price,aes(carwidth , price)) + geom_point()

#boxplot of carheight : no outlier
ggplot(car_price , aes(x = factor(1) , y = carheight)) + geom_boxplot()

#scatterplot of carheight-price 
#prices more than 30k will influence the fitted line
ggplot(car_price,aes(carheight , price)) + geom_point()

#boxplot of bore_stroke : few outlier
ggplot(car_price , aes(x = factor(1) , y = bore_stroke)) + geom_boxplot()

#scatterplot of bore_stroke-price :
#outliers on both the sides will influence the decision
ggplot(car_price,aes(bore_stroke , price)) + geom_point()

#boxplot of stroke : outliers up and down , more down
ggplot(car_price , aes(x = factor(1) , y = stroke)) + geom_boxplot()

#scatterplot of stroke-price : lots of outliers
ggplot(car_price,aes(stroke , price)) + geom_point()

#boxplot of boreratio : 0 outlier
ggplot(car_price , aes(x = factor(1) , y = boreratio)) + geom_boxplot()

#scatterplot of boreratio-price : positive relation 
#couple of outliers in the begining which might influence
ggplot(car_price,aes(boreratio , price)) + geom_point()

#boxplot of torque_max : 3 outlier
ggplot(car_price , aes(x = factor(1) , y = torque_max)) + geom_boxplot()

#scatterplot of torque_max-price : positive relation
#couple of outliers which can influence
ggplot(car_price,aes(torque_max , price)) + geom_point()


#boxplot of curbweight : 0 outlier
ggplot(car_price , aes(x = factor(1) , y = curbweight)) + geom_boxplot()

#scatterplot of curbweight-price : positive relation
ggplot(car_price,aes(curbweight , price)) + geom_point()

#-------------------DATA PREPARATION FOR MODELLING----------------------

#we see that fueltype diesel is only found in idi fuelsystem : Perfect collinearity
#mfi & spfi hast just 1 level each in gas fueltype , 0 in diesel
#so we will need to remove 1 of them
table(car_price$fuelsystem,car_price$fueltype)

#we bin enginetype into ohc and non ohc : there are 3 main types of engine
#ohc , ohv and l
levels(car_price$enginetype) <- c('ohc', 'non_ohc' , 'non_ohc' , 'ohc' , 'ohc' , 'non_ohc' , 'non_ohc' )


#binning symbolling variable
levels(car_price$symboling)
levels(car_price$symboling)[1:2] <- "safe"
levels(car_price$symboling)[2] <- "ok"
levels(car_price$symboling)[3:5] <- "risky"

#binning cylindernumber variable
#if 0< number <= 4 : <=Four , 4+ : >Four
levels(car_price$cylindernumber) <- c('>Four', '>Four' , '<=Four' , '>Four' , '<=Four' , '>Four' , '<=Four' )

summary(factor(round(car_price$compressionratio)))
#7   8   9  10  12  21  22  23 
#7  44 111  22   1   5   9   6 

#binning compressionratio and making it as factor
car_price$compressionratio <- as.factor(round(car_price$compressionratio))
levels(car_price$compressionratio) <- c('low', 'low' , 'moderate' , 'moderate' , 'moderate' , 'high' , 'high' ,'high' )

#we bin fuel system based on their type : bbl , multi-point ,single-point , direct injection
levels(car_price$fuelsystem) <- c('bbl', 'bbl' , 'bbl' , 'idi' , 'mpi' , 'mpi' , 'spi' , 'spi' )



#--------- convert factors with 2 levels to numerical variables-----------

#we convert gas as 1 and diesel as 0 in fueltype variable
levels(car_price$fueltype)<-c(0,1)
car_price$fueltype <- as.numeric(levels(car_price$fueltype))[car_price$fueltype]

#we convert std as 1 and turbo as 0 in aspiration variable
levels(car_price$aspiration)<-c(1,0)
car_price$aspiration <- as.numeric(levels(car_price$aspiration))[car_price$aspiration]

#we convert four as 1 and two as 0 in doornumber variable
levels(car_price$doornumber)<-c(1,0)
car_price$doornumber <- as.numeric(levels(car_price$doornumber))[car_price$doornumber]

car_price$CarBrand[car_price$enginelocation == "rear"]
#[1] porsche porsche porsche
#we see only 3 porsche cars have engine at the rear and all others are at front
#this variable won't give us any insights and wouldn't be useful in the model
#so we drop this variable

car_price <-  subset(car_price, select = -enginelocation)

#we convert <=Four as 1 and >Four as 0
levels(car_price$cylindernumber) <- c(0,1)
car_price$cylindernumber <- as.numeric(levels(car_price$cylindernumber))[car_price$cylindernumber]

#enginetype of ohc is given value 1 and non-ohc is given 0
levels(car_price$enginetype) <- c(1,0)
car_price$enginetype <- as.numeric(levels(car_price$enginetype))[car_price$enginetype]



#------------ convert factors with more than 2 levels to numerical variables-----------

#we use model.matrix to create dummy variables
#in| case of a factor with n levels, n-1 dummy variables are required
#in the end we combine the dummy variable to our daat frame using cbind
#the original independent variable is removed and dummy is used instead

#creating dummy for symbolling
dummy <- model.matrix(~symboling , data=car_price)
dummy<- dummy[,-1]
car_price <- cbind(car_price[,-1] , dummy)

#creating dummy for carbody
dummy <- model.matrix(~carbody , data=car_price)
dummy<- dummy[,-1]
car_price <- cbind(car_price[,-5] , dummy)

#creating dummy for drivewheel
dummy <- model.matrix(~drivewheel , data=car_price)
dummy<- dummy[,-1]
car_price <- cbind(car_price[,-5] , dummy)


#creating dummy for CarBrand
dummy <- model.matrix(~CarBrand , data=car_price)
dummy<- dummy[,-1]
car_price <- cbind(car_price[,-1] , dummy)

#creating dummy for fuelsystem
dummy <- model.matrix(~fuelsystem , data=car_price)
dummy<- dummy[,-1]
car_price <- cbind(car_price[,-12] , dummy)

#creating dummy for compressionratio
dummy <- model.matrix(~compressionratio , data=car_price)
dummy<- dummy[,-1]
car_price <- cbind(car_price[,-14] , dummy)

#we have 52 independent variables and 1 dependent


#----------------------------------Outlier Treatment---------------------

#whenever there is a extreme jump in the value we cap the outlier values

#checking for horsepower
#the highest jump comes at 99%
quantile(car_price$horsepower , seq(0,1,0.01))
car_price$horsepower[which(car_price$horsepower > 207.00)]<-207.00

#checking for wheelbase - the only jump comes above 99% 
#so we cap it
quantile(car_price$wheelbase , seq(0,1,0.01))
car_price$wheelbase[which(car_price$wheelbase > 115.544)]<-115.544


#checking for carlength - here the jump is at the begining
quantile(car_price$carlength , seq(0,1,0.01))
car_price$carlength[which(car_price$carlength < 155.000)]<-155.000

#checking for carwidth - here too the jump is at the begining
quantile(car_price$carwidth , seq(0,1,0.01))
car_price$carwidth[which(car_price$carwidth < 63.600)]<-63.600

#checking for carheight - no outlier: 
quantile(car_price$carheight , seq(0,1,0.01))

#checking for curbweight : outlier at the start
quantile(car_price$curbweight , seq(0,1,0.01))
car_price$curbweight[which(car_price$curbweight < 1819.72)]<-1819.72

#checking for price : there is a steady jump at regular percentiles from 90%
#but the highest is from 98% and above , since it is also the dependent variable
#we don't want to lose too much 
quantile(car_price$price , seq(0,1,0.01))
car_price$price[which(car_price$price > 36000)]<-36000

#checking for enginesize - it jumps after 209 and also at the star
quantile(car_price$enginesize , seq(0,1,0.01))
car_price$enginesize[which(car_price$enginesize > 209)]<-209
car_price$enginesize[which(car_price$enginesize < 90)]<-90

#checking boreratio : jump at the start
quantile(car_price$boreratio , seq(0,1,0.01))
car_price$boreratio[which(car_price$boreratio < 2.9000)]<-2.9000

#checking stroke : outliers at both ends
quantile(car_price$stroke , seq(0,1,0.01))
car_price$stroke[which(car_price$stroke < 2.6400)]<-2.6400
car_price$stroke[which(car_price$stroke > 3.9000)]<-3.9000

#checking for peakrpm : jump at 99%
quantile(car_price$peakrpm , seq(0,1,0.01))
car_price$peakrpm[which(car_price$peakrpm > 6000)]<-6000

#checking for torque : jump at 99%
quantile(car_price$torque_max , seq(0,1,0.01))
car_price$torque_max[which(car_price$torque_max > 214.7500)]<-214.7500

#checking for carmpg : jump at 98%
quantile(car_price$carmpg , seq(0,1,0.01))
car_price$carmpg[which(car_price$carmpg > 42.42)]<-42.42

#checking for carmpg : jump at the end
quantile(car_price$bore_stroke , seq(0,1,0.01))
car_price$bore_stroke[which(car_price$bore_stroke > 1.3700)]<-1.3700


#---------------------Modelling-----------------------------------

#perfect collinearity
#we know that diesel and idi has a direct correlation
#so 1 of the column should be removed , we also find perfect collinearity in 
#compressionratiohigh and fuelsystemidi
cor(car_price$compressionratiohigh,car_price$fuelsystemidi)

#we take a subset of car_price for modelling
#we remove compressionratiohigh and fuelsystemidi
car_price_prediction <- car_price[, -c(49,53)]

set.seed(1)
trainindices= sample(1:nrow(car_price_prediction), 0.7*nrow(car_price_prediction))
train = car_price_prediction[trainindices,]
test = car_price_prediction[-trainindices,]

model_1 <-lm(price~.,data=train)
summary(model_1)

#we use stepAIC to remove highly insignificant variables
step <- stepAIC(model_1, direction="both")

step

#we use the model from step for the next model
model_2 <- lm(formula = price ~ fueltype + wheelbase + carlength + carheight + 
                curbweight + cylindernumber + enginesize + boreratio + horsepower + 
                peakrpm + carmpg + torque_max + carbodyhatchback + carbodysedan + 
                carbodywagon + CarBrandbmw + CarBrandbuick + CarBranddodge + 
                CarBrandhonda + CarBrandisuzu + CarBrandmitsubishi + CarBrandnissan + 
                CarBrandpeugeot + CarBrandplymouth + CarBrandporsche + CarBrandsaab + 
                CarBrandtoyota + CarBrandvolkswagen + fuelsystemmpi + compressionratiomoderate, 
              data = train)
summary(model_2)

vif(model_2)

#peakrpm is removed because it is insignificant 
model_3 <- lm(formula = price ~ fueltype + wheelbase + carlength + carheight + 
                curbweight + cylindernumber + enginesize + boreratio + horsepower + 
                carmpg + torque_max + carbodyhatchback + carbodysedan + 
                carbodywagon + CarBrandbmw + CarBrandbuick + CarBranddodge + 
                CarBrandhonda + CarBrandisuzu + CarBrandmitsubishi + CarBrandnissan + 
                CarBrandpeugeot + CarBrandplymouth + CarBrandporsche + CarBrandsaab + 
                CarBrandtoyota + CarBrandvolkswagen + fuelsystemmpi + compressionratiomoderate, 
              data = train)
summary(model_3)

vif(model_3)

#carmpg is removed because it is insignificant 
model_4 <- lm(formula = price ~ fueltype + wheelbase + carlength + carheight + 
                curbweight + cylindernumber + enginesize + boreratio + horsepower + 
                torque_max + carbodyhatchback + carbodysedan + 
                carbodywagon + CarBrandbmw + CarBrandbuick + CarBranddodge + 
                CarBrandhonda + CarBrandisuzu + CarBrandmitsubishi + CarBrandnissan + 
                CarBrandpeugeot + CarBrandplymouth + CarBrandporsche + CarBrandsaab + 
                CarBrandtoyota + CarBrandvolkswagen + fuelsystemmpi + compressionratiomoderate, 
              data = train)
summary(model_4)

vif(model_4)


#CarBrandsaab is removed because it is insignificant 
model_5 <- lm(formula = price ~ fueltype + wheelbase + carlength + carheight + 
                curbweight + cylindernumber + enginesize + boreratio + horsepower + 
                torque_max + carbodyhatchback + carbodysedan + 
                carbodywagon + CarBrandbmw + CarBrandbuick + CarBranddodge + 
                CarBrandhonda + CarBrandisuzu + CarBrandmitsubishi + CarBrandnissan + 
                CarBrandpeugeot + CarBrandplymouth + CarBrandporsche +
                CarBrandtoyota + CarBrandvolkswagen + fuelsystemmpi + compressionratiomoderate, 
              data = train)
summary(model_5)

vif(model_5)


#compressionratiomoderate is removed because it is insignificant 
model_6 <- lm(formula = price ~ fueltype + wheelbase + carlength + carheight + 
                curbweight + cylindernumber + enginesize + boreratio + horsepower + 
                torque_max + carbodyhatchback + carbodysedan + 
                carbodywagon + CarBrandbmw + CarBrandbuick + CarBranddodge + 
                CarBrandhonda + CarBrandisuzu + CarBrandmitsubishi + CarBrandnissan + 
                CarBrandpeugeot + CarBrandplymouth + CarBrandporsche +
                CarBrandtoyota + CarBrandvolkswagen + fuelsystemmpi , 
              data = train)
summary(model_6)

vif(model_6)


#fueltype is removed because it is insignificant 
model_7 <- lm(formula = price ~  wheelbase + carlength + carheight + 
                curbweight + cylindernumber + enginesize + boreratio + horsepower + 
                torque_max + carbodyhatchback + carbodysedan + 
                carbodywagon + CarBrandbmw + CarBrandbuick + CarBranddodge + 
                CarBrandhonda + CarBrandisuzu + CarBrandmitsubishi + CarBrandnissan + 
                CarBrandpeugeot + CarBrandplymouth + CarBrandporsche +
                CarBrandtoyota + CarBrandvolkswagen + fuelsystemmpi , 
              data = train)
summary(model_7)

vif(model_7)

#torque_max is removed because it is insignificant and has a very high vif
model_8 <- lm(formula = price ~  wheelbase + carlength + carheight + 
                curbweight + cylindernumber + enginesize + boreratio + horsepower + 
                carbodyhatchback + carbodysedan + 
                carbodywagon + CarBrandbmw + CarBrandbuick + CarBranddodge + 
                CarBrandhonda + CarBrandisuzu + CarBrandmitsubishi + CarBrandnissan + 
                CarBrandpeugeot + CarBrandplymouth + CarBrandporsche +
                CarBrandtoyota + CarBrandvolkswagen + fuelsystemmpi , 
              data = train)
summary(model_8)

vif(model_8)


#fuelsystemmpi is removed because it is the least significant 
model_9 <- lm(formula = price ~  wheelbase + carlength + carheight + 
                curbweight + cylindernumber + enginesize + boreratio + horsepower + 
                carbodyhatchback + carbodysedan + 
                carbodywagon + CarBrandbmw + CarBrandbuick + CarBranddodge + 
                CarBrandhonda + CarBrandisuzu + CarBrandmitsubishi + CarBrandnissan + 
                CarBrandpeugeot + CarBrandplymouth + CarBrandporsche +
                CarBrandtoyota + CarBrandvolkswagen  , 
              data = train)
summary(model_9)

vif(model_9)


#CarBrandisuzu is removed because it is the least significant 
model_10 <- lm(formula = price ~  wheelbase + carlength + carheight + 
                curbweight + cylindernumber + enginesize + boreratio + horsepower + 
                carbodyhatchback + carbodysedan + 
                carbodywagon + CarBrandbmw + CarBrandbuick + CarBranddodge + 
                CarBrandhonda + CarBrandmitsubishi + CarBrandnissan + 
                CarBrandpeugeot + CarBrandplymouth + CarBrandporsche +
                CarBrandtoyota + CarBrandvolkswagen  , 
              data = train)
summary(model_10)

vif(model_10)


#carlength is removed because it is the least significant 
#enginesize and curbweight have the highest vif
model_11 <- lm(formula = price ~  wheelbase + carheight + 
                 curbweight + cylindernumber + enginesize + boreratio + horsepower + 
                 carbodyhatchback + carbodysedan + 
                 carbodywagon + CarBrandbmw + CarBrandbuick + CarBranddodge + 
                 CarBrandhonda + CarBrandmitsubishi + CarBrandnissan + 
                 CarBrandpeugeot + CarBrandplymouth + CarBrandporsche +
                 CarBrandtoyota + CarBrandvolkswagen  , 
               data = train)
summary(model_11)

vif(model_11)


#enginesize is removed because it is the least significant and have a high vif
model_12 <- lm(formula = price ~  wheelbase + carheight + 
                 curbweight + cylindernumber +  boreratio + horsepower + 
                 carbodyhatchback + carbodysedan + 
                 carbodywagon + CarBrandbmw + CarBrandbuick + CarBranddodge + 
                 CarBrandhonda + CarBrandmitsubishi + CarBrandnissan + 
                 CarBrandpeugeot + CarBrandplymouth + CarBrandporsche +
                 CarBrandtoyota + CarBrandvolkswagen  , 
               data = train)
summary(model_12)

vif(model_12)

#cylindernumber is removed because it is insignificant 
#curbweight has the highest vif
model_13 <- lm(formula = price ~  wheelbase + carheight + 
                 curbweight + boreratio + horsepower + 
                 carbodyhatchback + carbodysedan + 
                 carbodywagon + CarBrandbmw + CarBrandbuick + CarBranddodge + 
                 CarBrandhonda + CarBrandmitsubishi + CarBrandnissan + 
                 CarBrandpeugeot + CarBrandplymouth + CarBrandporsche +
                 CarBrandtoyota + CarBrandvolkswagen  , 
               data = train)
summary(model_13)

vif(model_13)


#CarBrandhonda is removed because it is insignificant 
model_14 <- lm(formula = price ~  wheelbase + carheight + 
                 curbweight + boreratio + horsepower + 
                 carbodyhatchback + carbodysedan + 
                 carbodywagon + CarBrandbmw + CarBrandbuick + CarBranddodge + 
                 CarBrandmitsubishi + CarBrandnissan + 
                 CarBrandpeugeot + CarBrandplymouth + CarBrandporsche +
                 CarBrandtoyota + CarBrandvolkswagen  , 
               data = train)
summary(model_14)

vif(model_14)


#CarBrandvolkswagen is removed because it is insignificant 
model_15 <- lm(formula = price ~  wheelbase + carheight + 
                 curbweight + boreratio + horsepower + 
                 carbodyhatchback + carbodysedan + 
                 carbodywagon + CarBrandbmw + CarBrandbuick + CarBranddodge + 
                 CarBrandmitsubishi + CarBrandnissan + 
                 CarBrandpeugeot + CarBrandplymouth + CarBrandporsche +
                 CarBrandtoyota  , 
               data = train)
summary(model_15)

vif(model_15)


#CarBranddodge is removed because it is insignificant 
model_16 <- lm(formula = price ~  wheelbase + carheight + 
                 curbweight + boreratio + horsepower + 
                 carbodyhatchback + carbodysedan + 
                 carbodywagon + CarBrandbmw + CarBrandbuick +  
                 CarBrandmitsubishi + CarBrandnissan + 
                 CarBrandpeugeot + CarBrandplymouth + CarBrandporsche +
                 CarBrandtoyota  , 
               data = train)
summary(model_16)

vif(model_16)


#CarBrandplymouth is removed because it is insignificant 
model_17 <- lm(formula = price ~  wheelbase + carheight + 
                 curbweight + boreratio + horsepower + 
                 carbodyhatchback + carbodysedan + 
                 carbodywagon + CarBrandbmw + CarBrandbuick +  
                 CarBrandmitsubishi + CarBrandnissan + 
                 CarBrandpeugeot + CarBrandporsche +
                 CarBrandtoyota  , 
               data = train)
summary(model_17)

vif(model_17)


#CarBrandnissan is removed because it is insignificant 
model_18 <- lm(formula = price ~  wheelbase + carheight + 
                 curbweight + boreratio + horsepower + 
                 carbodyhatchback + carbodysedan + 
                 carbodywagon + CarBrandbmw + CarBrandbuick +  
                 CarBrandmitsubishi + CarBrandpeugeot + CarBrandporsche +
                 CarBrandtoyota  , 
               data = train)
summary(model_18)

vif(model_18)

#CarBrandtoyota is removed because it is the least significant 
model_19 <- lm(formula = price ~  wheelbase + carheight + 
                 curbweight + boreratio + horsepower + 
                 carbodyhatchback + carbodysedan + 
                 carbodywagon + CarBrandbmw + CarBrandbuick +  
                 CarBrandmitsubishi + CarBrandpeugeot + CarBrandporsche 
                  , 
               data = train)
summary(model_19)

vif(model_19)


#curbweight has been highly correlated since the satrt
#so it has been been removed
model_20 <- lm(formula = price ~  wheelbase + carheight + 
                 boreratio + horsepower + 
                 carbodyhatchback + carbodysedan + 
                 carbodywagon + CarBrandbmw + CarBrandbuick +  
                 CarBrandmitsubishi + CarBrandpeugeot + CarBrandporsche 
               , 
               data = train)
summary(model_20)

vif(model_20)


#CarBrandpeugeot is removed because it is insignificant    
model_21 <- lm(formula = price ~  wheelbase + carheight + 
                 boreratio + horsepower + 
                 carbodyhatchback + carbodysedan + 
                 carbodywagon + CarBrandbmw + CarBrandbuick +  
                 CarBrandmitsubishi + CarBrandporsche 
               , 
               data = train)
summary(model_21)

vif(model_21)


#boreratio is removed because it is insignificant   
model_22 <- lm(formula = price ~  wheelbase + carheight + 
                 horsepower + 
                 carbodyhatchback + carbodysedan + 
                 carbodywagon + CarBrandbmw + CarBrandbuick +  
                 CarBrandmitsubishi + CarBrandporsche 
               , 
               data = train)
summary(model_22)

vif(model_22)


#carbodysedan is removed because it is the least significant
#and has a high vif
model_23 <- lm(formula = price ~  wheelbase + carheight + 
                 horsepower + 
                 carbodyhatchback + carbodywagon + CarBrandbmw + CarBrandbuick +  
                 CarBrandmitsubishi + CarBrandporsche 
               , 
               data = train)
summary(model_23)

vif(model_23)


#carbodywagon is removed because it  turns insignificant 
model_24 <- lm(formula = price ~  wheelbase + carheight + 
                 horsepower + 
                 carbodyhatchback + CarBrandbmw + CarBrandbuick +  
                 CarBrandmitsubishi + CarBrandporsche 
               , 
               data = train)
summary(model_24)

vif(model_24)


#carbodyhatchback is removed because it is the least significant 
model_25 <- lm(formula = price ~  wheelbase + carheight + 
                 horsepower + 
                 CarBrandbmw + CarBrandbuick +  
                 CarBrandmitsubishi + CarBrandporsche 
               , 
               data = train)
summary(model_25)

vif(model_25)

#carheight is removed because it is the least significant
model_26 <- lm(formula = price ~  wheelbase +  
                 horsepower + 
                 CarBrandbmw + CarBrandbuick +  
                 CarBrandmitsubishi + CarBrandporsche 
               , 
               data = train)
summary(model_26)

vif(model_26)


#CarBrandmitsubishi is removed because it is not highly significant 
model_27 <- lm(formula = price ~  wheelbase +  
                 horsepower + 
                 CarBrandbmw + CarBrandbuick +  
                 CarBrandporsche 
               , 
               data = train)
summary(model_27)

vif(model_27)
 

#------------------------------------------------------------------------
#Multiple R-squared:  0.8855,	Adjusted R-squared:  0.8813 
#---------------------------------------------------------------------


#so r-squared doesn't drop much and also r-adjusted square is high
#-------------------This is our finald model----------------------

#Model : Price = -36887.871 + 376.009*wb + 117.091*hp + 7181.259*bmw + 10095.492*buick + 9557.760*porsche
#hp = horsepower, wb= wheelbase , bmw = CarBrandbmw, buick = CarBrandbuick, porsche = CarBrandporsche
#so this is our final model with low vif and all variables are significant with
#Multiple R-squared:  0.9166 and Adjusted R-squared:  0.9136 

#-------------Driver Variables 
#our automobile consulting company cannot include exogenous factors such as Brand_names 
#because from business perspective u cannot say that these are the driving factors that affect the pricing of cars in the American marketing for the Chinese automobile company Geely Auto
#So Based on business understanding, we need to sideline these brand variables
#as the Chinese company are not manufacturing with the derived brand names
#Hence From business requirement perspective, they have no significance.


#So we select from the previous model two variables which can be set as driver variables
#we include wheelbase and horsepower
model1 <- lm(formula = price ~ wheelbase + 
                 horsepower , 
               data = train)

summary(model1)

vif(model1)

#highly significant and low vif : we try to find if there is another driver variable
#so we add compressionratemoderate
model2 <- lm(formula = price ~ wheelbase + 
                 horsepower + compressionratiomoderate , 
               data = train)

summary(model2)
#compressionratemoderate turns out to be insignificant 
#we also see our r-adjusted has decreased and r0squared hasn't increased much


#compressionratemoderate is dropped and carheight is added
model3 <- lm(formula = price ~ wheelbase + 
                 horsepower + carheight , 
               data = train)

summary(model3)

#carheight is dropped because it's insignificant and boreratio is added
model4 <- lm(formula = price ~ wheelbase + 
                 horsepower + boreratio , 
               data = train)

summary(model4)

#boreratio is dropped because it's insignificant and cylindernumber is added
model5 <- lm(formula = price ~ wheelbase + 
                 horsepower + cylindernumber , 
               data = train)

summary(model5)
#we see that R-squared ahs increased and adjusted R-squared too has increased
#cylindernumber is highly significant

vif(model5)
#vif is also low

#we add curbweight
model6 <- lm(formula = price ~ wheelbase + 
                 horsepower + cylindernumber + curbweight , 
               data = train)

summary(model6)

vif(model6)
#r-squared and adjusted r-squared have increased slightly
#but curbweight is highly collinear and it makes wheelbase insignificant

#so we remove curbweight and add enginesize
model7 <- lm(formula = price ~  wheelbase +
                 horsepower + cylindernumber + enginesize , 
               data = train)

summary(model7)

vif(model7)
#adjusted r-squared and R-squared have increased

#enginesize is highly collinear and it decreases significance of cylindernumber
#so enginesize is dropped and we include carlength
model8 <- lm(formula = price ~  wheelbase + carlength +
                 horsepower + cylindernumber  , 
               data = train)

summary(model8)

vif(model8)

#carlength is insignificant , we add carbodywagon
model9 <- lm(formula = price ~  wheelbase + carbodywagon +
               horsepower + cylindernumber  , 
             data = train)

summary(model9)

vif(model9)

#carbodywagon is insignificant so we drop it
model10 <- lm(formula = price ~  wheelbase + horsepower + cylindernumber, 
             data = train)

summary(model10)

vif(model10)

#after bringing back previously high significant variables from the model
#all three variables are highly significant and have no collinearity
#we conclude that these are the driving factors which affects car prices

# wheelbase , horsepower , cylindernumber




#--------------------------------------------------------------------------
#Multiple R-squared:  0.8222,	Adjusted R-squared:  0.8184 
#---------------------------------------------------------------------

#-----------------------------Model Evaluation & Assessment-----------------

#-----------------------Prediction and error testing for final model


Predict_1 <- predict(model_27,test[,-16])
test$test_price <- Predict_1

r <- cor(test$price,test$test_price)
rsquared <- cor(test$price,test$test_price)^2
rsquared
#we know that R² value for the testing data set should be close to the R² value for the training data set,
#for the model to be performing well.

#rsquared(correlation coefficient) : 0.8497695
#R² value is equal to the square of correlation between the two variables.
#so our R-squared and correlation coefficient are almost the same
#R-squared = 0.8855 , correlation coefficient = 0.8497695
test$error <-  test$price - test$test_price

#we plot the price and predicted price for test data set
#it overlaps quite nicely
ggplot(test, aes(wheelbase, price)) + geom_smooth(col = "blue") +
  scale_x_continuous(name = "wheelbaser") +
  scale_y_continuous(name = "price") +
  geom_smooth(aes(x=wheelbase, y=test_price, colour="red"))

#error test
#here we can see random distribution of errors against wheelbase
#his essentially confirms is that
#there are no variables that could have helped explain the model better
ggplot(test, aes(wheelbase, error)) + geom_point() +
  scale_x_continuous(name = "wheelbase") +
  scale_y_continuous(name = "error") +
  geom_hline(yintercept = 0) 


#------------------Predicting for the data set

#here we use our model to predict price on the actual data set

#we first take a subset of the original data set
car_subset<- car_price

#then we test our mdel in the same way we did on the test data set
car_subset$Predicted_price <- predict(model_27, car_subset[,-16])

car_subset$price_error <- car_subset$price - car_subset$Predicted_price

#we can see how perfectly the model overlaps
#thus indicating that the model is able to explain the change in car_price very well.
ggplot(car_subset, aes(wheelbase, price)) + geom_smooth(col = "yellow") +
   scale_x_continuous(name = "wheelbase") +
  scale_y_continuous(name = "price")  +
  geom_smooth(aes(x=wheelbase, y=Predicted_price, col = "red"))

#error test
#here we can see a random error pattern
#so no other variables could have helped explain the model better
ggplot(car_subset, aes(wheelbase, price_error)) + geom_point() +
  scale_x_continuous(name = "wheelbase") +
  scale_y_continuous(name = "error") +
  geom_hline(yintercept = 0) 


#-----------------------------Model Evaluation & Assessment for model10 -----------------

#------------------------Prediction and testing for the second model(Model10)

#we generated model10 to explain tthe driver variables for the chinese automobile company
#which they can use and predict car prices based on non-exogenous factors
#so lets test that model which is also a good model
Predict_2 <- predict(model10,test[,-16])
test$test_price <- Predict_2

r <- cor(test$price,test$test_price)
rsquared <- cor(test$price,test$test_price)^2
rsquared

#R-squared:  0.8222 , rsquared(correlation coefficient) : 0.7555042
#so our R-squared and rsquared(correlation coefficient) are pretty close

test$error <-  test$price - test$test_price

#so we can see there is an overlap but not as cleat as for the final model
ggplot(test, aes(wheelbase, price)) + geom_smooth(col = "blue") +
  geom_line() + scale_x_continuous(name = "wheelbase") +
  scale_y_continuous(name = "price") + geom_line(colour = "yellow" ) +
  geom_smooth(aes(x=wheelbase, y=test_price, colour="red"))

#error test
#we can see that the errors are capturing some patterns
ggplot(test, aes(wheelbase, error)) + geom_point() + 
  scale_x_continuous(name = "wheelbase") +
  scale_y_continuous(name = "error") +
  geom_hline(yintercept = 0) 


#------------------Predicting for the data set

car_subset1<- car_price

car_subset1$Predicted_price <- predict(model10, car_subset1[,-16])

car_subset1$price_error <- car_subset1$price - car_subset1$Predicted_price

#its a good model , prices overlap but it's not as nicely as the final model
ggplot(car_subset1, aes(wheelbase, price)) + geom_smooth(col = "yellow") +
  scale_x_continuous(name = "wheelbase") +
  scale_y_continuous(name = "price")  +
  geom_smooth(aes(x=wheelbase, y=Predicted_price, col = "red"))

#error test
#as we can see there are some non-random pattern
#which is not explained by this model
ggplot(car_subset1, aes(wheelbase, price_error)) + geom_point() +
  scale_x_continuous(name = "wheelbase") +
  scale_y_continuous(name = "error") +
  geom_hline(yintercept = 0) 


#-----------------Steps Performed in this Linear regression model

# first thing we do when we get the data set is to perform EDA to understand the data
#we perform various quality checks to check for missing values , NA etc.
#then we clean and prepare the data set to be fit for modelling
#we cannot have any other data type other than numeric ,
#so we convert factors, characters etc to a proper format and scaling them correctly
#we use visualisations to understand the data better , checking for outlier ..
#Outliers which have leverage and can influence the model(slope) are capped
#for factors with only two levels , we assign them (0,1) as per the output we want
#we create dummy variables for factors with levels more than 2 and bind them
#to our data set 
#we then perform modelling using lm() to regress our linear model
#In modeliing we use stepAIC function and direction is set to both to eliminate
#all insignificant variables , we then go step by step and eliminate variables
#based on their significance and correlation value which we get using vif
#when all the variables are significant we stop the model iteration and test it
#we derive variables from multiple models which fit our busines understanding
#and use them as our driver variables for future prediction
#if our model evaluates correctly we use it on our data set for prediction


#--------------------------------Interpreting the results------------

#Important variables
#1.Wheelbase
#2.horsepower
#3.CarBrandbmw
#4.CarBrandbuick
#5.CarBrandporsche

#our final model consists of these 5 variables which are highly significant
#to our model prediction of car price ,but as we know CarBrandbmw,CarBrandbuick
#and CarBrandporsche are just predictions , exogenous factors which cannot be used
#by the chinese automobile company to predict it's own cars' prices.
#so we need to draw insights from more than 1 model , thats why we modelled model10
#because it gives us factors which can "drive" the price up/down for a automobile company.

#we observe 3 important variables from model10
#1.horsepower
#2.wheelbase
#3.cylindernumber
# we also can use curbweight and enginesize for prediction but 
#since they are highly correlated we use these three as our Driver Variables
#that could be used to predict the price of the car

#1. wheelbase 2.horsepower 3.cylindernumber
