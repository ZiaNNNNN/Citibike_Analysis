url = "https://raw.githubusercontent.com/jcbonilla/BusinessAnalytics/master/BAData/JC-201709-citibike-tripdata.csv"
citibike = read.csv(url)
df = as.data.frame(citibike)

summary(df$tripduration)
var(df$tripduration)
sd(df$tripduration)
range(df$tripduration)

library(lubridate)
age <- function(dob, age.day = today(), units = "years", floor = TRUE) {
  calc.age = interval(dob, age.day) / duration(num = 1, units = units)
  if (floor) return(as.integer(floor(calc.age)))
  return(calc.age)
}
df$birth.year = as.Date(df$birth.year, "%Y")
df$age = age(df$birth.year)
summary(df$age)
var(df$age, na.rm=TRUE)
sd(df$age, na.rm=TRUE)
range(df$age, na.rm=TRUE)

df$tripduration_min = round((df$tripduration/60), digits = 2)
summary(df$tripduration_min)
var(df$tripduration_min)
sd(df$tripduration_min)
range(df$tripduration_min)

plot(df$tripduration,df$age)
cor(df$tripduration, df$age, use = "complete.obs")

library(dplyr)
count_less45 = df %>% summarize(count = sum(df$tripduration_min<=45))
count_greater45 = df %>% summarize(count = sum(df$tripduration_min>45))
price_less45 = 3
price_greater45 = 5
total_revenue = (count_less45 * price_less45) + (count_greater45 * price_greater45)
total_revenue[1,1]

hist(df$tripduration_min,
     xlim=c(0,100), breaks=10000,
     xlab="Min",
     border="blue",
     col="green",
     las=1,
     main="Histogram for Min")

var(df$tripduration_min)
sd(df$tripduration_min)

url2 = "https://raw.githubusercontent.com/jcbonilla/BusinessAnalytics/master/BAData/zagat.CSV"
zagat = read.csv(url2)
df2 = as.data.frame(zagat)
sapply(df2[,2:5], mean)
library(psych)
sapply(df2[,2:5], geometric.mean)
sapply(df2[,2:5], harmonic.mean)
library(DescTools)


sapply(df2[,2:5], var)
sapply(df2[,2:5], sd)

par(mfrow = c(2,2))
boxplot(df2$Food, xlab="Food", outline = F)
boxplot(df2$Decor, xlab="Decor", outline = F)
boxplot(df2$Service, xlab="Service", outline = F)
boxplot(df2$Price, xlab="Price", outline = F)


df2$Name <- NULL
min.max.norm <- function(x){
  (x-min(x))/(max(x)-min(x))
}

w1 = sapply(df2, min.max.norm)

first1 <- function(data)
{
  x <- c(data)
  for(i in 1:length(data))
    x[i] = data[i]/sum(data[])
  return(x)
}

df3 = apply(w1,2,first1)

first2 <- function(data)
{
  x <- c(data)
  for(i in 1:length(data)){
    if(data[i] == 0){
      x[i] = 0
    }else{
      x[i] = data[i] * log(data[i])
    }
  }
  return(x)
}

df4 = apply(df3, 2, first2)

k <- 1/log(length(df4[,1]))
d <- -k * colSums(df4)

d = 1-d

w = d/sum(d)
w
