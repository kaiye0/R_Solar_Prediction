#Final project
solar <- read.csv("SolarPrediction.csv")

#Data preprocessing
library(chron) #times and dates tranformation
solar$time <- as.numeric(times(solar$time))
solar$timesunrise <- as.numeric(times(solar$timesunrise))
solar$timesunset <- as.numeric(times(solar$timesunset))
solar$data <- gsub(paste0("12:00:00 AM",collapse = "|"),"", solar$data)
solar$data <- (as.numeric(dates(solar$data)) - as.numeric(dates('1/1/2016')))/365
#rename variables for simplicity
colnames(solar) <- c('unix', 'date', 'time', 'rad', 'temp', 'pre', 'hum', 'wd', 'ws', 'tsr', 'tss', 'tday')
#separate daytime data
solar$ifday <- (solar$time - solar$tsr)*(solar$tss - solar$time)
solarday <- solar[solar$ifday>0,]

#pair test
rows <- nrow(solarday)
rand <- floor(0.05 * rows) 
rand.dat <- solarday[sample(rows), ] 
solarpair <- rand.dat[1:rand, ] 
solarpair$unix <-NULL
solarpair$ifday <-NULL
pairs(solarpair, gap=0.5)

#anomaly of wind speed


#Linear regression model
solarday.lm <- lm(formula = rad ~ date + time + temp + pre + hum + wd + ws + tsr + tday, data = solarday)
#backward elimination process
solarday.lm <- update(solarday.lm, .~. - tday, data = solarday)

#Result Analysis--RMSE
test_delta <- function(ds, f) {
  rows <- nrow(ds) 
  upper_bound <- floor(f * rows) 
  permuted.dat <- ds[sample(rows), ] 
  train.dat <- permuted.dat[1:upper_bound, ] 
  test.dat <- permuted.dat[(upper_bound+1):rows, ]
  N <- nrow(test.dat)
  new.lm <- lm(formula = rad ~ date + time + temp + pre + hum + wd + ws + tsr + tday, data = train.dat)
  predicted.dat <- predict(new.lm, newdata=test.dat)
  delta <- predicted.dat - test.dat$rad
  RMSE <- sqrt(sum(delta*delta)/N)
  return(RMSE)
}

D_f <- c()
for (k in 1:100) 
{
  D_f <- c(D_f, test_delta(solarday, 0.6)) 
}
mean(D_f)

#Segmentation model
solarday$noon <- (solarday$tsr + solarday$tss)/2
solarday$ifpeak <- (solarday$time - solarday$noon + 3/24)*(solarday$noon + 3/24 - solarday$time)
solarpeak <- solarday[solarday$ifpeak>0,]
solarvalley <- solarday[solarday$ifpeak<=0,]
solarpeak.lm <- lm(formula = rad ~ date + time + temp + pre + hum + wd + ws + tsr + tday, data = solarpeak)
solarvalley.lm <- lm(formula = rad ~ date + time + temp + pre + hum + wd + ws + tsr + tday, data = solarvalley)
solarvalley.lm <- update(solarvalley.lm, .~. - date, data = solarvalley)

solaragg <- aggregate(rad ~ time, data=solarday, mean, na.rm=TRUE)
plot(solaragg$time,solaragg$rad)
solarday$ifwork <- (solarday$time - solarday$tsr - 1.5/24)*(solarday$tss -1.5/24 - solarday$time)
solarwork <- solarday[solarday$ifwork > 0,]
solarwork.lm <- lm(formula = rad ~ date + time + temp + pre + hum + wd + ws + tsr + tday, data = solarwork)
solarwork.lm <- update(solarwork.lm, .~. - hum, data = solarwork)

