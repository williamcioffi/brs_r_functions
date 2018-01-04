# prep some data for crawl
library(crawl)
library(rgdal)


arg <- read.table("~/Desktop/CURRENT/ZcTag042/MASTER-argos.csv", header = TRUE, sep = ',')
head(arg)
arg <- arg[arg$LocationQuality %in% c("3", "2", "1", "0", "A", "B"), ]
arg$LocationQuality <- factor(arg$LocationQuality, levels = c("2", "1", "0", "A", "B"))
arg <- arg[!is.na(arg$Latitude), ]

dates <- arg$Date
dates_posix <- as.POSIXct(dates, format = "%H:%M:%S %d-%b-%Y", tz = "UTC")
dates_numeric <- as.numeric(dates_posix)
time <- dates_numeric - dates_numeric[1]

dat <- data.frame(time = time, y = arg$Latitude, x = arg$Longitude, qal = arg$LocationQuality)

coordinates(dat) <- ~x + y
proj4string(dat) <- CRS("+proj=longlat") # initial projection

dat <- spTransform(dat, CRS("+init=epsg:32618"))


initial = list(
  a=c(coordinates(dat)[1,1],0,0,
      coordinates(dat)[1,2],0,0),
  P=diag(c(10000^2,5400^2,5400^2,10000^2,5400^2,5400^2))
)

fixPar = c(log(250), log(500), log(1500), rep(NA,6))
displayPar( mov.model=~1, err.model=list(x=~qal-1), drift = TRUE,
            data=dat,fixPar=fixPar)

constr=list(
  lower=c(rep(log(1500),2), rep(-Inf,4)),
  upper=rep(Inf,6)
)

ln.prior = function(theta){-abs(theta[4]+3)/0.5}

set.seed(321)
fit1 <- crwMLE(
  mov.model=~1, err.model=list(x=~qal-1), drift=TRUE,
  data=dat, Time.name="time", 
  initial.state=initial, fixPar=fixPar, constr=constr,# prior=ln.prior,
  control=list(trace=1, REPORT=1)
)

fit1
##Make hourly location predictions
predTime <- seq(ceiling(min(dat$time)), floor(max(dat$time)), 1)
predObj <- crwPredict(object.crwFit=fit1, predTime, speedEst=TRUE, flat=TRUE)
head(predObj)
crwPredictPlot(predObj, "map", asp=TRUE)

##Create simulation object with 100 parameter draws
set.seed(123)
simObj <- crwSimulator(fit1, predTime, method="IS", parIS=100, df=5, scale=18/20)

## Examine IS weight distribution
w <- simObj$thetaSampList[[1]][,1]
hist(w*100, main='Importance Sampling Weights', sub='More weights near 1 is desirable')

##Approximate number of independent samples
round(100/(1+(sd(w)/mean(w))^2))

#dev.new(bg=gray(0.75))
jet.colors <-
  colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan",
                     "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
crwPredictPlot(predObj, 'map', asp=TRUE)

## Sample 20 tracks from posterior predictive distribution
iter <- 20
cols <- jet.colors(iter)
for(i in 1:iter){
  samp <- crwPostIS(simObj, fullPost = FALSE)
  lines(samp$alpha.sim[,'mu.x'], samp$alpha.sim[,'mu.y'],col=cols[i])
}
