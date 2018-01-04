# douglas filter tester

#load data
datadir <- "~/Desktop/CURRENT/nulltest"
source("~/git/brs_r_functions/basic_tag_functions/load.R")
streams <- loadtag(datadir)
argos <- streams$argos
argos <- argos[which(argos$LocationQuality %in% c("0", "1", "2", "3", "A", "B", "Z")), ]

movedat <- data.frame(
	t = argos$Date,
	lat = argos$Latitude,
	lon = argos$Longitude,
	lc = argos$LocationQuality,
	lat2 = argos$Latitude2,
	lon2 = argos$Longitude2,
	stringsAsFactors = FALSE
)

movedat <- split(movedat, argos$DeployID)
movedat <- movedat[[1]]
m0 <- movedat

# movedat <- rbind(data.frame(t = "2017-05-10 16:17:00 UTC", lat = 35.58, lon = -74.712, lc = "3", stringsAsFactors = FALSE), movedat)
movedat$lc <- factor(movedat$lc, levels = c("Z", "B", "A", "0", "1", "2", "3"))

# load douglas filter


PARAMS <- list(maxredun = 10, keep_lc = 1, minrate = 50, ratecoef = 25)

m1 <- douglasfilter(movedat)
PARAMS$ratecoef = 50
m2 <- douglasfilter(movedat, PARAMS)
PARAMS$ratecoef = 5
m3 <- douglasfilter(movedat, PARAMS)

dgf <- read.table("~/Desktop/douglas_filtered/ZcTag054-058-Filtered.csv", header = TRUE, sep = ',')
dd <- split(dgf, dgf$animal)[[1]]
plot(dd$longitud, dd$latitude)

points(m0$lon, m0$lat, cex = .5, col = "blue")
points(m1$lon, m1$lat, cex = .5, col = "red")

# points(movedat$lon2, movedat$lat2, col = "blue", cex = .5)