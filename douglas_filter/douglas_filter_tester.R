# douglas filter tester
source("douglasfunctions.R")
# 
# 
# an <- 1 # animal
# # load example douglas filtered
# # from this i can infer that they used the DAR filter with 10 as a minrate
# # with only L2s accepted always
# # and perhaps a rate coefficient of less than 25?
# dgf <- read.table("~/Desktop/douglas_filtered/ZcTag054-058-Filtered.csv", header = TRUE, sep = ',')
# dd <- split(dgf, dgf$animal)[[an]]
# 
# # load data
# datadir <- "~/Desktop/CURRENT/nulltest"
# source("~/git/brs_r_functions/basic_tag_functions/load.R")
# streams <- loadtag(datadir)
# argos <- streams$argos

argos <- read.table("ZcTag029.csv", header = TRUE, sep = ',', stringsAsFactors = FALSE)
# argos <- argos[which(argos$argos.lc %in% c("0", "1", "2", "3", "A", "B", "Z")), ]

movedat <- data.frame(
	t = argos$timestamp,
	lat = argos$argos.lat1,
	lon = argos$argos.lon1,
	lc = argos$argos.lc,
	lat2 = argos$argos.lat2,
	lon2 = argos$argos.lon2,
	stringsAsFactors = FALSE
)
# 
# movedat <- split(movedat, argos$DeployID)
# movedat <- movedat[[an]]
# # movedat <- rbind(data.frame(
# 	# t = "2017-05-10 16:17:00 UTC",
# 	# lat = 35.58,
# 	# lon = -74.712,
# 	# lc = "3",
# 	# lat2 = 35.58,
# 	# lon2 = -74.712
# # ), movedat)

movedat$lc <- factor(movedat$lc, levels = c("Z", "B", "A", "0", "1", "2", "3"))
m0 <- movedat[complete.cases(movedat), ]

# load douglas filter


PARAMS <- list(maxredun = 10, keep_lc = 5, minrate = 50, ratecoef = 25, r_only = FALSE)
PARAMS$maxredun <- 3
PARAMS$minrate <- 10
PARAMS$ratecoef <- 25
PARAMS$keep_lc <- 6
# PARAMS$r_only = TRUE

m1 <- douglasfilter(m0, PARAMS)

x11()
par(mfrow = c(2, 1), mar = rep(0, 4))
m2 <- m1[m1$filtered == FALSE, ]
plot(m2$lon, m2$lat, ylim = c(35, 38), xlim = c(-75.9, -73), pch = 16, cex = .5, col = "red")
# points(m1$lon, m1$lat)

argos2 <- argos[argos$algorithm.marked.outlier != "true", ]
plot(argos2$location.long, argos2$location.lat, ylim = c(35, 38), xlim = c(-75.9, -73), pch = 16, cex = .5, col = "blue")
# points(argos$location.long, argos$location.lat)
