# douglas filter tester
source("douglasfunctions.r")

# load example douglas filtered
# from this i can infer that they used the DAR filter with 10 as a minrate
# with only L2s accepted always
# and perhaps a rate coefficient of less than 25?
dgf <- read.table("~/Desktop/douglas_filtered/ZcTag054-058-Filtered.csv", header = TRUE, sep = ',')
dd <- split(dgf, dgf$animal)[[1]]

# load data
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
# movedat <- rbind(data.frame(
	# t = "2017-05-10 16:17:00 UTC",
	# lat = 35.58,
	# lon = -74.712,
	# lc = "3",
	# lat2 = 35.58,
	# lon2 = -74.712
# ), movedat)

movedat$lc <- factor(movedat$lc, levels = c("Z", "B", "A", "0", "1", "2", "3"))
m0 <- movedat

# load douglas filter


PARAMS <- list(maxredun = 10, keep_lc = 5, minrate = 50, ratecoef = 25, r_only = FALSE)
PARAMS$minrate <- 10
PARAMS$ratecoef <- 15
PARAMS$keep_lc <- 6
# PARAMS$r_only = TRUE

m1 <- douglasfilter(movedat, PARAMS)

# plot
plot(dd$longitud, dd$latitude, cex = 1.5)

# points(m0$lon, m0$lat, cex = .5, col = "blue")
text(m0$lon, m0$lat, paste(1:nrow(m0)), cex = .5, col = "blue")
points(m1$lon, m1$lat, cex = .5, col = "red")

#
q <- 30
points(m0$lon[q], m0$lat[q], cex = 5, col = "purple")

# # for(i in 2:nrow(m1)) {
	# lines(m1$lon[c(i-1, i)], m1$lat[c(i-1, i)], col = "blue")
	# invisible(readline(prompt="Press [enter] to continue"))
# }

# for(i in 2:nrow(dd)) {
	# lines(dd$longitud[c(i-1, i)], dd$latitude[c(i-1, i)], col = "black")
	# invisible(readline(prompt="Press [enter] to continue"))
# }

# points(movedat$lon2, movedat$lat2, col = "blue", cex = .5)