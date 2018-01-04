# douglas filter r implementation functions

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

require(geosphere)

### helper functions
calcDAR <- function(A, B, C) {
	dist1 <- distGeo(A, B) / 1000
	dist2 <- distGeo(B, C) / 1000
	
	time1 <- as.numeric(difftime(movedat$t[iB], movedat$t[iA], units = "hours"))
	time2 <- as.numeric(difftime(movedat$t[iC], movedat$t[iB], units = "hours"))
	rate1 <- dist1 / time1
	rate2 <- dist2 / time2
	
	# alpha #how do I calculate alpha?
	a1 <- bearing(A, B)
	a2 <- bearing(B, C)
	
	aa1 <- (a1 + 360) %% 360
	aa2 <- (a2 + 360) %% 360
	
	deltaz <- abs(aa1 - aa2)
	alpha <- abs(180 - deltaz)
	
	list(dist1 = dist1, dist2 = dist2, rate1 = rate1, rate2 = rate2, alpha = alpha)
}


### three iterations of this
for(i in 1:3) {
	retain <- rep(FALSE, nrow(movedat))
	retain[1] <- TRUE # first one
	retain[length(retain)] <- TRUE # last one
	retain[length(retain) - 1] <- TRUE # second to last one
	
	for(n in 1:(nrow(movedat) - 3)) {		
		iA <- n
		iB <- iA + 1
		iC <- iB + 1
		iD <- iC + 1
		
		A <- c(movedat$lon[iA], movedat$lat[iA])
		B <- c(movedat$lon[iB], movedat$lat[iB])
		C <- c(movedat$lon[iC], movedat$lat[iC])
		D <- c(movedat$lon[iD], movedat$lat[iD])
		
		dars <- calcDAR(A, B, C)
		
		dist3 <- distGeo(C, D) / 1000
		dist4 <- distGeo(A, C) / 1000
		dist5 <- distGeo(B, D) / 1000
		
		### first three
		if(dars$dist1 < MAXREDUN) {
			retain_tmp <- TRUE	
		} else if(as.numeric(movedat$lc[iB]) >= KEEP_LC + 4) { #fix this so they appear in the right order
			retain_tmp <- TRUE
		} else if(dars$alpha < -25 + RATECOEF*log(min(dars$dist1, dars$dist2))) {
			retain_tmp <- FALSE
		} else if(dars$rate1 > MINRATE) {
			retain_tmp <- FALSE
		} else if(dars$rate2 > MINRATE | dars$dist1 + dist5 > dist4 + dist3) {
			retain_tmp <- FALSE
		} else {
			retain_tmp <- TRUE
		}
		
		retain[iB] <- retain_tmp
		
		# plot(movedat$lon[iA:iD], movedat$lat[iA:iD], main = paste(retain_tmp))
		# points(movedat$lon[iA:iD], movedat$lat[iA:iD], col = "red")
		# lines(movedat$lon[iA:iD], movedat$lat[iA:iD], lty = 2)
		# text(movedat$lon[iA:iD], movedat$lat[iA:iD], LETTERS[1:4], pos = 2)
		# readline(prompt="Press [enter] to continue")
	}
	
	movedat <- movedat[retain, ]
}

# two iterations of this

for(i in 1:2) {
	retain <- rep(FALSE, nrow(movedat))
	retain[1] <- TRUE # first one
	retain[length(retain)] <- TRUE # last one
	
	for(n in 1:(nrow(movedat) - 2)) {		
		iA <- n
		iB <- iA + 1
		iC <- iB + 1
		
		A <- c(movedat$lon[iA], movedat$lat[iA])
		B <- c(movedat$lon[iB], movedat$lat[iB])
		C <- c(movedat$lon[iC], movedat$lat[iC])
		
		dars <- calcDAR(A, B, C)
		
		### first three
		if(dars$dist1 < MAXREDUN) {
			retain_tmp <- TRUE	
		} else if(as.numeric(movedat$lc[iB]) >= KEEP_LC + 4) { #fix this so they appear in the right order
			retain_tmp <- TRUE
		} else if(dars$alpha < -25 + RATECOEF*log(min(dars$dist1, dars$dist2))) {
			retain_tmp <- FALSE
		} else if(dars$rate1 > MINRATE | dars$rate2 > MINRATE) {
			retain_tmp <- FALSE
		} else {
			retain_tmp <- TRUE
		}
				
		retain[iB] <- retain_tmp
	}
	
	movedat <- movedat[retain, ]
}