# douglas filter r implementation functions

require(geosphere)

### douglas filter
douglasfilter <- function(movedat, PARAMS = list(maxredun = 10, keep_lc = 1, minrate = 50, ratecoef = 25)) {
	MAXREDUN <- PARAMS$maxredun
	KEEP_LC <- PARAMS$keep_lc
	MINRATE <- PARAMS$minrate
	RATECOEF <- PARAMS$ratecoef
	
	
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
			
			dist1 <- distGeo(A, B) / 1000
			dist2 <- distGeo(B, C) / 1000
			dist3 <- distGeo(C, D) / 1000
			dist4 <- distGeo(A, C) / 1000
			dist5 <- distGeo(B, D) / 1000
			
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
			
			### first three
			if(dist1 < MAXREDUN) {
				retain_tmp <- TRUE	
			} else if(as.numeric(movedat$lc[iB]) >= KEEP_LC + 4) { #fix this so they appear in the right order
				retain_tmp <- TRUE
			} else if(alpha < -25 + RATECOEF*log(min(dist1, dist2))) {
				retain_tmp <- FALSE
			} else if(rate1 > MINRATE) {
				retain_tmp <- FALSE
			} else if(rate2 > MINRATE | dist1 + dist5 > dist4 + dist3) {
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
			
			### first three
			if(dist1 < MAXREDUN) {
				retain_tmp <- TRUE	
			} else if(as.numeric(movedat$lc[iB]) >= KEEP_LC + 4) { #fix this so they appear in the right order
				retain_tmp <- TRUE
			} else if(alpha < -25 + RATECOEF*log(min(dist1, dist2))) {
				retain_tmp <- FALSE
			} else if(rate1 > MINRATE | rate2 > MINRATE) {
				retain_tmp <- FALSE
			} else {
				retain_tmp <- TRUE
			}
					
			retain[iB] <- retain_tmp
		}
		
		movedat <- movedat[retain, ]
	}
	
	movedat
}
