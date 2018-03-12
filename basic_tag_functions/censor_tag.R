###
# censor_tag
#
# censors behavior records based on duration and depth
# ~wrc 20180312

censor_tag <- function(b1, depth = 50, duration = 33*60) {
	depthresh <- depth
	timthresh <- duration
	
	# flag dives that no longer qualify
	tooshallow <- b1$DepthMax[b1$What == "Dive"] <= depthresh
	tooshort <- b1$DurationMax[b1$What == "Dive"] <= 33*60
	
	flagged <- tooshallow | tooshort
	b1_flagged <- b1
	b1_flagged$What[b1$What == "Dive"][flagged] <- "Surface"
	
	# give each block of surfaces a unique id
	k <- 0
	last <- ""
	surfaceid <- 1:nrow(b1_flagged)*NA
	deserows <- which(b1_flagged$What != "Message")
	
	for(i in deserows) {
		currow <- b1_flagged[i, ]
		if(currow$What == "Surface") {
			if(last != "Surface") k <- k + 1
			surfaceid[i] <- k
		}
		last <- currow$What
	}
	
	# iterate over all surface blocks greater in length than 1 and group together into signal row
	usurfaceid <- unique(surfaceid[!is.na(surfaceid)])
	nsurfaceid <- length(usurfaceid)
	b1_censored <- b1_flagged
	delete <- rep(FALSE, nrow(b1_censored))
	for(i in 1:nsurfaceid) {
		dese <- which(surfaceid == usurfaceid[i])
		en <- length(dese)
		if(en > 1) {
			newrow <- b1_flagged[dese[1], ]
			newrow$End <- b1_flagged$End[dese[en]]
			newrow$Shape <- ""
			newrow$DepthMin <- NA
			newrow$DepthMax <- NA
			newrow$Count <- NA
			newrow$DurationMin <- sum(b1_flagged$DurationMin[dese])
			newrow$DurationMax <- sum(b1_flagged$DurationMax[dese])
			
			b1_censored[dese[1], ] <- newrow
			delete[dese[2:en]] <- TRUE
		}
	}
	
	# delete the extra surface messages
	b1_censored <- b1_censored[!delete, ]
	
	# correct the message times for compatibility with other code
	msg <- which(b1_censored$What == 'Message')
	lastrow <- msg - 1
	lastrow <- c(lastrow[lastrow > 0], nrow(b1_censored))
	firstrow <- msg + 1
	
	msg_en_time_change <- as.numeric(difftime(b1_censored$End[lastrow], b1_censored$End[msg])) != 0
	b1_censored$End[msg][msg_en_time_change] <- b1_censored$End[lastrow][msg_en_time_change]
	
	msg_st_time_change <- as.numeric(difftime(b1_censored$Start[firstrow], b1_censored$Start[msg])) != 0
	b1_censored$Start[msg][msg_st_time_change] <- b1_censored$Start[firstrow][msg_st_time_change]
	
	# return censored data
	b1_censored
}