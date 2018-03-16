###
# censor_tag
#
# censors behavior records based on duration and depth
# ~wrc 20180312

censor_tag2 <- function(b1, depth = 50, duration = 33*60) {
	depthresh <- depth
	timthresh <- duration
	
	# flag dives that no longer qualify
	tooshallow <- b1$DepthMax[b1$What == "Dive"] <= depthresh
	tooshort <- b1$DurationMax[b1$What == "Dive"] <= 33*60
	
	flagged <- tooshallow | tooshort
	b1_flagged <- b1
	b1_flagged$What[b1$What == "Dive"][flagged] <- "Surface"
	stretches <- findgaps2(b1)$stretchid
	
	# give each block of surfaces a unique id
	whatid <- as.numeric(as.factor(b1_flagged$What))
	surfid <- cumsum(c(TRUE, diff(whatid) != 0) & c(TRUE, diff(stretches) == 0))
	surfid[b1_flagged$What != "Surface"] <- NA

	# iterate over all surface blocks greater in length than 1 and group together into signal row
	usurfid <- unique(surfid[!is.na(surfid)])
	nsurfid <- length(surfid)
	b1_censored <- b1_flagged
	delete <- rep(FALSE, nrow(b1_censored))
	
	for(i in 1:nsurfid) {
		dese <- which(surfid == usurfid[i])
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
	
	msg_en_time_change <- (b1_censored$End[msg] - b1_censored$End[lastrow]) != 0
	b1_censored$End[msg][msg_en_time_change] <- b1_censored$End[lastrow][msg_en_time_change]
	
	msg_st_time_change <- (b1_censored$Start[msg] - b1_censored$Start[firstrow]) != 0
	b1_censored$Start[msg][msg_st_time_change] <- b1_censored$Start[firstrow][msg_st_time_change]
	
	# return censored data
	b1_censored
}
