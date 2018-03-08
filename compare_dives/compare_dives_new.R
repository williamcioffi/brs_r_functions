# modified function which I think makes more sense
compare_dives <- function(b1, b2) {
	# make sure times are POSIX
	# this is probably a bad idea just in case I pass it something not in UTC
	# should probably just check it beforehand and spit back an error
	b1$Start <- as.POSIXct(b1$Start, tz = "UTC")
	b2$Start <- as.POSIXct(b2$Start, tz = "UTC")
	b1$End <- as.POSIXct(b1$End, tz = "UTC")
	b2$End <- as.POSIXct(b2$End, tz = "UTC")
	
	b1raw <- b1
	b2raw <- b2
	
	# clip time clips both records only to the time that they were both transmitting
	start <- max(c(min(b1$Start), min(b2$Start)))
	end   <- min(c(max(b1$End)  , max(b2$End)))
	
	b1 <- b1[which(b1$Start >= start & b1$End <= end), ]
	b2 <- b2[which(b2$Start >= start & b2$End <= end), ]
	
	# take the midpoint of duration and depth
	b1[, 'DurationMean'] <- apply(b1[, c('DurationMax', 'DurationMin')], 1, mean)
	b2[, 'DurationMean'] <- apply(b2[, c('DurationMax', 'DurationMin')], 1, mean)

	b1[, 'DepthMean'] <- apply(b1[, c('DepthMax', 'DepthMin')], 1, mean)
	b2[, 'DepthMean'] <- apply(b2[, c('DepthMax', 'DepthMin')], 1, mean)
	
	# remove messages
	b1 <- b1[b1$What != "Message", ]
	b2 <- b2[b2$What != "Message", ]

	# grab end times and make sure they are in posix
	b1_ens <- as.POSIXct(b1$End, tz = "UTC")
	b2_ens <- as.POSIXct(b2$End, tz = "UTC")
	
	# add the first start to the beginning
	b1_ens <- as.POSIXct(c(as.character(b1$Start), as.character(b1_ens)), tz = "UTC")
	b2_ens <- as.POSIXct(c(as.character(b2$Start), as.character(b2_ens)), tz = "UTC")
	
	# delete any gaps
	gaps1 <- findgaps(b1raw)
	gaps2 <- findgaps(b2raw)
	
	if(gaps1$ngaps > 0) {
		for(i in 1:gaps1$ngaps) {
			b2_ens[b2_ens > gaps1$gap_st[i] & b2_ens < gaps1$gap_en[i]] <- NA
		}
		b2_ens <- b2_ens[!is.na(b2_ens)]
	}
	
	if(gaps2$ngaps > 0) {
		for(i in 1:gaps2$ngaps) {
			b1_ens[b1_ens > gaps2$gap_st[i] & b1_ens < gaps2$gap_en[i]] <- NA
		}
		
		b1_ens <- b1_ens[!is.na(b1_ens)]
	}
	
	# if b2 is longer than b1 flip them
	if(nrow(b2) > nrow(b1)) {
		b1.old <- b1
		b2.old <- b2
		b1 <- b2.old
		b2 <- b1.old
	}
	
	# set up some vectors to hold the results
	t1 <- vector()
	t2 <- vector()
	diff_times <- vector()
	diff_durs <- vector()
	diff_deps <- vector()
	diff_b1_type <- vector()
	diff_b2_type <- vector()
	
	for(i in 1:length(b1_ens)) {
		dt <- abs(difftime(b1_ens[i], b2_ens, units = "secs"))
		dis <- which.min(dt)
		
		t1[i] <- as.character(b1_ens[i])
		t2[i] <- as.character(b2_ens[dis])
		
		diff_times[i] <- difftime(b1_ens[i], b2_ens[dis], units = "secs")
		diff_durs[i] <- b1$DurationMean[i] - b2$DurationMean[dis]
		diff_deps[i] <- b1$DepthMean[i] - b2$DepthMean[dis]
		diff_b1_type[i] <- b1$What[i]
		diff_b2_type[i] <- b2$What[dis]
	}
	
	t1 <- as.POSIXct(t1, tz = "UTC")
	t2 <- as.POSIXct(t2, tz = "UTC")
	
	list(tag1 = b1$DeployID[1], tag2 = b2$DeployID[2], t1 = t1, t2_matched = t2, diff_times = diff_times, diff_durs = diff_durs, diff_deps = diff_deps, type1 = diff_b1_type, type2 = diff_b2_type)
}