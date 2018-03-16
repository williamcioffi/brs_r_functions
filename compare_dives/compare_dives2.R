#tiny helper function
matchtimes <- function(t1, t2) {
# t1, t2 are numeric
	findInterval(t1, c(-Inf, head(t2, -1)) + c(0, diff(t2)/2))
}

# modified function which I think makes more sense
# expects Start and End to be numeric seconds
compare_dives2 <- function(b1, b2, tz = "UTC") {
	# b1$Start <- as.numeric(as.POSIXct(b1$Start, tz = tz))
	# b2$Start <- as.numeric(as.POSIXct(b2$Start, tz = tz))
	# b1$End <- as.numeric(as.POSIXct(b1$End, tz = tz))
	# b2$End <- as.numeric(as.POSIXct(b2$End, tz = tz))
	
	if(nrow(b1) > 0 & nrow(b2) > 0) {
		# take the midpoint of duration and depth
		b1[, 'DurationMean'] <- rowMeans(b1[, c('DurationMax', 'DurationMin')])
		b2[, 'DurationMean'] <- rowMeans(b2[, c('DurationMax', 'DurationMin')])
	
		b1[, 'DepthMean'] <- rowMeans(b1[, c('DepthMax', 'DepthMin')])
		b2[, 'DepthMean'] <- rowMeans(b2[, c('DepthMax', 'DepthMin')])
		
		# find the gaps
		gaps1 <- findgaps2(b1)
		gaps2 <- findgaps2(b2)
		
		# add msg id and remove messages
		b1[, 'msgid'] <- cumsum(b1$What == "Message")
		b2[, 'msgid'] <- cumsum(b2$What == "Message")	
		b1 <- b1[b1$What != "Message", ]
		b2 <- b2[b2$What != "Message", ]
		
		# kill gaps
		if(gaps1$ngaps > 0) {
			for(i in 1:gaps1$ngaps) {
				b2$End[b2$End > gaps1$gap_st[i] & b2$End < gaps1$gap_en[i]] <- NA
			}
			b2 <- b2[!is.na(b2$End), ]
		}
		
		if(gaps2$ngaps > 0) {
			for(i in 1:gaps2$ngaps) {
				b1$End[b1$End > gaps2$gap_st[i] & b1$End < gaps2$gap_en[i]] <- NA
			}
			b1 <- b1[!is.na(b1$End), ]
		}
		
		# clip time clips both records only to the time that they were both transmitting
		start <- max(c(min(b1$Start), min(b2$Start)))
		end   <- min(c(max(b1$End)  , max(b2$End)))
	
		b1 <- b1[which(b1$Start >= start & b1$End <= end), ]
		b2 <- b2[which(b2$Start >= start & b2$End <= end), ]
		
		# if b2 is longer than b1 flip them
		# if(nrow(b2) > nrow(b1)) {
			# tmpb1 <- b1
			# b1 <- b2
			# b2 <- tmpb1
		# }
		
		match <- matchtimes(b1$End, b2$End)
			
		t1 <- b1$End
		t2 <- b2$End[match]
		
		diff_times <- t2 - t1
		diff_durs <- b1$DurationMean - b2$DurationMean[match]
		diff_deps <- b1$DepthMean - b2$DepthMean[match]
		
		what1 <- sub("Surface", "dive_st", as.character(b1$What))
		what1 <- sub("Dive", "dive_en", what1)
		what2 <- sub("Surface", "dive_st", as.character(b2$What[match]))
		what2 <- sub("Dive", "dive_en", what2)
				
		out <- list(tag1 = as.character(b1$DeployID[1]), tag2 = as.character(b2$DeployID[1]), t1 = t1, t2_matched = t2, matchindices = match, diff_times = diff_times, diff_durs = diff_durs, diff_deps = diff_deps, what1 = what1, what2 = what2, msgid1 = b1$msgid, msgid2 = b2$msgid)
	} else {
		warning("tags don't appear to overlap?")
		out <- NULL
	}
	
	out
}