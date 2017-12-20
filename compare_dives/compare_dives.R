#compare 2 dive patterns
# 
# ff <- list.files()
# dese <- grep("Behavior", ff)
# ff <- ff[dese]
# nf <- length(ff)
# 
# b <- vector()
# 
# for(i in 1:nf) {
# 	tmp <- read.table(ff[i], header = TRUE, sep = ',', stringsAsFactors = FALSE)
# 	b <- rbind(b, tmp)
# }
# 
# 
# bb <- split(b, b$DeployID)
# names(bb)
# 
# b1 <- bb[[1]]
# b2 <- bb[[3]]
# 
# com <- compare_dives(b1, b2)
# 
# 
# 
# 
# b1_null <- build_null_diver(b1, sim_start_time = b1$Start[1], sim_end_time = b1$Start[nrow(b1)])
# b2_null <- build_null_diver(b2, sim_start_time = b2$Start[1], sim_end_time = b2$Start[nrow(b2)])
# 
# com_null <- compare_dives(b1_null, b2_null)
# 
# 
# par(mfrow = c(2, 1), mar = rep(0, 4), oma = c(4.1, 4.1, 0, 0))
# plot(as.POSIXct(com$b1_ens, tz = "UTC"), abs(com$diff_times ), log = 'y')
# abline(h = 60, lty = 2, col = "purple")
# 
# plot(as.POSIXct(com_null$b1_ens, tz = "UTC"), abs(com_null$diff_times ), log = 'y')
# abline(h = 60, lty = 2, col = "purple")
# 
# 
# 
# # # par(mfrow = c(2, 1), mar = rep(0, 4), oma = c(4.1, 4.1, 0, 0))
# # plotcomparedives(com, obs = TRUE)
# # plotcomparedives(com_null)
# 
# # cplot(list(com$diff_times, com_null$diff_times))
# 
# #draw gaps
# gaps <- findgaps(b2)
# gapx1 <- as.POSIXct(gaps$gap_st, tz = "UTC")
# gapx2 <- as.POSIXct(gaps$gap_en, tz = "UTC")
# rect(gapx1, -1000, gapx2, 15000, col = rgb(1, 0, 0, .25), border = NA)
# 
# 
# #draw gaps
# gaps <- findgaps(b1)
# gapx1 <- as.POSIXct(gaps$gap_st, tz = "UTC")
# gapx2 <- as.POSIXct(gaps$gap_en, tz = "UTC")
# rect(gapx1, -1000, gapx2, 15000, col = rgb(0, 1, 0, .25), border = NA)



#plot compare output
plotcomparedives <- function(com, obs = FALSE, log = '', ...) {
	days <- as.Date(com$b1_ens)
	means <- tapply(abs(com$diff_times), days, mean)
	quants <- tapply(abs(com$diff_times), days, quantile, c(0.025, 0.975))
	daily_means <- data.frame(date = as.character(names(means)), mean = as.vector(means), lower = sapply(quants, '[[', 1), upper = sapply(quants, '[[', 2))

	xx <- as.POSIXct(daily_means$date, tz = "UTC")[1:(nrow(daily_means))]
	xx <- rep(xx, 2)
	yy <- c(daily_means$upper[1:nrow(daily_means)], daily_means$lower[1:nrow(daily_means)])

	plot(xx, yy, type = 'n', xaxt = 'n', las = 1, log = log, ...)
	axis.POSIXct(1, at = xx)
	
	x <- as.POSIXct(daily_means$date, tz = "UTC")
	y <- daily_means$mean
	u <- daily_means$upper
	l <- daily_means$lower
	points(x, y, pch = 16)

	segments(x, l, x, u)
	abline(h = 60, lty = 2)
	if(obs) {
		points(as.POSIXct(com$b1_ens, tz = "UTC"), abs(com$diff_times ))
	}
}







compare_dives <- function(b1, b2, cliptime = FALSE) {
	b2raw <- b2
	
	# clip time clips both records only to the time that they were both transmitting
	if(cliptime) {
		start <- max(c(min(b1$Start), min(b2$Start)))
		end   <- min(c(max(b1$End)  , max(b2$End)))
		
		b1 <- b1[which(b1$Start >= start & b1$End <= end), ]
		b2 <- b2[which(b2$Start >= start & b2$End <= end), ]
	}
	
	b1[, 'DurationMean'] <- apply(b1[, c('DurationMax', 'DurationMin')], 1, mean)
	b2[, 'DurationMean'] <- apply(b2[, c('DurationMax', 'DurationMin')], 1, mean)

	b1[, 'DepthMean'] <- apply(b1[, c('DepthMax', 'DepthMin')], 1, mean)
	b2[, 'DepthMean'] <- apply(b2[, c('DepthMax', 'DepthMin')], 1, mean)
	
	b1 <- b1[-which(b1$What == "Message"), ]
	b2 <- b2[-which(b2$What == "Message"), ]

	b1_ens <- as.POSIXct(b1$End, tz = "UTC")
	b2_ens <- as.POSIXct(b2$End, tz = "UTC")
	
	if(b1_ens[1] > b2_ens[1]) {
		st <- which.min(abs(difftime(b1_ens[1], b2_ens, unit = "secs")))
		b2 <- b2[st:nrow(b2), ]
	} else {
		st <- which.min(abs(difftime(b2_ens[1], b1_ens, unit = "secs")))
		b1 <- b1[st:nrow(b1), ]
	}
	
	b1_ens <- as.POSIXct(b1$End, tz = "UTC")
	b2_ens <- as.POSIXct(b2$End, tz = "UTC")
	
	diff_times <- 1:nrow(b1)*NA
	diff_durs <- 1: nrow(b1)*NA
	diff_deps <- 1: nrow(b1)*NA
	diff_b1_type <- 1:nrow(b1)*NA
	diff_b2_type <- 1:nrow(b1)*NA
	
	for(i in 1: nrow(b1)) {
		dt <- abs(difftime(b1_ens[i], b2_ens, units = "secs"))
		dis <- which.min(dt)
		
		diff_times[i] <- difftime(b1_ens[i], b2_ens[dis], units = "secs")
		diff_durs[i] <- b1$DurationMean[i] - b2$DurationMean[dis]
		diff_deps[i] <- b1$DepthMean[i] - b2$DepthMean[dis]
		diff_b1_type[i] <- b1$What[i]
		diff_b2_type[i] <- b2$What[dis]
	}
	
	gaps <- findgaps(b2raw)
	
	if(gaps$ngaps > 0) {
		for(i in 1:gaps$ngaps) {
			dese <- which(b1_ens > as.POSIXct(gaps$gap_st[i], tz = "UTC") & b1_ens < as.POSIXct(gaps$gap_en[i], tz = "UTC"))
			if(length(dese) > 0) {
				b1_ens		 <- b1_ens[-dese]
				diff_times	 <- diff_times[-dese]
				diff_durs	 <- diff_durs[-dese]
				diff_deps	 <- diff_deps[-dese]
				diff_b1_type <- diff_b1_type[-dese]
				diff_b2_type <- diff_b2_type[-dese]
			}
		}
	}
	
	list(b1_ens = b1_ens, diff_times = diff_times, diff_durs = diff_durs, diff_deps = diff_deps, type1 = diff_b1_type, type2 = diff_b2_type)
}