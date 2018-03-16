###
# clean_tags.r
#
# look for depths over +/- 10 meters
# ~wrc 20180306

# update this

datadir <- "."
savedir <- "."

source("~/git/brs_r_functions/basic_tag_functions/findgaps.R")
source("~/git/brs_r_functions/basic_tag_functions/plot_dives.r")
source("~/git/brs_r_functions/basic_tag_functions/plot_status_corrupt.R")
source("~/git/brs_r_functions/basic_tag_functions/cattag.r")
source("~/git/brs_r_functions/basic_tag_functions/load.R")

#cut off after this many status message depths below +/- 10
cutoffnum <- 2

# cattag(datadir)
streams <- loadtag(datadir)
beh <- streams$behavior
sta <- streams$status
sta <- sta[-which(sta$Type == ""), ]
crp <- streams$corrupt


stal <- split(sta, sta$DeployID)
behl <- split(beh, beh$DeployID)
crpl <- split(crp, crp$DeployID)

library(colorspace)
cols <- rainbow_hcl(length(behl))

status_based_cutoff <- vector()
startstart <- vector()
end_or_fail <- vector()

# pdf()
for(i in 1:length(stal)) {
	cursta <- stal[[i]]
	curbeh <- behl[[i]]
	curcrp <- crpl[[i]]
	depover10 <- abs(cursta$Depth[!is.na(cursta$Depth)]) > 10
	numpastthresh <- length(which(depover10))
	
	startstart[i] <- as.character(curbeh[1, ]$Start)
	
	if(numpastthresh >= cutoffnum) {
		lastgood <- min(which(depover10)) - 1
		if(lastgood > 0) {
			status_based_cutoff[i] <- as.character(cursta$Received[lastgood])
			end_or_fail[i] <- "pressure sensor flagged"
		} else {
			status_based_cutoff[i] <- NA # never got a good status message
		}
	} else {
		status_based_cutoff[i] <- as.character(cursta$Received[nrow(cursta)])
		end_or_fail[i] <- "last status message"
	}
	
	status_withdeps <- which(!is.na(cursta$Depth))
	status_en <- cursta$Received[max(status_withdeps)]
	behavi_en <- curbeh$End[nrow(curbeh)]
	plot_en <- max(as.POSIXct(status_en, tz = "UTC"), as.POSIXct(behavi_en, tz = "UTC"))
	
	par(mfrow = c(3, 1), mar = c(4.1, 4.1, 0, 0))
	plot_status(cursta, 'Depth', col = cols[i], end_time = plot_en)
	abline(h = c(-10, 10), lty = 2)
	abline(v = as.POSIXct(status_based_cutoff[i], tz = "UTC"))
	legend("topright", legend = "status: depth", bty = 'n')
	plot_status(cursta, 'ZeroDepthOffset', col = cols[i], end_time = plot_en)
	abline(v = as.POSIXct(status_based_cutoff[i], tz = "UTC"))
	legend("topright", legend = "status: zero depth offset", bty = 'n')
	plot_dives(curbeh, show_gaps = TRUE, col = cols[i], pch = 16, cex = .5, lty = NA, end_time = plot_en)
	abline(v = as.POSIXct(status_based_cutoff[i], tz = "UTC"))
	# plot_corrupt(curcrp)
	# par(mfrow = c(2, 1))
	# dives <- curbeh[curbeh$What == "Dive", ]
	# surfs <- curbeh[curbeh$What == "Surface", ]
	# plot(dives$DurationMax, dives$DepthMax, col = cols[i])
	# abline(0, 1)
	# text(dives$DurationMax, dives$DepthMax, 1:nrow(dives))
	# legend("topleft", legend = dives$DeployID[1])
	# hist(surfs$DurationMax, col = cols[i])
	# legend("topleft", legend = dives$DeployID[1])
	
}

diffs <- difftime(as.POSIXct(status_based_cutoff, tz = "UTC"), as.POSIXct(startstart, tz = "UTC"), units = "days")
# dev.off()

names(status_based_cutoff) <- names(stal)

endtimesdf <- data.frame(DeployID = names(stal), date = status_based_cutoff, reason = end_or_fail)
write.table(endtimesdf, file = "endtimes.csv", sep = ',', row.names = FALSE)
