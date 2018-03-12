# status plots
# ~wrc 20180310


# load data and neccessary functions
if(file.exists("3-functions_data.RData")) {
	load("3-functions_data.RData")
} else {
	source("2-prep_functions_data.r")
}

# output directory and filename prefix
OUTS_PREFIX <- "outs_status/status_"

pb <- txtProgressBar(style = 3)
for(i in 1:length(behl)) {
setTxtProgressBar(pb, i / length(behl))
	b <- behl[[i]]
	s <- stal[[i]]
	tagname <- b$DeployID[1]
	
	# prepare status messages
	s <- s[s$Type == "CRC", ] # passed cyclic redundancy check
	sdates <- as.POSIXct(s$Received, tz = "UTC")
	
	s[, 'sdates'] <- sdates
	stown <- statusdepth_filter(s)
	
	plotst <- min(as.POSIXct(b$Start, tz = "UTC"))
	ben <- max(as.POSIXct(b$End, tz = "UTC"))
	sen <- max(sdates)
	ploten <- max(ben, sen)
		
	# save as a tiff
	tiff(paste0(OUTS_PREFIX, tagname, ".tif"), width = 800, height = 600)
	
	# set up the plotting area for dive profile
	par(fig = c(0, 1, 0, 0.80), mar = c(4.1, 5.1, 1.1, 0))
	plot_dives(b, show_gaps = TRUE, col = 1, pch = 16, hidelegend = TRUE, show_hours = FALSE, ylab = "", lty = NA, start_time = plotst, end_time = ploten)
	if(stown$reason == "pressure sensor flagged")
		abline(v = as.POSIXct(stown$cutoff, tz = "UTC"), col = "royalblue1", lwd = 2)
	mtext("Depth (m)", side = 2, line = 4.1, cex = 1)
	
	# set up the plotting area for depth status
	par(fig = c(0, 1, 0.80, 1), mar = c(0, 5.1, 1.1, 0), new = TRUE)
	xx <- as.POSIXct(s$sdates, tz = "UTC")
	yy <- s$Depth
	if(!all(is.na(yy))) {
		plot(xx, yy, xlim = c(plotst, ploten), pch = 16, col = "black", axes = FALSE, xlab = "", ylab = "")
		lines(xx[!is.na(yy)], yy[!is.na(yy)])
		axis(2, las = 1)
		abline(h = c(-10,10), lty = 2, col = "darkgrey")
		mtext("Depth at Dry (m)", side = 2, line = 4.1, cex = 1)
	} else {
		warning(paste(i, "] looks like there aren't any depth messages"))
	}
	
	# give it a title
	year <- format(as.POSIXct(b$Start[1], tz = "UTC"), format = "%Y")
	title(paste0(b$DeployID[1], " (", year, ")"))
	dev.off()
}
close(pb)

