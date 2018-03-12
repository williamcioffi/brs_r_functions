# count plots
# ~wrc 20180310

# load data and neccessary functions
if(file.exists("3-functions_data.RData")) {
	load("3-functions_data.RData")
} else {
	source("2-prep_functions_data.r")
}

# output directory and filename prefix
OUTS_PREFIX <- "outs_counts/count_"

pb <- txtProgressBar(style = 3)
for(i in 1:length(behl)) {
setTxtProgressBar(pb, i / length(behl))
	b <- behl[[i]]
	tagname <- b$DeployID[1]
	tiff(paste0(OUTS_PREFIX, tagname, ".tif"), width = 800, height = 600)
	par(fig = c(0, 1, 0, 0.80), mar = c(4.1, 5.1, 1.1, 0))
	plot_dives(b, show_gaps = TRUE, col = 1, pch = 16, hidelegend = TRUE, show_hours = FALSE, ylab = "", lty = NA)
	mtext("Depth (m)", side = 2, line = 4.1, cex = 1)
	
	# get just the messages for plotting counts
	msg <- b[b$What == "Message", ]
	sts <- as.POSIXct(msg$Start, tz = "UTC")
	ens <- as.POSIXct(msg$End, tz = "UTC")
	
	# set up the plotting area for message counts
	par(fig = c(0, 1, 0.80, 1), mar = c(0, 5.1, 1.1, 0), new = TRUE)
	plot(sts, msg$Count, axes = FALSE, xlab = "", ylab = "", type = 'n', xlim = c(min(sts), max(ens)), ylim = c(0, maxcount))
	mtext("Message Count", side = 2, line = 4.1, cex = 1)
	rect(sts, 0, ens, msg$Count, col = rgb(0, 0, 0, .15), border = rgb(0, 0, 0, .25))
	axis(2, las = 1)
	
	year <- format(as.POSIXct(b$Start[1], tz = "UTC"), format = "%Y")
	title(paste0(b$DeployID[1], " (", year, ")"))
	dev.off()
}
close(pb)
