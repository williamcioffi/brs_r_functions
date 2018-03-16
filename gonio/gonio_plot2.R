# plot images, gonio, and dive data

plot_gonio2 <- function(goniofile, behaviorfile = "MASTER-Behavior.csv", pttkeyfile = "pttkey.csv", depth_lim = NULL, tz = "UTC", show_gaps = TRUE) {
	require(colorspace)
	
	### load behavior file
	beh <- read.table(behaviorfile, header = TRUE, sep = ',', stringsAsFactors = FALSE)
	beh$Start <- as.numeric(as.POSIXct(beh$Start, format = "%H:%M:%S %d-%b-%Y", tz = tz))
	beh$End <- as.numeric(as.POSIXct(beh$End, format = "%H:%M:%S %d-%b-%Y", tz = tz))
	
	### load goniometer file
	gon <- read_gonio(goniofile)
	pttkey <- read.table(pttkeyfile, header = TRUE, sep = ',', stringsAsFactors = FALSE)
	gon$DeployID <- pttkey$DEPLOYID[match(gon$V9, pttkey$HEX)]
	gon$datetime <- as.POSIXct(gon$V1, tz = tz)
	
	uanimals <- sort(unique(gon$DeployID[!is.na(gon$DeployID)]))
	
	mint <- min(gon$datetime)
	maxt <- max(gon$datetime)
	
	beh <- beh[beh$DeployID %in% uanimals, ]
	gon <- gon[gon$DeployID %in% uanimals, ]
	
	cols <- rainbow_hcl(length(uanimals), c = 100, alpha = .5)
	par(fig = c(0, 1, 0, .8), mar = c(5.1, 7.1, 0, 0))
	plot_dives2(beh, start_time = mint, end_time = maxt, col = cols, lwd = 5, pch = NA, show_gaps = show_gaps, depth_lim = depth_lim)
	tseq <- seq.POSIXt(
		from = trunc(mint, "hour"),
		to = trunc(maxt, "hour") + 60*60,
		by = "hour"
	)
	mtext(trunc(mint, "day"), side = 1, line = 3.1)
	axis.POSIXct(1, at = tseq, format = "%Hhrs")
	
	par(fig = c(0, 1, .8, 1), mar = c(0, 7.1, 0, 0), new = TRUE)
	plot(c(beh$Start[1], beh$End[nrow(beh)]), rep(0, 2), type = 'n', ylim = c(1, length(uanimals)), xlim = c(mint, maxt), xlab = "", ylab = "", axes = FALSE)
	
	for(i in 1:length(uanimals)) {
		dese <- gon$DeployID == uanimals[i]
		points(gon$datetime[dese], rep(i, length(which(dese))), col = cols[i], pch = 2)
	}
	
	axis(2, at = 1:length(uanimals), lab = uanimals, las = 1)
}