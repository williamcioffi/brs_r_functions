# plot images, gonio, and dive data

plot_gonio <- function(deploy_ids, imagefile, goniofile, behaviorfile = "MASTER-Behavior.csv", pttkeyfile = "pttkey.csv", depth_lim = NULL, tz = "UTC", show_gaps = FALSE) {
	require(colorspace)
	UNIX_EPOCH <- "1970-01-01"
	uanimals <- deploy_ids
	
	### load image file
	im <- read.table(imagefile, header = TRUE, sep = ',', stringsAsFactors = FALSE)
	im$DateTimeOriginal <- as.POSIXct(im$DateTimeOriginal, tz = tz)
	names(im)[2] <- "datetime"
	im.all <- im
	
	im <- im[grep("ZcTag", im$SourceFile), ]
	labs <- strsplit(im$SourceFile, "_")
	im$DeployID <- sapply(labs, function(l) l[[grep("ZcTag", l)]])
	
	### load behavior file
	beh <- read.table(behaviorfile, header = TRUE, sep = ',', stringsAsFactors = FALSE)
	beh$Start <- as.numeric(as.POSIXct(beh$Start, format = "%H:%M:%S %d-%b-%Y", tz = tz))
	beh$End <- as.numeric(as.POSIXct(beh$End, format = "%H:%M:%S %d-%b-%Y", tz = tz))
	
	### load goniometer file
	gon <- read_gonio(goniofile)
	pttkey <- read.table(pttkeyfile, header = TRUE, sep = ',', stringsAsFactors = FALSE)
	gon$DeployID <- pttkey$DEPLOYID[match(gon$V9, pttkey$HEX)]

	gon$datetime <- as.POSIXct(gon$V1, tz = tz)
	
	### take a look
	cat("deploy ids:\n")
	print(uanimals)
	cat("goniometer animals:\n")
	print(unique(gon$DeployID))
	cat("image animals:\n")
	print(unique(im$DeployID))
	
	# resp <- readline("change deploy ids?")
	# if(resp != 'n') {
		# uanimals <- unlist(strsplit(resp, split = " "))
	# }
	
	mintp <- min(im$datetime)
	maxtp <- max(im$datetime)
	
	mintg <- min(gon$datetime)
	maxtg <- max(gon$datetime)
	
	mint <- min(mintg, mintp)
	maxt <- max(maxtg, maxtp)
	
	beh <- beh[beh$DeployID %in% uanimals, ]
	gon <- gon[gon$DeployID %in% uanimals, ]
	im  <- im [im$DeployID  %in% uanimals, ]
	
	cols <- rainbow_hcl(length(uanimals), c = 100)
	cols_a <- rainbow_hcl(length(uanimals), c = 100, alpha = 0.5)

	par(fig = c(0, 1, 0, .8), mar = c(5.1, 7.1, 0, 0))
	plot_dives2(beh, start_time = mint, end_time = maxt, col = cols_a, lwd = 5, pch = NA, depth_lim = depth_lim, show_gaps = show_gaps)
	tseq <- seq.POSIXt(
		from = trunc(mint, "hour"),
		to = trunc(maxt, "hour") + 60*60,
		by = "hour"
	)
	mtext(trunc(mint, "day"), side = 1, line = 3.1)
	axis.POSIXct(1, at = tseq, format = "%Hhrs")
	
	par(fig = c(0, 1, .8, 1), mar = c(0, 7.1, 0, 0), new = TRUE)
	plot(c(beh$Start[1], beh$End[nrow(beh)]), rep(0, 2), type = 'n', ylim = c(1, length(uanimals)*2 + 2), xlim = c(mint, maxt), xlab = "", ylab = "", axes = FALSE)
	
	for(i in 1:length(uanimals)) {
		dese <- im$DeployID == uanimals[i]
		points(im$datetime[dese], rep(1 + i, length(which(dese))), col = cols[i])
	}
	
	for(i in 1:length(uanimals)) {
		dese <- gon$DeployID == uanimals[i]
		points(gon$datetime[dese], rep(1 + length(uanimals) + i, length(which(dese))), col = cols[i], pch = 2)
	}
	
	points(im.all$datetime, rep(1, nrow(im.all)), col = "black", pch = 3)
	abline(h = c(1.5, 1.5 + length(uanimals), 1.5 + length(uanimals)*2), col = "grey", lty = 2)
	axis(2, at = c(1, 1 + length(uanimals)/2 + .5, 1 + length(uanimals) + length(uanimals)/2 + .5), lab = c("all photos", "tag photos", "gonio hits"), las = 1)
}
