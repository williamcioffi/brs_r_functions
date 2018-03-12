# message distributions
# ~wrc 20180311

# load data and neccessary functions
if(file.exists("3-functions_data.RData")) {
	load("3-functions_data.RData")
} else {
	source("2-prep_functions_data.r")
}

# output directory and filename prefix
OUTS_PREFIX <- "outs_msg_distributions/msgd_"

### count messages per day, length, and transmit times
# split tags by BASELINE-BRS and Zc and Gm
tagyears <- sapply(behl, function(l) {
	as.numeric(format(as.POSIXct(l$Start[1], tz = "UTC"), format = "%Y"))
})

tagcats <- list(
	"2017 Zc" = names(tagyears)[grepl("Zc", names(tagyears)) & tagyears == 2017],
	"2014-2016 Zc" = names(tagyears)[grepl("Zc", names(tagyears)) & tagyears != 2017],
	"2017 Gm" = names(tagyears)[grepl("Gm", names(tagyears)) & tagyears == 2017],
	"2014-2016 Gm" = names(tagyears)[grepl("Gm", names(tagyears)) & tagyears != 2017]
)

for(i in 1:length(tagcats)) {
	desetags <- tagcats[[i]]
	tagcatlab <- names(tagcats)[i]
	
	b <- behl[desetags]
	
	msgdays <- list()
	msglens <- list()
	msgcnts <- list()
	for(p in 1:length(b)) {
		b_tmp <- b[[p]]
		b_tmp_msg <- b_tmp[b_tmp$What == "Message", ]
		
		# calculate number of messages per day
		days <- as.Date(b_tmp_msg$End)
		tdays <- as.vector(table(days))
		msgdays[[p]] <- tdays
		
		# calculate the message length in hours
		st <- as.POSIXct(b_tmp_msg$Start, tz = "UTC")
		en <- as.POSIXct(b_tmp_msg$End, tz = "UTC")
		msglens[[p]] <- as.numeric(difftime(en, st, units = "hours"))
		
		# get number of times message transmitted
		msgcnts[[p]] <- b_tmp_msg$Count
	}
	
	
	names(msglens) <- names(b)
	# names(msgdays) <- names(b)
	# names(msgcnts) <- names(b)
	
	fname <- gsub(" ", "_", tagcatlab, fixed = TRUE)
	tiff(paste0(OUTS_PREFIX, fname, ".tif"), width = 800, height = 600)
	par(mfrow = c(1, 3), mar = c(5.1, 0, 0, 0), oma = c(0, 8.1, 4.4, 0))
	cplot(msglens, zero = FALSE, rug = FALSE, cex.axis = 1.5)
	mm <- median(unlist(msglens))
	axis(3, at = mm, label = round(mm, 1), tick = FALSE, pos = length(msglens), cex.axis = 1.5)
	segments(mm, 0, mm, length(msglens), col = "royalblue2", lty = 2)
	mtext("msg length (hrs)", side = 1, line = 3.1, cex = 1)
	
	names(msgdays) <- rep("", length(msgdays))
	cplot(msgdays, zero = FALSE, rug = FALSE, cex.axis = 1.5)
	mm <- median(unlist(msgdays))
	axis(3, at = mm, label = mm, tick = TRUE, pos = length(msgdays), cex.axis = 1.5)
	segments(mm, 0, mm, length(msgdays), col = "royalblue2", lty = 2)	
	mtext("msg / day (count)", side = 1, line = 3.1, cex = 1)
	
	mtext(tagcatlab, side = 3, line = 2.1, cex = 2)
	
	names(msgcnts) <- rep("", length(msgcnts))
	cplot(msgcnts, zero = FALSE, rug = FALSE, cex.axis = 1.5)
	mm <- median(unlist(msgcnts))
	axis(3, at = mm, label = mm, tick = TRUE, pos = length(msgcnts), cex.axis = 1.5)
	segments(mm, 0, mm, length(msgcnts), col = "royalblue2", lty = 2)
	mtext("times msg received (count)", side = 1, line = 3.1, cex = 1)
	dev.off()
}