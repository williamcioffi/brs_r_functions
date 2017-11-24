

# tic for each minute
mseq <- seq.POSIXt(as.POSIXct(paste(as.Date(tt[1]), "00:00:00"), tz = "GMT"), as.POSIXct(paste(as.Date(tt[length(tt)]) + 1, "00:00:00"), tz = "GMT"), by = "min")
axis.POSIXct(1, at = mseq, labels = FALSE, tcl = 0.75)
axis.POSIXct(1, at = tseq, labels = TRUE, tcl = 0)


#look at gonio





g <- read.table("20170510_nprf.txt", header = FALSE, sep = ",", stringsAsFactors = FALSE)
phototimes <- read.table("datetime.csv", header = FALSE, sep = ",", stringsAsFactors = FALSE)

dese <- c("0A1FBD4",
		  "0A1FBE1",
		  "7CB7DBE")
		  
dose <- which(g[, 9] %in% dese)
cols <- as.factor(g[dose, 9])

		  
points(gdates[dose], rep(0, length(dose)), pch = 16, cex = .5, col = cols)


datetimesplit <- strsplit(phototimes[, 2], " ")
dates <- sapply(datetimesplit, '[[', 2)
times <- sapply(datetimesplit, '[[', 3)
dates <- gsub(":", "-", dates)
datetimes <- paste(dates, times)

notthese <- which(phototimes[, 5] == "spacer")
gdates <- as.POSIXct(substring(g[, 1], 1, 19), tz = "GMT")
ptimes <- as.POSIXct(datetimes, tz = "GMT")
points(gdates, rep(0, length(gdates)), pch = 16, cex = .5, col = rgb(0, 0, 1, .75))
points(ptimes[-notthese], rep(-100, length(ptimes[-notthese])), pch = 16, cex = .5, col = rgb(1, 0, 1, .75))

dese <- grep("ZcTag054", phototimes[, 1])
points(ptimes[dese], rep(-200, length(dese)), pch = 16, cex = .5, col = rgb(0, 1, 1, .75))

text(ptimes, rep(-200, length(ptimes)), as.character(1:length(ptimes)))

points(as.POSIXct("2017-06-03 17:56:06", tz = "GMT"), -300, pch = 17, col = "red", cex = 3)