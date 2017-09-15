###
# look at ZcTag051 walkabout

datadir <- "~/Downloads/tmp/walkabout1/"

source("~/git/brs_r_functions/basic_tag_functions/findgaps.R")
source("~/git/brs_r_functions/basic_tag_functions/plot_dives.r")
source("~/git/brs_r_functions/basic_tag_functions/cattag.r")
source("~/git/brs_r_functions/basic_tag_functions/load.R")

cattag(datadir)
streams <- loadtag(datadir)
beh <- streams$behavior
arg <- streams$argos
arg <- arg[-which(arg$LocationQuality == ""), ]

xxpos <- as.POSIXct(arg$Date, tz = "UTC")
xxdiv <- as.POSIXct(beh$Start, tz = "UTC")

tt <- as.character(beh$Start)
tt <- c(tt, as.character(max(beh$End)))

zoom1 <- as.POSIXct(min(tt), tz = "UTC")
zoom2 <- as.POSIXct(max(tt), tz = "UTC")

tseq <- seq.POSIXt(
  as.POSIXct(paste(as.Date(min(tt)), "00:00:00"), tz = "UTC"),
  as.POSIXct(paste(as.Date(max(tt)) + 1, "00:00:00"), tz = "UTC"),
  by = "hour"
)

dseq <- seq.POSIXt(
  as.POSIXct(paste(as.Date(min(tt)), "00:00:00"), tz = "UTC"),
  as.POSIXct(paste(as.Date(max(tt)) + 1, "00:00:00"), tz = "UTC"),
  by = "day"
)

x11()
par(mfrow = c(2, 1), mar = c(4.1, 6.1, 0, 0), oma = c(0, 0, 2.1, 2.1))
plot(xxpos, arg$Latitude, ylim = c(33, 36), pch = 16, cex = .5, las = 1, xlab = "", ylab = "Latitude (deg)", xaxt = 'n', xlim = c(zoom1, zoom2))
axis.POSIXct(1, at = dseq, format = "%d%b", las = 2, tcl = '-0.75')
axis.POSIXct(1, at = tseq, labels = FALSE)
plot_dives(beh, show_gaps = TRUE, pch = 16, lty = 0, cex = .5, col = "black")
box()

plot(beh$DepthMax)
