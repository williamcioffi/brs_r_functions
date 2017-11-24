#bsam test

library(bsam)

datadir <- "~/Desktop/nulltest"
savedir <- "~/Desktop/nulltest"

source("~/git/brs_r_functions/basic_tag_functions/findgaps.R")
source("~/git/brs_r_functions/basic_tag_functions/plot_dives.r")
source("~/git/brs_r_functions/basic_tag_functions/cattag.r")
source("~/git/brs_r_functions/basic_tag_functions/load.R")

# cattag(datadir)
streams <- loadtag(datadir)
argos <- streams$argos

argos <- argos[which(argos$LocationQuality %in% c("0", "1", "2", "3", "A", "B")), ]

movedat <- data.frame(id = argos$DeployID, date = argos$Date, lc = argos$LocationQuality, lon = argos$Longitude, lat = argos$Latitude)

fit <- fit_ssm(movedat, model = "DCRW", tstep = 0.25, adapt = 5000, samples = 5000, 
              thin = 5, span = 1) 

# map_ssm(fit)
# diag_ssm(fit)
# plot_fit(fit)

results <- get_summary(fit)

dese <- grep("Zc", results$id)
# desetags <- c("ZcTag055", "ZcTag056")
# dese <- which(results$id %in% desetags)
results <- results[dese, ]

xx <- as.POSIXct(results$date, tz = "UTC")
cc <- results$id

# uids <- unique(results$id)
# nids <- length(uids)

# timelags <- vector()
# for(i in 1:nids) {
	# dese <- which(results$id == uids[i])
	# timelags[dese] <- difftime(xx[dese], min(xx[dese]), units = "days")
# }
# xx <- timelags

  
lay <- matrix(c(1, 1, 1, 1, 1, 1, 1, 3,
				2, 2, 2, 2, 2, 2, 2, 3), 2, 8, byrow = TRUE)
layout(lay)

y1 <- results$lon
l1 <- results$lon.025
u1 <- results$lon.975

par(mar = c(0, 4.1, 0, 0), las = 1)
plot(rep(xx, 3), c(y1, u1, l1), xaxt = 'n', type = 'n', ylab = "", xlab = "")
points(xx, y1, col = cc, pch = 16)
segments(xx, l1, xx, u1, col = cc)
legend("bottomleft", legend = "LONGITUDE", bty = 'n')

y2 <- results$lat
l2 <- results$lat.025
u2 <- results$lat.975

par(mar = c(4.1, 4.1, 0, 0), las = 1)
plot(rep(xx, 3), c(y2, u2, l2), type = 'n', xaxt ='n', ylab = "", xlab = "")
points(xx, y2, col = cc, pch = 16)
segments(xx, l2, xx, u2, col = cc)
legend("bottomleft", legend = "LATITUDE", bty = 'n')

dseq <- seq.POSIXt(
	as.POSIXct(paste(as.Date(min(xx)), "00:00:00"), tz = "UTC"),
	as.POSIXct(paste(as.Date(max(xx)) + 1, "00:00:00"), tz = "UTC"),
	by = "day"
)

axis.POSIXct(1, at = dseq, format = "%d%b", las = 2, tcl = '-0.75')


plot(1, 1, type = 'n', axes = FALSE, xlab = "", ylab = "")
legend("center", legend = unique(cc), col = unique(cc), pch = rep(16, length(unique(cc))))




#rworldmap
library(rworldmap)
worldmap <- getMap(resolution = "high")
plot(c(y1, l1, u1), c(y2, l2, u2), type = 'n')
user <- par()$usr
plot(worldmap, xlim = user[1:2], ylim = user[3:4])
points(y1, y2,  col = cc, pch = 16)
segments(y1, l2, y1, u2, col = cc)
segments(l1, y2, u1, y2, col = cc)

plot(worldmap, xlim = user[1:2], ylim = user[3:4])
points(y1, y2,  col = cc, pch = 16)
for(i in 1:length(y1)) {
	points(y1[i], y2[i], pch = 16, col = "red")
	readline(prompt="Press [enter] to continue")
}
