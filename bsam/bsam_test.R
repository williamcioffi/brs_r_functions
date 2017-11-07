#bsam test

library(bsam)

datadir <- "~/Desktop/nulltest"
savedir <- "~/Desktop/nulltest"

source("~/git/brs_r_functions/basic_tag_functions/findgaps.R")
source("~/git/brs_r_functions/basic_tag_functions/plot_dives.r")
source("~/git/brs_r_functions/basic_tag_functions/cattag.r")
source("~/git/brs_r_functions/basic_tag_functions/load.R")

source("~/git/brs_r_functions/compare_dives/compare_dives.R")
source("~/git/brs_r_functions/compare_dives/build_null_diver.R")

# cattag(datadir)
streams <- loadtag(datadir)
argos <- streams$argos

argos <- argos[which(argos$LocationQuality %in% c("0", "1", "2", "3", "A", "B", "Z")), ]
argos <- argos[which(argos$DeployID == "ZcTag057"),]

movedat <- data.frame(id = argos$DeployID, date = argos$Date, lc = argos$LocationQuality, lon = argos$Longitude, lat = argos$Latitude)

fit <- fit_ssm(movedat, model = "DCRW", tstep = 1, adapt = 5000, samples = 5000, 
              thin = 5, span = 1) 

# map_ssm(fit)
# diag_ssm(fit)
# plot_fit(fit)

results <- get_summary(fit)
xx <- as.POSIXct(results$date, tz = "UTC")
cc <- results$id

# dese <- which(xx > "2017-08-17")
# st <- as.POSIXct("2017-09-12 16:00:00 UTC", tz = "UTC")
# en <- as.POSIXct("2017-09-12 17:00:00 UTC", tz = "UTC")

# xx <- xx[dese]
# cc <- cc[dese]

par(mfrow = c(2, 1), mar = c(rep(0, 4)), oma = c(4.1, 4.1, 0, 0), las = 1)
y1 <- results$lon
l1 <- results$lon.025
u1 <- results$lon.975
  
plot(rep(xx, 3), c(y1, u1, l1), xaxt = 'n', type = 'n')
points(xx, y1, col = cc, pch = 16)
segments(xx, l1, xx, u1, col = cc)
legend("topleft", legend = unique(cc), col = unique(cc), pch = rep(16, length(unique(cc))))
legend("bottomleft", legend = "LONGITUDE", bty = 'n')

y2 <- results$lat
l2 <- results$lat.025
u2 <- results$lat.975

plot(rep(xx, 3), c(y2, u2, l2), xaxt = 'n', type = 'n')
points(xx, y2, col = cc, pch = 16)
segments(xx, l2, xx, u2, col = cc)
legend("topleft", legend = unique(cc), col = unique(cc), pch = rep(16, length(unique(cc))))
legend("bottomleft", legend = "LATITUDE", bty = 'n')

library(rworldmap)
worldmap <- getMap(resolution = "high")
plot(c(y1, l1, u1), c(y2, l2, u2), type = 'n')
user <- par()$usr
plot(worldmap, xlim = user[1:2], ylim = user[3:4])
points(y1, y2,  col = cc, pch = 16)
segments(y1, l2, y1, u2, col = cc)
segments(l1, y2, u1, y2, col = cc)

plot(worldmap, xlim = user[1:2], ylim = user[3:4])
for(i in 1:length(y1)) {
	points(y1[i], y2[i], pch = 16, col = "red")
	readline(prompt="Press [enter] to continue")
}

