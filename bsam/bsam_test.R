#bsam test

library(bsam)

datadir <- "~/Downloads/tmp/batch_downloaded_20170915_0051UTC/"
source("~/git/brs_r_functions/basic_tag_functions/cattag.r")
source("~/git/brs_r_functions/basic_tag_functions/load.R")
cattag(datadir)
streams <- loadtag(datadir)
argos <- streams$argos
argos <- argos[-which(argos$LocationQuality == ""), ]
zcs <- grep("Zc", argos$DeployID)
argos <- argos[zcs, ]

movedat <- data.frame(id = argos$DeployID, date = argos$Date, lc = argos$LocationQuality, lon = argos$Longitude, lat = argos$Latitude)

fit <- fit_ssm(movedat, model = "DCRW", tstep = 1, adapt = 5000, samples = 5000, 
              thin = 5, span = 1) 

# map_ssm(fit)
# diag_ssm(fit)
# plot_fit(fit)

results <- get_summary(fit)
xx <- as.POSIXct(results$date, tz = "UTC")
cc <- results$id

dese <- which(xx > "2017-08-17")
st <- as.POSIXct("2017-09-12 16:00:00 UTC", tz = "UTC")
en <- as.POSIXct("2017-09-12 17:00:00 UTC", tz = "UTC")

xx <- xx[dese]
cc <- cc[dese]

x11()
par(mfrow = c(2, 1), mar = c(rep(0, 4)), oma = c(4.1, 4.1, 0, 0), las = 1)
y1 <- results$lon[dese]
l1 <- results$lon.025[dese]
u1 <- results$lon.975[dese]
  
plot(xx, y1, col = cc, pch = 16, xaxt = 'n')
segments(xx, l1, xx, u1, col = cc)
legend("topleft", legend = unique(cc), col = unique(cc), pch = rep(16, length(unique(cc))))
legend("bottomleft", legend = "LONGITUDE", bty = 'n')

y2 <- results$lat[dese]
l2 <- results$lat.025[dese]
u2 <- results$lat.975[dese]

plot(xx, y2, col = cc, pch = 16)
segments(xx, l2, xx, u2, col = cc)
legend("topleft", legend = unique(cc), col = unique(cc), pch = rep(16, length(unique(cc))))
legend("bottomleft", legend = "LATITUDE", bty = 'n')


x11()
plot(y1, y2, col = cc, pch = 16)
segments(y1, l2, y1, u2, col = cc)
segments(l1, y2, u1, y2, col = cc)

plot(y1, y2, col = cc, pch = 16, ylim = c(35, 36), xlim = c(-75.25, -74))
segments(y1, l2, y1, u2, col = cc)
segments(l1, y2, u1, y2, col = cc)

