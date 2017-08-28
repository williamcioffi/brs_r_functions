#bsam test

library(bsam)

ff <- list.files()
dese <- grep("-Argos", ff)
ff <- ff[dese]
nf <- length(ff)

argos <- read.table(ff, header = TRUE, sep =',', stringsAsFactors = FALSE)
argos <- argos[-which(argos$LocationQuality == ""), ]
argos$Date <- strptime(argos$Date, format = "%H:%M:%S %d-%b-%Y", tz = "UTC")

# kill <- -which(argos$DeployID == "ZcTag063")[which(argos$Date[which(argos$DeployID == "ZcTag063")] < "2017-08-01 00:00:00 UTC")]
# argos <- argos[kill, ]

# friendnames <- unique(argos$DeployID)
# dese <- c(2, 4, 5, 7) #7, 2, 4, 5
# a <- argos[which(argos$DeployID %in% friendnames[dese]), ]

movedat <- data.frame(id = argos$DeployID, date = argos$Date, lc = argos$LocationQuality, lon = argos$Longitude, lat = argos$Latitude)

fit <- fit_ssm(movedat, model = "DCRW", tstep = 1, adapt = 5000, samples = 5000, 
              thin = 5, span = 1)

# map_ssm(fit)
# diag_ssm(fit)
# plot_fit(fit)

results <- get_summary(fit)
xx <- as.POSIXct(results$date, tz = "UTC")
cc <- results$id

par(mfrow = c(2, 1), mar = c(rep(0, 4)), oma = c(4.1, 4.1, 0, 0), las = 1)
y1 <- results$lon
l1 <- results$lon.025
u1 <- results$lon.975

plot(xx, y1, col = cc, pch = 16, xaxt = 'n')
segments(xx, l1, xx, u1, col = cc)
legend("topright", legend = unique(cc), col = unique(cc), pch = rep(16, length(unique(cc))))
legend("topleft", legend = "LONGITUDE", bty = 'n')

y2 <- results$lat
l2 <- results$lat.025
u2 <- results$lat.975

plot(xx, y2, col = cc, pch = 16)
segments(xx, l2, xx, u2, col = cc)
legend("topright", legend = unique(cc), col = unique(cc), pch = rep(16, length(unique(cc))))
legend("topleft", legend = "LATITUDE", bty = 'n')



plot(y1, y2, col = cc, pch = 16)
segments(y1, l2, y1, u2, col = cc)
segments(l1, y2, u1, y2, col = cc)

plot(y1, y2, col = cc, pch = 16, ylim = c(35, 36), xlim = c(-75.25, -74))
segments(y1, l2, y1, u2, col = cc)
segments(l1, y2, u1, y2, col = cc)

