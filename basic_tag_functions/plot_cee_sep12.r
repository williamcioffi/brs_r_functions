###
# look at CEE
#

#cattag
#load

friends <- sort(unique(beh$DeployID))
zcs <- friends[grep("Zc", friends)]
gms <- friends[grep("Gm", friends)]

cst <- as.POSIXct("2017-09-12 16:00:00 UTC", tz = "UTC")
cen <- as.POSIXct("2017-09-12 17:00:00 UTC", tz = "UTC")
x11()
plot_dives(beh, start_time = "2017-09-12 11:00:00", end_time = "2017-09-13 11:00:00", show_gaps = TRUE, deploy_ids = zcs)
rect(cst, 0, cen, -4000, col = rgb(0, 0, 0, .5), border = NA)


x11()
par(mfrow = c(3, 3))
for(i in 1:length(zcs)) {
  plot_dives(beh, deploy_ids = zcs[i], show_gaps = TRUE, start_time = "2017-09-05")
  rect(cst, 0, cen, -4000, col = rgb(0, 0, 0, .5), border = NA)
}


# check lengths
x11()
b <- beh[which(beh$What == "Dive"), ]
plot(b$DurationMax, b$DepthMax, pch = 16, cex = 0.5, col = factor(b$DeployID))
b[which(b$DurationMax > 10000),]
legend("topright", legend = unique(factor(b$DeployID)), col = unique(factor(b$DeployID)), pch = rep(16, 10))
