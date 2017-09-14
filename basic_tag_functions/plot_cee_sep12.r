###
# look at CEE
#

datadir <- "~/Downloads/tmp/batch_downloaded_20170914_1816UTC"

source("~/git/brs_r_functions/basic_tag_functions/findgaps.R")
source("~/git/brs_r_functions/basic_tag_functions/plot_dives.r")
source("~/git/brs_r_functions/basic_tag_functions/cattag.r")
source("~/git/brs_r_functions/basic_tag_functions/load.R")

cattag(datadir)
streams <- loadtag(datadir)
beh <- streams$behavior

friends <- sort(unique(beh$DeployID))
zcs <- friends[grep("Zc", friends)]
gms <- friends[grep("Gm", friends)]

cst <- as.POSIXct("2017-09-12 16:00:00 UTC", tz = "UTC")
cen <- as.POSIXct("2017-09-12 17:00:00 UTC", tz = "UTC")
x11()

# centered on cee
for(i in 1:length(zcs)) {
pdf(paste0(zcs[i], "-zoom.pdf"))
plot_dives(beh, start_time = "2017-09-11 16:00:00", end_time = "2017-09-13 16:00:00", show_gaps = TRUE, deploy_ids = zcs[i], col = "black")
rect(cst, 0, cen, -4000, col = rgb(0, 0, 0, .5), border = NA)
dev.off()
}

# since sep 05
for(i in 1:length(zcs)) {
  pdf(paste0(zcs[i], "-since05sep.pdf"))
  plot_dives(beh, start_time = "2017-09-05", end_time = "2017-09-15", show_gaps = TRUE, deploy_ids = zcs[i], col = "black")
  rect(cst, 0, cen, -4000, col = rgb(0, 0, 0, .5), border = NA)
  dev.off()
}


# check lengths
x11()
b <- beh[which(beh$What == "Dive"), ]
plot(b$DurationMax, b$DepthMax, pch = 16, cex = 0.5, col = factor(b$DeployID))
b[which(b$DurationMax > 10000),]
legend("topright", legend = unique(factor(b$DeployID)), col = unique(factor(b$DeployID)), pch = rep(16, 10))
