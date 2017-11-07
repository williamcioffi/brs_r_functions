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
beh <- streams$behavior
beh <- beh[grep("ZcTag", beh$DeployID), ]

beh <- split(beh, beh$DeployID)

n1 <- build_null_diver(beh[[1]], deployid = "n1")	
n2 <- build_null_diver(beh[[2]], deployid = "n2")

realcompare <- compare_dives(beh[[1]],  beh[[2]])
nullcompare <- compare_dives(n1, n2)

lay <- matrix(c(1, 1, 1, 2), 1, 4)
layout(lay)
plot(c(nullcompare$b1_ens, realcompare$b1_ens), c(abs(nullcompare$diff_times), abs(realcompare$diff_times)))
lims <- par()$usr
plot(nullcompare$b1_ens, abs(nullcompare$diff_times), ylim = lims[3:4])
points(realcompare$b1_ens, abs(realcompare$diff_times), col = "red")
nulldens <- density(abs(nullcompare$diff_times))
realdens <- density(abs(realcompare$diff_times))
nullxx <- nulldens$x
nullyy <- nulldens$y
realxx <- realdens$x
realyy <- realdens$y

plot(c(realyy, nullyy), c(realxx, nullxx), type = 'n', ylim = lims[3:4])
lines(realyy, realxx, col = "red")
lines(nullyy, nullxx)


nulls <- rbind(n1, n2)
names(nulls)
nulls$Start <- as.character(nulls$Start)
nulls$End <- as.character(nulls$End)

reals <- do.call('rbind', beh[c(1, 2)])
reals$Shape[which(reals$Shape != "")] <- "U"

# # dev.new()

# par(mfrow = c(2, 1), mar = c(0, 0, 0, 0))
# plot_dives(reals, start_time = "2017-08-19", end_time = "2017-08-21", hidelegend = TRUE)
# plot_dives(nulls, start_time = "2017-08-19", end_time = "2017-08-21", hidelegend = TRUE)

# length(which(reals$What == "Dive"))
# length(which(nulls$What == "Dive"))

