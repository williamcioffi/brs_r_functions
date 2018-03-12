# yy <- predict(loess(abs(nullcompare$diff_times) ~ as.numeric(nullcompare$b1_ens)))
# lines(nullcompare$b1_ens, yy)
# yy2 <- predict(loess(abs(realcompare$diff_times) ~ as.numeric(realcompare$b1_ens)))
# lines(realcompare$b1_ens, yy2, col = "red")




# xx <- 1:206
# y1 <- abs(realcompare$diff_times)
# y0 <- abs(nullcompare$diff_times[xx])

# plot(c(xx, xx), c(y0, y1), type = 'n')
# points(xx, y1, col = "red")
# points(xx, y0)

# ypred0 <- predict(loess(y0 ~ xx))
# ypred1 <- predict(loess(y1 ~ xx, family = "symmetric"))

# lines(xx, ypred0, lty = 2)
# lines(xx, ypred1, col = "red")



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
realcompare <- compare_dives(beh[[1]],  beh[[3]])

#make a thousand
nsim <- 10
nullcompare <- list()
areas <- list()


pb <- txtProgressBar(style = 3)
for(i in 1:nsim) {
setTxtProgressBar(pb, i/nsim)
	n1 <- build_null_diver(beh[[1]], deployid = "n1")	
	n2 <- build_null_diver(beh[[2]], deployid = "n2")
	
	nullcompare[[i]] <- compare_dives(n1, n2)
	areas[[i]] <- calc_overlap(abs(nullcompare[[i]]$diff_time), abs(realcompare$diff_time))
}
close(pb)


nulldiff <- lapply(nullcompare, '[[', 2)
nulldiffdens <- lapply(nulldiff, function(l) density(abs(l)))
nulldiffdensx <- lapply(nulldiffdens, function(l) l$x)
minx <- do.call('min', nulldiffdensx)
maxx <- do.call('max', nulldiffdensx)
nulldiffdens <- lapply(nulldiff, function(l) density(abs(l), from = minx, to = maxx))
medianx <- lapply(nulldiffdens, function(l) l$x)[[1]]
nulldiffdensy <- lapply(nulldiffdens, function(l) l$y)
nulldiffdensy <- do.call('cbind', nulldiffdensy)
mediany <- apply(nulldiffdensy, 1, median)

plot(density(abs(realcompare$diff_times)), xlim = c(minx, maxx))
#do this with or without replacement?
lapply(nullcompare[sample(1:1000, 100, replace = FALSE)], function(l) lines(density(abs(l$diff_times)), col = rgb(0, 0, 0, .15)))

lines(medianx, mediany, lty = 2, col = "purple")
areas <- sapply(areas, '[[', 1)
areasmd <- median(areas)
areaslo <- quantile(areas, 0.025)
areashi <- quantile(areas, 0.975)

# # dev.new() 
# plot(0, areasmd, ylim = c(0, 1), xaxt = 'n', las = 1)
# segments(0, areaslo, 0, areashi)