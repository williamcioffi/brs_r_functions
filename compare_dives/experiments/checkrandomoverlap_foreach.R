# 20171221
# ~ wrc

# library(doMC)
# registerDoMC(cores = 4)

beh.backup <- beh
beh <- beh[grep("ZcTag", beh$DeployID), ]
beh <- split(beh, beh$DeployID)

### only need to do this on windows because it deosn't know what an unambiguous format is
# beh <- lapply(beh, function(l) {
	# l$Start <- as.POSIXct(l$Start, format = "%H:%M:%S %d-%b-%Y", tz = "UTC")
	# l$End <- as.POSIXct(l$End  , format = "%H:%M:%S %d-%b-%Y", tz = "UTC")
	# l
# })

library(doParallel)
cl <- makeCluster(detectCores())
registerDoParallel(cl)

ntags <- length(beh)
ntagpairs <- ntags*(ntags-1) / 2
tagpairs <- t(combn(1:ntags, 2))

starts <- Sys.time()
nsim <- 1000
nconsec <- 4 # the number of consecutive time points to count

clusterExport(cl, ls())
	
outs <- foreach(i = 1:ntagpairs) %dopar% {
	b1 <- beh[[tagpairs[i, 1]]]
	b2 <- beh[[tagpairs[i, 2]]]
	
	# calculate real overlap
	com <- compare_dives(b1, b2)
	tdif <- abs(com$diff_times)
	sorl <- (tdif <= 60)

	st <- seq(1, length(sorl) - (nconsec - 1))
	en <- seq(nconsec, length(sorl))

tryCatch({
	for(p in 1:(length(sorl) - (nconsec - 1))) {
		concheck[[p]] <- sorl[st[p]:en[p]]
	}
	consecutive <- length(which(sapply(concheck, all)))
}, error = function(e) { 
	warning("it seems like these animals might not overlap") 
}, finaly = {
	consecutive <- NA
})
	alignedevents <- length(which(sorl))
				
	# calculate null overlap
	consecutive_sim <- vector(mode = "numeric", length = nsim)
	alignedevents_sim <- vector(mode = "numeric", length = nsim)
	ndiff_sim <- vector(mode = "numeric", length = nsim)
	
	for(s in 1: nsim) {
		n1 <- build_null_diver_vectorized(b1, deployid = "n1")
		n2 <- build_null_diver_vectorized(b2, deployid = "n2")
		
		com_sim <- compare_dives(n1, n2)
		tdif_sim <- abs(com_sim$diff_times)
		sorl_sim <- (tdif_sim <= 60)
		
		st_sim <- seq(1, length(sorl_sim) - (nconsec - 1))
		en_sim <- seq(nconsec, length(sorl_sim))
		
		concheck_sim <- list()
		concheck <- list()
tryCatch({		
		for(p in 1:(length(sorl_sim) - (nconsec - 1))) {
			concheck_sim[[p]] <- sorl_sim[st_sim[p]:en_sim[p]]
		}
				
		consecutive_sim[s] <- length(which(sapply(concheck_sim, all)))
}, error = function(e) { 
	warning("it seems like these animals might not overlap") 
}, finaly = {
	consecutive_sim[s] <- NA
})

	alignedevents_sim[s] <- length(which(sorl_sim))
	ndiff_sim[s] <- length(com_sim$diff_times)
	}
	
	list(deployids = paste(b1$DeployID[1], b2$DeployID[1], sep = '-'), consecutive_sim = consecutive_sim, alignedevents_sim = alignedevents_sim, ndiff_sim = ndiff_sim, consecutive = consecutive, alignedevents = alignedevents, ndiff = length(com$diff_times))
}

stopCluster(cl)


# visualize
dev.new()
par(mfrow = c(6, 6), mar = c(3.1, 0, 0, 0))
for(i in (1:ntagpairs)) {
	o <- outs[[i]]
	
	if(o$ndiff != 1) {
		plot(density(o$alignedevents_sim / o$ndiff_sim), xlim = c(0, 0.2), bty = 'n', yaxt = 'n', xlab = "", main = "", ylab = "")
		abline(v = o$alignedevents / o$ndiff, lty = 2)
	} else {
		plot(0, 0, type = 'n', axes = FALSE, xlab = "", ylab = "")
	}
	
	legend("topright", legend = paste(names(beh)[tagpairs[i, 1]], names(beh)[tagpairs[i, 2]], sep = "-"), bty = 'n')
	
	if(!is.na(o$consecutive) & o$consecutive > 0) {
		legend("bottomright", legend = paste(o$consecutive), text.col = "purple", bty = 'n')
	}
}
