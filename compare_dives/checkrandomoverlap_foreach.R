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

	consecutive <- vector(mode = "numeric", length = nsim)
	alignedevents <- vector(mode = "numeric", length = nsim)
	for(s in 1: nsim) {
		n1 <- build_null_diver_vectorized(b1, deployid = "n1")
		n2 <- build_null_diver_vectorized(b2, deployid = "n2")
		com <- compare_dives(n1, n2)
		tdif <- abs(com$diff_times)
		sorl <- vector(mod = "logical", length = length(tdif))
		sorl[which(tdif <= 60)] <- TRUE
		sorl[which(tdif >  60)] <- FALSE
		
		st <- seq(1, length(sorl) - nconsec - 1)
		en <- seq(nconsec, length(sorl))
		
		concheck <- list()
		
		for(p in 1:(length(sorl) - nconsec - 1)) {
			concheck[[p]] <- sorl[st[p]:en[p]]
		}
		
		consecutive[s] <- length(which(sapply(concheck, all)))
		alignedevents[s] <- length(which(sorl))
	}
	
	list(deployids = paste(b1$DeployID[1], b2$DeployID[1], sep = '-'), consecutive = consecutive, alignedevents = alignedevents)
}

stopCluster(cl)

par(mfrow = c(5, 2), mar = c(3.1, 0, 0, 0))
for(i in 1:ntagpairs) {
	b1 <- beh[[tagpairs[i, 1]]]
	b2 <- beh[[tagpairs[i, 2]]]
	
	com <- compare_dives(b1, b2, cliptime = TRUE)
	length(which(abs(com$diff_times) <= 60)) / length(com$diff_times)
	plot(density(outs[[i]]$alignedevents / length(com$diff_times)), xlim = c(0, 1), bty = 'n', yaxt = 'n', xlab = "", main = "", ylab = "")
	abline(v = length(which(abs(com$diff_times) <= 60)) / length(com$diff_times), lty = 2)
	legend("topright", legend = paste(names(beh)[tagpairs[i, 1]], names(beh)[tagpairs[i, 2]], sep = "-"))
	# abline(v = quantile(outs[[i]]$alignedevents / length(com$diff_times), 0.95), col = "purple")
}