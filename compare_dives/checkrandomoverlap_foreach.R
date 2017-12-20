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
outs <- list()
nsim <- 1000

clusterExport(cl, ls())
	
outs <- foreach(i = 1:ntagpairs, .combine = c) %dopar% {
	b1 <- beh[[tagpairs[i, 1]]]
	b2 <- beh[[tagpairs[i, 2]]]

	outchecks <- vector(mode = "numeric", length = nsim)
	for(sim in 1: nsim) {
		n1 <- build_null_diver_vectorized(b1, deployid = "n1")
		n2 <- build_null_diver_vectorized(b2, deployid = "n2")
		com <- compare_dives(n1, n2)
		tdif <- abs(com$diff_times)
		sorl <- vector(mod = "logical", length = length(tdif))
		sorl[which(tdif <= 60)] <- TRUE
		sorl[which(tdif >  60)] <- FALSE
		
		n <- 4
		st <- seq(1, length(sorl) - n - 1)
		en <- seq(n, length(sorl))
		
		checks <- list()
		
		for(p in 1:(length(sorl) - n - 1)) {
			checks[[p]] <- sorl[st[p]:en[p]]
		}
		
		outchecks[sim] <- length(which(sapply(checks, all)))
	}
	
	list(outchecks)
}

stopCluster(cl)