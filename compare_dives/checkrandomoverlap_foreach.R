# library(doMC)
# registerDoMC(cores = 4)


### only need to do this on windows because it deosn't know what an unambiguous format is
# beh <- lapply(beh, function(l) {
	# l$Start <- as.POSIXct(l$Start, format = "%H:%M:%S %d-%b-%Y", tz = "UTC")
	# l$End <- as.POSIXct(l$End  , format = "%H:%M:%S %d-%b-%Y", tz = "UTC")
	# l
# })

library(doParallel)
cl <- makeCluster(detectCores())
registerDoParallel(cl)

starts <- Sys.time()
outs <- foreach(n = 1:1000) %dopar% {
	n1 <- build_null_diver_vectorized(beh[[8]], deployid = "n1")
	n2 <- build_null_diver_vectorized(beh[[9]], deployid = "n2")
	com <- compare_dives(n1, n2)
	tdif <- abs(com$diff_times)
	sorl <- vector(mod = "logical", length = length(tdif))
	sorl[which(tdif <= 60)] <- TRUE
	sorl[which(tdif >  60)] <- FALSE
	
	n <- 4
	st <- seq(1, length(sorl) - n - 1)
	en <- seq(n, length(sorl))
	
	checks <- list()
	
	for(i in 1:(length(sorl) - n - 1)) {
		checks[[i]] <- sorl[st[i]:en[i]]
	}
	
	length(which(sapply(checks, all)))
}
Sys.time() - starts

stopCluster(cl)