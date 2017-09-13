#find gaps


findgaps <- function(behavior) {
	MAX_ALLOWED_DIFF <- 60
	
	wh <- behavior$What
	
	st <- behavior$Start [-which(wh == "Message")]
	en <- behavior$End	  [-which(wh == "Message")]
	n  <- length(st)
	
	diffs <- difftime(st[2:n], en[1:(n - 1)], units = "secs")
	desegaps <- which(abs(diffs) > MAX_ALLOWED_DIFF)
	gap_st <- en[desegaps]
	gap_en <- st[desegaps + 1]
	ngaps  <- length(desegaps)
	
	if(ngaps == 0) {
		stretch <- rep(1, n)
	} else {
		stretch <- 1:n*NA
		posgap <- desegaps + 1
		
		stretch[1:desegaps[1]] <- 1
		
		if(ngaps > 1) {
			for(i in 2:ngaps) {
				stretch[(desegaps[i - 1] + 1):desegaps[i]] <- i
			}
		}
		
		stretch[(desegaps[ngaps] + 1):n] <- ngaps + 1
	}
	
	list(deploy_id = unique(behavior$DeployID), ngaps = ngaps, gap_st = gap_st, gap_en = gap_en, gap_diffs = diffs, stretchid = stretch)
}