# censor_dives
# should i use min or max or mean duration as the cut off?
# which one is actually used in the tag

censor_dives <- function(b, dur_cutoff = 33*60) {
	b.out <- b
	dur <- apply(b[, c('DepthMin', 'DepthMax')], 1, mean)
	shorts <- which(dur < dur_cutoff)
	b.out$What[shorts] <- "Surface"
	b.out$DepthMin <- NA
	b.out$DepthMax <- NA
	
	index <- 1
	while(index < nrow(b.out)) {
		
	}
}