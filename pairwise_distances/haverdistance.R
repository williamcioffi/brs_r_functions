haverdist <- function(lat1, lon1, lat2, lon2) {
	r <- 6371
	lat1 <- deg2rad(lat1)
	lat2 <- deg2rad(lat2)
	lon1 <- deg2rad(lon1)
	lon2 <- deg2rad(lon2)

	dlon <- lon2 - lon1
	dlat <- lat2 - lat1
	
	a = sin(dlat/2) * sin(dlat/2) + cos(lat1) * cos(lat2) * sin(dlon/2) * sin(dlon/2)
	c = 2 * atan2(sqrt(a), sqrt(1-a))
	
	r * c
}

deg2rad <- function(deg) {
	deg*pi/180
}