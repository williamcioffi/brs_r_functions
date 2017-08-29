#count the number of days for each tag

friends <- alm$DeployID
ufriend <- sort(unique(friends))
nfriend <- length(ufriend)

timediff <- vector(length = nfriend)

for(i in 1:nfriend) {
	curfriend <- ufriend[i]
	dese <- which(friends == curfriend)
	alm_cur <- alm[dese, ]
	
	st <- sort(alm_cur$Msg.Date)[1]
	en <- sort(alm_cur$Msg.Date)[nrow(alm_cur)]
	
	timediff[i] <- difftime(en, st, units = "day")
}