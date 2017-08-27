#tag_diagnostics

plot_status <- function(status, ycolname, start_time = NULL, end_time = NULL, show_minutes = FALSE, deploy_ids = NULL) {

if(!is.null(deploy_ids)) {
	dese <- which(status$DeployID %in% deploy_ids)
	status <- status[dese, ]
}
	
ps <- prepstatus(status, start_time, end_time)

col <- which(colnames(ps$s_ordered) == ycolname)
yy <- ps$s_ordered[, col]
xx <- as.POSIXct(ps$s_ordered$Received, tz = "UTC")

plot(xx, yy,
	axes = FALSE, las = 1,
	xlab = "", ylab = "",
	type = 'n', 
	xlim = c(ps$zoom1, ps$zoom2)
)

axis(2, las = 1)
axis.POSIXct(1, at = ps$dseq, format = "%d%b", las = 2, tcl = '-0.75')
axis.POSIXct(1, at = ps$tseq, labels = FALSE)

if(show_minutes) axis.POSIXct(1, at = ps$mseq, labels = FALSE)

for(i in 1:length(ps$slist)) {
	xx <- as.POSIXct(ps$slist[[i]]$Received, tz = "UTC")
	yy <- ps$slist[[i]][, col]
	kill <- -which(is.na(yy))
	if(length(kill) > 0) {
		xx <- xx[kill]
		yy <- yy[kill]
	}
	points(xx, yy, pch = ps$pches[i], col = ps$colors_dark[i], cex = ps$cex[i])
	lines(xx, yy, col = ps$colors_dark[i])
}

taglabs <- sapply(ps$slist, function(l) as.character(l$DeployID[1]))
ntags <- length(taglabs)
legend("topleft", legend = taglabs, pch = ps$pches[1:ntags], lty = c(rep(1, ntags)), col = c(ps$colors_dark[1:ntags]), bty = 'n')

}






plot_corrupt <- function(corrupt, start_time = NULL, end_time = NULL, show_minutes = FALSE, behavior_only = TRUE, deploy_ids = NULL) {

if(!is.null(deploy_ids)) {
	dese <- which(corrupt$DeployID %in% deploy_ids)
	corrupt <- corrupt[dese, ]
}

corrupt <- corrupt[order(corrupt$Date), ]
friendnames <- sort(unique(corrupt$DeployID))
c_oo <- order(corrupt$DeployID)
c_ordered <- corrupt[c_oo, ]
clist <- split(c_ordered, as.character(c_ordered$DeployID))


tt <- as.character(c_ordered$Date)

if(is.null(start_time)) {
	zoom1 <- as.POSIXct(min(tt), tz = "UTC")
} else {
	zoom1 <- as.POSIXct(start_time, tz = "UTC")
}

if(is.null(end_time)) {
	zoom2 <- as.POSIXct(max(tt), tz = "UTC")
} else {
	zoom2 <- as.POSIXct(end_time, tz = "UTC")
}

if(behavior_only) {
	behtype <- lapply(clist, function(l) l[which(l$Possible.Type == "Behavior"), ])
} else {
	behtype <- clist
}

cumcount <- lapply(behtype, function(l) 1:nrow(l))
cumdates <- lapply(behtype, function(l) as.character(l$Date))
xx <- as.POSIXct(unlist(cumdates), tz = "UTC")
yy <- unlist(cumcount)

plot(xx, yy,
	axes = FALSE, las = 1,
	xlab = "", ylab = "",
	type = 'n', 
	xlim = c(zoom1, zoom2)
)

tseq <- seq.POSIXt(
	as.POSIXct(paste(as.Date(min(tt)), "00:00:00"), tz = "UTC"),
	as.POSIXct(paste(as.Date(max(tt)) + 1, "00:00:00"), tz = "UTC"),
	by = "hour"
)
	
dseq <- seq.POSIXt(
	as.POSIXct(paste(as.Date(min(tt)), "00:00:00"), tz = "UTC"),
	as.POSIXct(paste(as.Date(max(tt)) + 1, "00:00:00"), tz = "UTC"),
	by = "day"
)

mseq <- seq.POSIXt(
	as.POSIXct(paste(as.Date(min(tt)), "00:00:00"), tz = "UTC"),
	as.POSIXct(paste(as.Date(max(tt)) + 1, "00:00:00"), tz = "UTC"),
	by = "min"
)

axis.POSIXct(1, at = dseq, format = "%d%b", las = 2, tcl = '-0.75')
axis.POSIXct(1, at = tseq, labels = FALSE)

if(show_minutes) axis.POSIXct(1, at = mseq, labels = FALSE)


colorss <- c(
	rgb(1, 0, 0, .33),
	rgb(0, 1, 0, .33),
	rgb(0, 0, 1, .33),
	rgb(1, 1, 0, .33),
	rgb(0, 1, 1, .33),
	rgb(1, 0, 1, .33),
	rgb(0, 0, 0, .33),
	rgb(.5, .5, 0, .33),
	rgb(.5, 0, .5, .33),
	rgb(0, .5, .5, .33)
)
			
colors_dark <- c(
	rgb(1, 0, 0),
	rgb(0, 1, 0),
	rgb(0, 0, 1),
	rgb(1, 1, 0),
	rgb(0, 1, 1),
	rgb(1, 0, 1),
	rgb(0, 0, 0),
	rgb(.5, .5, 0),
	rgb(.5, 0, .5),
	rgb(0, .5, .5)
)
			
pches <- c(15, 16, 17, 18, 19, 21, 22, 23, 24, 25)
cexes <- rep(1, 10)


for(i in 1:length(behtype)) {
	xx <- as.POSIXct(behtype[[i]]$Date, tz = "UTC")
	yy <- cumcount[[i]]
	points(xx, yy, pch = pches[i], col = colors_dark[i], cex = .5)
	lines(xx, yy, col = colors_dark[i])
}

taglabs <- sapply(behtype, function(l) as.character(l$DeployID[1]))
ntags <- length(taglabs)
legend("topleft", legend = taglabs, pch = pches[1:ntags], lty = c(rep(1, ntags)), col = c(colors_dark[1:ntags]), bty = 'n')

}







prepstatus <- function(status, start_time, end_time) {
	status <- status[order(status$Received), ]
	# friendnames <- sort(unique(status$DeployID))
	s_oo <- order(status$DeployID)
	s_ordered <- status[s_oo, ]
	slist <- split(s_ordered, as.character(s_ordered$DeployID))
	
	
	tt <- as.character(s_ordered$Received)

	if(is.null(start_time)) {
		zoom1 <- as.POSIXct(min(tt), tz = "UTC")
	} else {
		zoom1 <- as.POSIXct(start_time, tz = "UTC")
	}

	if(is.null(end_time)) {
		zoom2 <- as.POSIXct(max(tt), tz = "UTC")
	} else {
		zoom2 <- as.POSIXct(end_time, tz = "UTC")
	}
	
	tseq <- seq.POSIXt(
		as.POSIXct(paste(as.Date(min(tt)), "00:00:00"), tz = "UTC"),
		as.POSIXct(paste(as.Date(max(tt)) + 1, "00:00:00"), tz = "UTC"),
		by = "hour"
	)
		
	dseq <- seq.POSIXt(
		as.POSIXct(paste(as.Date(min(tt)), "00:00:00"), tz = "UTC"),
		as.POSIXct(paste(as.Date(max(tt)) + 1, "00:00:00"), tz = "UTC"),
		by = "day"
	)
	
	mseq <- seq.POSIXt(
		as.POSIXct(paste(as.Date(min(tt)), "00:00:00"), tz = "UTC"),
		as.POSIXct(paste(as.Date(max(tt)) + 1, "00:00:00"), tz = "UTC"),
		by = "min"
	)

		
	colorss <- c(
		rgb(1, 0, 0, .33),
		rgb(0, 1, 0, .33),
		rgb(0, 0, 1, .33),
		rgb(1, 1, 0, .33),
		rgb(0, 1, 1, .33),
		rgb(1, 0, 1, .33),
		rgb(0, 0, 0, .33),
		rgb(.5, .5, 0, .33),
		rgb(.5, 0, .5, .33),
		rgb(0, .5, .5, .33)
	)
				
	colors_dark <- c(
		rgb(1, 0, 0),
		rgb(0, 1, 0),
		rgb(0, 0, 1),
		rgb(1, 1, 0),
		rgb(0, 1, 1),
		rgb(1, 0, 1),
		rgb(0, 0, 0),
		rgb(.5, .5, 0),
		rgb(.5, 0, .5),
		rgb(0, .5, .5)
	)
				
	pches <- c(15, 16, 17, 18, 19, 21, 22, 23, 24, 25)
	cexes <- rep(1, 10)

	list(s_ordered = s_ordered, slist = slist, tseq = tseq, dseq = dseq, mseq = mseq, pches = pches, cexes = cexes, tt = tt, colors_dark = colors_dark, colorss = colorss, zoom1 = zoom1, zoom2 = zoom2)
}