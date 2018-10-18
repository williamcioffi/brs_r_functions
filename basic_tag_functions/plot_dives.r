###
# plot_dives
# wrc 20170911

plot_dives2 <- function(
	b,
	depth_lim = NULL,
	start_time = NULL,
	end_time = NULL,
	tz = "UTC", 
	show_gaps = TRUE,
	show_shape = TRUE,
	show_minutes = FALSE,
	show_hours = FALSE,
	col = NULL,
	pch = NULL,
	lty = 1,
	lwd = 1,
	cex = 1,
	hidelegend = FALSE,
	legendpos = "bottomright",
	ylab = "depth (meters)",
	xaxt = TRUE,
	gap_plotting_buffer = 0.15,
	dep_plotting_buffer = 0.15
) {

require(colorspace)

# constants  
UNIX_EPOCH <- "1970-01-01"
USEFUL_PCH <- 0:14 
GAP_PLOTTING_BUFFER <- gap_plotting_buffer
DEP_PLOTTING_BUFFER <- dep_plotting_buffer + 1

# apply start and end time cut offs
if(!is.null(start_time)) {
	start_time <- as.numeric(as.POSIXct(start_time, tz = tz))	
} else {
	start_time <- min(b$Start)
}

if(!is.null(end_time)) {
	end_time <- as.numeric(as.POSIXct(end_time, tz = tz))
} else {
	end_time <- max(b$End)
}

# calculate deps and limits for plotting
bclipped <- b[b$Start >= start_time & b$End <= end_time, ]
dep <- -rowMeans(bclipped[, c("DepthMax", "DepthMin")])
dep[is.na(dep)] <- 0

if(is.null(depth_lim)) {
	depth_lim <- min(dep) * DEP_PLOTTING_BUFFER
}

# create a little room to plot gaps
if(show_gaps) {
	deplim <- c(depth_lim, abs(depth_lim) * GAP_PLOTTING_BUFFER)
} else {
	deplim <- c(depth_lim, 0)
}

# set up the y axis and pretty ylim
prettyaxis <- deplim
prettyaxis[2] <- 0 # don't include the gaps in the axis
deplim[1] <- pretty(prettyaxis)[1] # this is the lowest dive

# set up a plotting area
plot(
	0, 0,
	xlab = "", ylab = ylab, type = 'n',
	xlim = c(start_time, end_time), ylim = deplim,
	las = 1, bty = 'n', axes = FALSE
)

# plot the y axis
axis(2, at = pretty(prettyaxis), las = 1)

if(xaxt) {
	dseq <- seq.POSIXt(
		as.POSIXct(paste(as.Date(start_time/60/60/24, origin = UNIX_EPOCH), "00:00:00"), tz = tz),
		as.POSIXct(paste(as.Date(end_time/60/60/24 + 1, origin = UNIX_EPOCH), "00:00:00"), tz = tz),
		by = "day"
	)
	
	hseq <- seq.POSIXt(
		as.POSIXct(paste(as.Date(start_time/60/60/24, origin = UNIX_EPOCH), "00:00:00"), tz = tz),
		as.POSIXct(paste(as.Date(end_time/60/60/24 + 1, origin = UNIX_EPOCH), "00:00:00"), tz = tz),
		by = "hour"
	)
	
	mseq <- seq.POSIXt(
		as.POSIXct(paste(as.Date(start_time/60/60/24, origin = UNIX_EPOCH), "00:00:00"), tz = tz),
		as.POSIXct(paste(as.Date(end_time/60/60/24 + 1, origin = UNIX_EPOCH), "00:00:00"), tz = tz),
		by = "min"
	)

	axis.POSIXct(1, at = dseq, format = "%d%b", las = 2, tcl = '-0.75')
	if(show_hours) axis.POSIXct(1, at = hseq, labels = FALSE)
	if(show_minutes) axis.POSIXct(1, at = mseq, labels = FALSE)
}


# make a tag list
bl <- split(b, b$DeployID)
tags <- names(bl)
ntags <- length(bl)

# set up plotting symbols and colors
if(is.null(col)) {
	set.seed(1)
	col <- rainbow_hcl(ntags, c = 100, alpha = 0.5)[sample(1:ntags, ntags)]
}

if(is.null(pch)) pch <- rep(USEFUL_PCH, ceiling(ntags / length(USEFUL_PCH)))[1:ntags]
if(length(cex) == 1) cex <- rep(cex, ntags)
if(length(lty) == 1) lty <- rep(lty, ntags)
if(length(lwd) == 1) lwd <- rep(lwd, ntags)

for(l in 1:ntags) {
	cur <- bl[[l]]

	wht <- cur$What
	dep <- -rowMeans(cur[, c('DepthMin', 'DepthMax')])
	stt <- cur$Start
	ent <- cur$End
	shp <- cur$Shape
		
	xx <- vector()
	yy <- vector()
	rid <- vector()
	i <- 0
	
	
	i1 <- min(which(stt <= end_time & ent >= start_time))
	i2 <- max(which(stt <= end_time & ent >= start_time))
	
	if(any(is.infinite(c(i1, i2)))) {
		i1 <- 1
		i2 <- nrow(cur)
		warning("no data for tag in start and end time range perhaps in a gap?")
	}
	
	if(show_shape) {
		for(m in i1:i2) {
			if(wht[m] == "Dive") {
				i <- i + 1
				xx[i] <- stt[m]
				yy[i] <- 0
				rid[i] <- m
				
				if(shp[m] == "Square") {
					i <- i + 1
					xx[i] <- stt[m]
					yy[i] <- dep[m]
					rid[i] <- m
				
					i <- i + 1
					xx[i] <- ent[m]
					yy[i] <- dep[m]
					rid[i] <- m
				} else if(shp[m] == "U") {
					st <- stt[m]
					en <- ent[m]
					df <- abs(en - st)
					df_25per <- df*0.25
					df_75per <- df*0.75
					
					btm <- vector()
					btm[1] <- st + df_25per
					btm[2] <- st + df_75per
					
					i <- i + 1
					xx[i] <- btm[1]
					yy[i] <- dep[m]
					rid[i] <- m
					
					i <- i + 1
					xx[i] <- btm[2]
					yy[i] <- dep[m]
					rid[i] <- m
					
				} else if(shp[m] == "V") {
					mid <- (stt[m] + ent[m]) / 2
					
					i <- i + 1
					xx[i] <- mid
					yy[i] <- dep[m]
					rid[i] <- m
				}
				
				i <- i + 1
				xx[i] <- ent[m]
				yy[i] <- 0
				rid[i] <- m
			}
			if(wht[m] == "Surface") {
				i <- i + 1
				xx[i] <- stt[m]
				yy[i] <- 0
				rid[i] <- m
				
				i <- i + 1
				xx[i] <- ent[m]
				yy[i] <- 0
				rid[i] <- m
			}
		}
	} else {
		xx <- (stt + ent) / 2
		yy <- dep
	}
	
	points(xx, yy, col = col[l], pch = pch[l], cex = cex[l])
	if(!all(is.na(lty))) {
		if(show_gaps) {
			stretch <- findgaps2(bl[[l]])$stretchid
			if(!show_shape) stretch <- stretch[bl[[l]]$What != "Message"][i1:i2]
			ustretch <- unique(stretch)
			nstretch <- length(ustretch)
			
			for(p in 1:nstretch) {
				dese <- which(stretch == ustretch[p])
				dese <- rid %in% dese
				lines(xx[dese], yy[dese], type ='l', col = col[l], lty = lty[l], lwd = lwd[l])
			}
		} else {
			lines(xx, yy, col = col[l], lty = lty[l], lwd = lwd[l])
		}
	}
}

if(!hidelegend) {
	if(!all(is.na(lty))) {
		legend(legendpos, legend = tags, pch = pch, lty = lty, col = col, bty = 'n')
	} else {
		legend(legendpos, legend = taglabs, pch = pch, col = col, bty = 'n')
	}
}

if(show_gaps) {
	gapy1 <- abs(depth_lim)*0.05
	gapy2 <- abs(depth_lim)*0.15
	
	gapslist <- lapply(bl, findgaps2)
	
	
	for(i in 1:length(gapslist)) {
		if(gapslist[[i]]$ngaps > 0) {
			gapx1 <- gapslist[[i]]$gap_st
			gapx2 <- gapslist[[i]]$gap_en

			g1 <- gapx1[gapx1 <= end_time & gapx2 >= start_time]
			g2 <- gapx2[gapx1 <= end_time & gapx2 >= start_time]
			
			if(length(g1) > 0 & length(g2) > 0)
				rect(g1, abs(depth_lim)*0.05, g2, abs(depth_lim)*0.15, col = col[i], border = NA)
		}
	}
	axis(2, at = mean(c(gapy1, gapy2)), labels = c("Gaps"), las = 1)
}

}
