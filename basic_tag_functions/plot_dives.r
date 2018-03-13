###
# plot_dives
# wrc 20170911

plot_dives <- function(
  behavior,
  depth_lim = NULL,
  start_time = NULL,
  end_time = NULL,
  show_shape = TRUE,
  show_gaps = FALSE,
  show_minutes = FALSE,
  show_hours = TRUE,
  deploy_ids = NULL,
  col = NULL,
  pch = NULL,
  lty = 1,
  lwd = 1,
  cex = 1,
  hidelegend = FALSE,
  ylab = "depth (meters)"
) {

###
# constants  
USEFUL_PCH <- 0:18
UNIX_EPOCH <- "1970-01-01"
  
###
# set up of some parameters
  
# restrict to certain deployids
if(!is.null(deploy_ids)) {
	dese <- which(behavior$DeployID %in% deploy_ids)
	behavior <- behavior[dese, ]
}

# make sure everything is in chronological order and build a list by deployid
# friendnames is used later to draw the legend
# nfriends is used to know how many animals we are plotting
behavior <- behavior[order(behavior$Start), ]
friendnames <- sort(unique(behavior$DeployID))
nfriends <- length(friendnames)
b_oo <- order(behavior$DeployID)
b_ordered <- behavior[b_oo, ]
blist <- split(b_ordered, as.character(b_ordered$DeployID))

# calculate depth limits for plotting.
dep <- -apply(b_ordered[, c('DepthMin', 'DepthMax')], 1, mean)
dep[which(is.na(dep))] <- 0

if(is.null(depth_lim)) {
	depth_lim <- min(dep)*1.15
}

# find the min and max datetime for plotting zoom and axes
tt <- as.character(b_ordered$Start)
tt <- c(tt, as.character(max(b_ordered$End)))

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

# create a little room to plot gaps
deplim <- c(depth_lim, abs(depth_lim)*0.15)

# set up the plotting area
plot(
	as.POSIXct(tt, tz = "UTC"), rep(0, length(tt)),
	xlab = "", ylab = "", 
	type = 'n', 
	xlim = c(zoom1, zoom2), ylim = deplim,
	las = 1, bty = 'n', axes = FALSE
)

mtext(ylab, side = 2, line = 4.1, las = 3)

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

# plot date and hour by default and minutes if flag is set
axis.POSIXct(1, at = dseq, format = "%d%b", las = 2, tcl = '-0.75')
if(show_hours) axis.POSIXct(1, at = tseq, labels = FALSE)
if(show_minutes) axis.POSIXct(1, at = mseq, labels = FALSE)


# I don't want to include the gaps plot (15% of maxdepth above 0) in the axis
prettyaxis <- deplim
prettyaxis[2] <- 0

axis(2, at = pretty(prettyaxis), las = 1)

# set up plotting symbols
# assuming that user supplied colors have alpha = 1, turn it down for better plotting
# if pch aren't specified pull from a list of good pches
if(is.null(col)) col <- rainbow(nfriends)
if(all(is.na(col))) col <- rep("black", nfriends)

colors_dark <- col
colorss <- adjustcolor(col, alpha.f = 0.33)
cexes <- rep(cex, nfriends)
if(is.null(pch)) pch <- rep(USEFUL_PCH, ceiling(nfriends / length(USEFUL_PCH)))[1:nfriends]

##############################
###CUT OFF TO SHORTEST TAG?###
###currently: no           ###
##############################
			
tagends <- as.character(sapply(blist, function(l) l$End[length(l$End)]))
cutoff <- min(as.POSIXct(tagends, tz = "UTC"))

DESEROWS <- lapply(blist, function(l) 1:nrow(l))
# DESEROWS <- lapply(b_beaks, function(l) {
	# which(as.POSIXct(l$End, tz = "UTC") <= cutoff)
# })

for(l in 1:length(blist)) {
	cur <- blist[[l]]
	cur <- cur[DESEROWS[[l]], ]
	cur <- cur[cur$What != "Message", ]
	wht <- cur$What
	dep <- -apply(cur[, c('DepthMin', 'DepthMax')], 1, mean)
	stt  <- as.character(cur$Start)
	ent  <- as.character(cur$End)
	shp <- cur$Shape
	
	
	xx <- vector()
	yy <- vector()
	rid <- vector()
	i <- 0
	
	if(show_shape) {
		for(m in 1:nrow(cur)) {
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
					st <- as.POSIXct(stt[m], tz = "UTC")
					en <- as.POSIXct(ent[m], tz = "UTC")
					df <- difftime(en, st, units = "secs")
					df_25per <- df*0.25
					df_75per <- df*0.75
					btm <- vector()
					btm[1] <- format(st + df_25per)
					btm[2] <- format(st + df_75per)
					
					i <- i + 1
					xx[i] <- btm[1]
					yy[i] <- dep[m]
					rid[i] <- m
					
					i <- i + 1
					xx[i] <- btm[2]
					yy[i] <- dep[m]
					rid[i] <- m
					
				} else if(shp[m] == "V") {
					mid <- format(mean(as.POSIXct(c(stt[m], ent[m]), tz = "UTC")))
					
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
		xxdivebottom <- as.character(as.POSIXct((as.numeric(as.POSIXct(stt, tz = "UTC")) + as.numeric(as.POSIXct(ent, tz = "UTC"))) / 2, origin = UNIX_EPOCH, tz = "UTC")) 
		xxdivebottom <- xxdivebottom[!is.na(dep)]
		xxdivest <- stt[!is.na(dep)]
		xxdiveen <- ent[!is.na(dep)]
		yy <- dep[!is.na(dep)]
		
		xx <- c(rbind(xxdivest, xxdivebottom, xxdiveen))
		yy <- c(rbind(rep(0, length(yy)), yy, rep(0, length(yy))))
		rid <- rep(1:length(dep[!is.na(dep)]), each = 3)
	}
	
	xx <- as.POSIXct(xx, tz = "UTC")
	points(xx, yy, col = colorss[l], pch = pch[l], cex = cexes[l])
	if(!is.na(lty)) {
		if(show_gaps) {
			stretch <- findgaps(cur)$stretchid
			if(!show_shape) stretch <- stretch[cur$What[cur$What != "Message"] == "Dive"]
			ustretch <- unique(stretch)
			nstretch <- length(ustretch)
			
			for(p in 1:nstretch) {
				dese <- which(stretch == ustretch[p])
				dese <- rid %in% dese
				lines(xx[dese], yy[dese], type ='l', col = colorss[l], lty = lty, lwd = lwd)
			}
		} else {
			lines(xx, yy, col = colorss[l], lty = lty, lwd = lwd)
		}
	}
	
	# cur_alltimes <- c(as.character(cur$Start), as.character(cur$End))
	# lines(as.POSIXct(cur_alltimes, tz = "UTC"), rep(l*100, nrow(cur)*2), lwd = 10, col = colors_dark[l])
}

taglabs <- sapply(blist, function(l) as.character(l$DeployID[1]))
ntags <- length(taglabs)

if(!hidelegend) {
	if(!is.na(lty)) {
		legend("bottomright", legend = taglabs, pch = pch[1:ntags], lty = rep(lty, ntags), col = colors_dark[1:ntags], bty = 'n')
	} else {
		legend("bottomright", legend = taglabs, pch = pch[1:ntags], col = colors_dark[1:ntags], bty = 'n')
	}
}

if(show_gaps) {
	gapy1 <- abs(depth_lim)*0.05
	gapy2 <- abs(depth_lim)*0.15
	
	gapslist <- lapply(blist, findgaps)
	
	for(i in 1:length(gapslist)) {
		if(gapslist[[i]]$ngaps > 0) {
			gapx1 <- as.POSIXct(gapslist[[i]]$gap_st, tz = "UTC")
			gapx2 <- as.POSIXct(gapslist[[i]]$gap_en, tz = "UTC")
			rect(gapx1, abs(depth_lim)*0.05, gapx2, abs(depth_lim)*0.15, col = colorss[i], border = NA)
		}
	}
	
	axis(2, at = mean(c(gapy1, gapy2)), labels = c("Gaps"), las = 1)
}

}


# # #synoptic
# # basedir <- "Z:/Research/Read/Users/cioffi/private/divedata/ZcTag051/ZcTag051-94815"

# # basedir 	<- ""
# # basedir 	<- "Z:/Research/Read/Users/cioffi/argoslook/argoslook_dev/DATA/BRS_lastupdated_20may2017_1807hrsGMT"
# # basedir	 <- "../argoslook_dev/DATA/BRS_lastupdated_07june2017_1054hrsGMT"
# # prefix 		<- "sync"

# # afile 		<- file.path(basedir, paste(prefix, "Argos.csv", sep = ''))
# # bfile 		<- file.path(basedir, paste(prefix, "Behavior.csv", sep = ''))
# # corruptfile <- file.path(basedir, paste(prefix, "Corrupt.csv", sep = ''))

# # afile <- file.path(basedir, "BRS1_dap_filter_config-Argos.csv")
# # bfile <- file.path(basedir, "BRS1_dap_filter_config-Behavior.csv")
# # corruptfile <- file.path(basedir, "BRS1_dap_filter_config-Corrupt.csv")

# # a <- read.table(afile, header = TRUE, sep = ',')
# b <- read.table("sync.csv", header = TRUE, sep = ',')
# # corrupt <- read.table(corruptfile, header = TRUE, sep = ',')

# # a <- a[order(a$Date), ]
# b <- b[order(b$Start), ]
# corrupt <- corrupt[order(corrupt$Date), ]

# friendnames <- sort(unique(b$DeployID))
# desetags <- friendnames[grep("Zc", friendnames)]
# desetags <- desetags[1:3]
# # desetags <- desetags[4:5]
# # desetags <- friendnames[grep("Gm", friendnames)]
# # desetags <- desetags[c(7, 9)]

# # a_beak <- a[which(a$DeployID %in% desetags), ]
# b_beak <- b[which(b$DeployID %in% desetags), ]
# # c_beak <- corrupt[which(corrupt$DeployID %in% desetags), ]

# # a_oo <- order(a_beak$DeployID)
# b_oo <- order(b_beak$DeployID)
# # r_oo <- order(c_beak$DeployID)

# # a_beak <- a_beak[a_oo, ]
# b_beak <- b_beak[b_oo, ]
# # c_beak <- c_beak[r_oo, ]

# b_beaks <- split(b_beak, as.character(b_beak$DeployID))
# # a_beaks <- split(a_beak, as.character(a_beak$DeployID))
# # c_beaks <- split(c_beak, as.character(c_beak$DeployID))

# dep <- -apply(b_beak[, c('DepthMin', 'DepthMax')], 1, mean)
# dep[which(is.na(dep))] <- 0
# tt <- as.character(b_beak$Start)
# tt[length(tt)] <- as.character(max(as.POSIXct(b_beak$End, tz = "UTC")))

# #######################
# ###Select date Range###
# #######################

# unique(format(as.POSIXct(tt), "%d-%b-%y"))

# # zoom1 <- as.POSIXct("2017-05-22 00:00:00", tz = "UTC")
# zoom1 <- as.POSIXct("2016-05-28 15:00:00", tz = "UTC")
# zoom2 <- as.POSIXct("2016-06-10 01:00:00", tz = "UTC")



# ##########
# ###Plot###
# ##########
# par(mar = c(4.1, 6.1, 3.1, 3.1))
# # par(mar = c(0, 0, 0, 0), oma = c(6, 6, 2, 3))
# # lay <- matrix(c(2, 2, 
			  	# # 1, 1,
			  	# # 1, 1,
			  	# # 1, 1), 4, 2, byrow = TRUE)
# # layout(lay)
# #look at the argos hit pattern

# plot(as.POSIXct(tt, tz = "UTC"), dep, las = 1, bty = 'n', xaxt = 'n', xlab = "", ylab = "", type = 'n', xlim = tlim, ylim = depth_lim)
# mtext("depth (meters)", side = 2, line = 4.1, las = 3)

# tseq <- seq.POSIXt(as.POSIXct(paste(as.Date(tt[1]), "00:00:00"), tz = "UTC"), as.POSIXct(paste(as.Date(tt[length(tt)]) + 1, "00:00:00"), tz = "UTC"), by = "hour")

# dseq <- seq.POSIXt(as.POSIXct(paste(as.Date(tt[1]), "00:00:00"), tz = "UTC"), as.POSIXct(paste(as.Date(tt[length(tt)]) + 1, "00:00:00"), tz = "UTC"), by = "day")
# axis.POSIXct(1, at = dseq, format = "%d%b", las = 2, tcl = '-0.75')
# axis.POSIXct(1, at = tseq, labels = FALSE)

# colorss <- c(rgb(1, 0, 0, .33),
			# rgb(0, 1, 0, .33),
			# rgb(0, 0, 1, .33),
			# rgb(1, 1, 0, .33),
			# rgb(0, 1, 1, .33),
			# rgb(1, 0, 1, .33),
			# rgb(0, 0, 0, .33),
			# rgb(.5, .5, 0, .33),
			# rgb(.5, 0, .5, .33),
			# rgb(0, .5, .5, .33))
			
# colors_dark <- c(rgb(1, 0, 0),
				 # rgb(0, 1, 0),
				 # rgb(0, 0, 1),
				 # rgb(1, 1, 0),
				 # rgb(0, 1, 1),
				 # rgb(1, 0, 1),
				 # rgb(0, 0, 0),
				 # rgb(.5, .5, 0),
				 # rgb(.5, 0, .5),
				 # rgb(0, .5, .5))
			
# pch <- c(15, 16, 17, 18, 19, 21, 22, 23, 24, 25)
# cexes <- c(1, 1, 1, 1, 1)

# ##############################
# ###CUT OFF TO SHORTEST TAG?###
# ###currently: no           ###
# ##############################
			
# tagends <- as.character(sapply(b_beaks, function(l) l$End[length(l$End)]))
# cutoff <- min(as.POSIXct(tagends, tz = "UTC"))

# DESEROWS <- lapply(b_beaks, function(l) 1:nrow(l))
# # DESEROWS <- lapply(b_beaks, function(l) {
	# # which(as.POSIXct(l$End, tz = "UTC") <= cutoff)
# # })

# for(l in 1:length(b_beaks)) {
	# cur <- b_beaks[[l]]
	# cur <- cur[DESEROWS[[l]], ]
	# cur <- cur[-which(cur$What == "Message"), ]
	# wht <- cur$What
	# dep <- -apply(cur[, c('DepthMin', 'DepthMax')], 1, mean)
	# stt  <- as.character(cur$Start)
	# ent  <- as.character(cur$End)
	# shp <- cur$Shape
	
	
	# xx <- vector()
	# yy <- vector()
	# i <- 0
	
	# for(m in 1:nrow(cur)) {
		# if(wht[m] == "Dive") {
			# i <- i + 1
			# xx[i] <- stt[m]
			# yy[i] <- 0
			
			# if(shp[m] == "Square") {
				# i <- i + 1
				# xx[i] <- stt[m]
				# yy[i] <- dep[m]
			
				# i <- i + 1
				# xx[i] <- ent[m]
				# yy[i] <- dep[m]
			# } else if(shp[m] == "U") {
				# st <- as.POSIXct(stt[m], tz = "UTC")
				# en <- as.POSIXct(ent[m], tz = "UTC")
				# df <- difftime(en, st, units = "secs")
				# df_25per <- df*0.25
				# df_75per <- df*0.75
				# btm <- vector()
				# btm[1] <- format(st + df_25per)
				# btm[2] <- format(st + df_75per)
				
				# i <- i + 1
				# xx[i] <- btm[1]
				# yy[i] <- dep[m]
				
				# i <- i + 1
				# xx[i] <- btm[2]
				# yy[i] <- dep[m]
				
			# } else if(shp[m] == "V") {
				# mid <- format(mean(as.POSIXct(c(stt[m], ent[m]), tz = "UTC")))
				
				# i <- i + 1
				# xx[i] <- mid
				# yy[i] <- dep[m]
			# }
			
			# i <- i + 1
			# xx[i] <- ent[m]
			# yy[i] <- 0
		# }
		# if(wht[m] == "Surface") {
			# i <- i + 1
			# xx[i] <- stt[m]
			# yy[i] <- 0
			
			# i <- i + 1
			# xx[i] <- ent[m]
			# yy[i] <- 0
		# }
	# }
	
	# xx <- as.POSIXct(xx, tz = "UTC")
	# points(xx, yy, col = colorss[l], pch = pch[l], cex = cexes[l])
	# lines(xx, yy, col = colorss[l])
	
	# # cur_alltimes <- c(as.character(cur$Start), as.character(cur$End))
	# # lines(as.POSIXct(cur_alltimes, tz = "UTC"), rep(l*100, nrow(cur)*2), lwd = 10, col = colors_dark[l])
# }

# taglabs <- sapply(b_beaks, function(l) as.character(l$DeployID[1]))
# ntags <- length(taglabs)
# legend("bottomright", legend = taglabs, pch = pch[1:ntags], lty = rep(1, ntags), col = colors_dark[1:ntags], bty = 'n')






# #coverage
# # b_beak_alltimes <- c(as.character(b_beak$Start), as.character(b_beak$End))

# # plot(as.POSIXct(b_beak_alltimes, tz = "UTC"), rep(1, nrow(b_beak)*2), ylim = c(0.75, 3.25), type = 'n', xlab = "", ylab = "", las = 1, axes = FALSE, xlim = c(zoom1, zoom2))
# # axis(2, at = c(1, 2, 3), labels = unique(a_beak$DeployID), las = 1)
# # # axis.POSIXct(1, at = dseq, format = "%d%b", las = 2, tcl = '-0.75')
# # # axis.POSIXct(1, at = tseq, labels = FALSE)

# # colors_dark <- c(rgb(1, 0, 0), rgb(0, 1, 0), rgb(0, 0, 1))
# # for(l in 1:length(b_beaks)) {
	# # bek <- b_beaks[[l]]
	# # bek_alltimes <- c(as.character(bek$Start), as.character(bek$End))
	# # lines(as.POSIXct(bek_alltimes, tz = "UTC"), rep(l, nrow(bek)*2), lwd = 10, col = colors_dark[l])
# # }


# labs_pre <- as.character(unique(a_beak$DeployID))
# labs_spt <- strsplit(labs_pre, 'Tag')
# labs <- sapply(labs_spt, function(l) {paste(l[1], l[2], sep = "")})

# ylims <- c(0 + length(labs)*0.05, length(labs)*1.05)

# plot(as.POSIXct(a_beak$Date, tz = "UTC"), rep(1, nrow(a_beak)), ylim = ylims, type = 'n', xlab = "", ylab = "", las = 1, axes = FALSE, xlim = c(zoom1, zoom2))
# mtext("argos fixes", side = 2, line = 4.1, cex = .75)
# axis(2, at = rev(seq(1:length(labs))), labels = labs, las = 1)
# # axis.POSIXct(1, at = dseq, format = "%d%b", las = 2, tcl = '-0.75')
# # axis.POSIXct(1, at = tseq, labels = FALSE)


# for(l in 1:length(a_beaks)) {
	# bek <- a_beaks[[l]]
	# points(as.POSIXct(bek$Date), rep(length(a_beaks)+1-l, nrow(bek)), pch = 16, col = colorss[l])
# }


# #look at the corrupt
# dese <- which(c_beak$Possible.Type == "Behavior")

# cb_beak <- c_beak[dese, ]
# cb_beaks <- split(cb_beak, as.character(cb_beak$DeployID))

# cumcount <- lapply(cb_beaks, function(l) 1:nrow(l))

# xx <- as.POSIXct(cb_beak$Date, tz = "UTC")
# yy <- unlist(cumcount)

# plot(xx, yy, axes = FALSE, xlab = "", ylab = "", type = 'n', xlim = c(zoom1, zoom2))
# mtext("# corrupt", 2, las = 3, line = 4.1, cex = .75)
# # tseq <- seq.POSIXt(as.POSIXct(paste(as.Date(xx[1]), "00:00:00"), tz = "UTC"), as.POSIXct(paste(as.Date(xx[length(xx)]) + 1, "00:00:00"), tz = "UTC"), by = "hour")
# # dseq <- seq.POSIXt(as.POSIXct(paste(as.Date(xx[1]), "00:00:00"), tz = "UTC"), as.POSIXct(paste(as.Date(xx[length(xx)]) + 1, "00:00:00"), tz = "UTC"), by = "day")
# # axis.POSIXct(1, at = dseq, format = "%d%b", las = 2, tcl = '-0.75')
# # axis.POSIXct(1, at = tseq, labels = FALSE)
# axis(2, las = 1)


# for(l in 1:length(cb_beaks)) {
	# xx <- as.POSIXct(cb_beaks[[l]]$Date, tz = "UTC")
	# yy <- cumcount[[l]]
	# points(xx, yy, pch = pch[l], col = colors_dark[l], cex = .5)
	# lines(xx, yy, col = colors_dark[l])
# }

# # abline(v = max(as.POSIXct(b_beaks[[2]]$End, tz = "UTC")), lty = 2, col = "purple")

# legend("topleft", legend = taglabs, pch = pch[1:ntags], lty = c(rep(1, ntags)), col = c(colors_dark[1:ntags]), bty = 'n')
