###
# look at same sat passes
#

datadir <- "~/Downloads/tmp/brs1/"

source("~/git/brs_r_functions/basic_tag_functions/findgaps.R")
source("~/git/brs_r_functions/basic_tag_functions/plot_dives.r")
source("~/git/brs_r_functions/basic_tag_functions/cattag.r")
source("~/git/brs_r_functions/basic_tag_functions/load.R")
source("~/git/brs_r_functions/pairwise_distances/haverdistance.R")

cattag(datadir)
streams <- loadtag(datadir)

arg <- streams$argos

# arg <- arg[-which(arg$Quality == ""),]
arg <- arg[which(arg$LocationQuality %in% c(0, 1, 2, 3, "A")), ]
# arg.list <- split(arg, arg$DeployID)

arg.dates <- as.POSIXct(arg$Date, tz = "UTC")
# 
# pas <- paste0(strftime(arg.dates, format = "%Y%m%d%H%M"), arg$Satellite)
# arg[, 'pass'] <- match(pas, unique(pas))

arg2 <- data.frame(
  did = arg$DeployID, date = arg$Date,
  lat = arg$Latitude, lon = arg$Longitude, lq = arg$LocationQuality,
  sat = arg$Satellite
)

arg.list <- split(arg2, arg2$did)

head(arg.list[[1]])
friends <- names(arg.list)
nfriend <- length(friends)

library(reshape2)

pairdists <- list()
dirmat  <- matrix(NA, nfriend, nfriend, dimnames = list(friends, friends))
dirmat[upper.tri(dirmat)] <- 1
pairs <- melt(dirmat)
pairs <- pairs[-which(is.na(pairs$value)),]

for(i in 1:choose(nfriend, 2)) {
  f1 <- pairs[i, 1]
  f2 <- pairs[i, 2]
  
  af1 <- arg.list[[f1]]
  af2 <- arg.list[[f2]]
  
  pf1 <- 1:nrow(af1)
  pf2 <- NA*1:nrow(af2)
  
  for(p in 1:nrow(af1)) {
    diff <- abs(difftime(af1$date[p], af2$date, units = 'secs'))
    dese <- which(diff < 800)
    dose <- which(af1$sat[p] == af2$sat[dese])
    dese <- dese[dose]
    
    if(length(dese) > 0) {
      pf2[dese] <- p
    }
  }
  
  af1 <- data.frame(af1, pass = pf1)
  af2 <- data.frame(af2, pass = pf2)
  
  f1f2 <- merge(af1, af2, by = "pass")
  dist <- haverdist(f1f2$lat.x, f1f2$lon.x, f1f2$lat.y, f1f2$lon.y)
  pairdists[[paste(f1, f2)]] <- list(f1 = f1, f2 = f2, d = cbind(f1f2, dist))
}

oo <- order(pairdists[[2]]$d$date.x)
pairdists[[2]]$d[oo, ]

medmat  <- matrix(NA, nfriend, nfriend, dimnames = list(friends, friends))
nmat    <- matrix(NA, nfriend, nfriend, dimnames = list(friends, friends))
sdmat   <- matrix(NA, nfriend, nfriend, dimnames = list(friends, friends))

lapply(pairdists, function(l) {
  dists <- l$d$dist
  medmat[l$f1, l$f2] <<- median(dists)
  nmat[l$f1, l$f2] <<- length(dists)
  sdmat[l$f1, l$f2] <<- sd(dists)
})

