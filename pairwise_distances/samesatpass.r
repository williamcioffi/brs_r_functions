###
# look at same sat passes
#

datadir <- "~/Downloads/tmp/brs1/"

source("~/git/brs_r_functions/basic_tag_functions/findgaps.R")
source("~/git/brs_r_functions/basic_tag_functions/plot_dives.r")
source("~/git/brs_r_functions/basic_tag_functions/cattag.r")
source("~/git/brs_r_functions/basic_tag_functions/load.R")
source("~/git/brs_r_functions/pairwise_distances/haverdistance.R")

# cattag(datadir)
streams <- loadtag(datadir)

arg <- streams$locations

# arg <- arg[-which(arg$Quality == ""),]
arg <- arg[which(arg$Quality %in% c(0, 1, 2, 3, "A")), ]
# arg.list <- split(arg, arg$DeployID)

arg.dates <- round(as.POSIXct(arg$Date, tz = "UTC"), units = "mins")

pas <- paste0(strftime(arg.dates, format = "%Y%m%d%H%M"), arg$Satellite)
arg[, 'pass'] <- match(pas, unique(pas))

err <- arg$Error.radius

arg2 <- data.frame(
  did = arg$DeployID, date = arg$Date,
  lat = arg$Latitude, lon = arg$Longitude, lq = arg$Quality,
  err = err,
  pass = arg$pass
)

arg.list <- split(arg2, arg2$did)

head(arg.list[[1]])
friends <- names(arg.list)
nfriend <- length(friends)

pairdists <- list()
dirmat  <- matrix(NA, nfriend, nfriend, dimnames = list(friends, friends))
meanmat <- matrix(NA, nfriend, nfriend, dimnames = list(friends, friends))
medmat  <- matrix(NA, nfriend, nfriend, dimnames = list(friends, friends))
nmat    <- matrix(NA, nfriend, nfriend, dimnames = list(friends, friends))
sdmat   <- matrix(NA, nfriend, nfriend, dimnames = list(friends, friends))

library(reshape2)
dirmat[upper.tri(dirmat)] <- 1
pairs <- melt(dirmat)
pairs <- pairs[-which(is.na(pairs$value)),]

for(i in 1:choose(nfriend, 2)) {
  f1 <- pairs[i, 1]
  f2 <- pairs[i, 2]
  
  f1f2 <- merge(arg.list[[f1]], arg.list[[f2]], by = "pass")
  dist <- haverdist(f1f2$lat.x, f1f2$lon.x, f1f2$lat.y, f1f2$lon.y)
  pairdists[[paste(f1, f2)]] <- list(f1 = f1, f2 = f2, d = cbind(f1f2, dist))
}

lapply(pairdists, function(l) {
  dists <- l$d$dist
  medmat[l$f1, l$f2] <<- median(dists)
  nmat[l$f1, l$f2] <<- length(dists)
})

