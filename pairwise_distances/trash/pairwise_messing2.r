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
  
  vf1 <- do.call('rbind', arg.list[f1])
  vf2 <- do.call('rbind', arg.list[f2])
  
  focpairs <- expand.grid(rownames(vf1), rownames(vf2))
  m1 <- merge(focpairs, vf1, by.x = 'Var1', by.y = 0)
  m2 <- merge(m1, vf2, by.x = 'Var2', by.y = 0)
  
  m2[, 'dt'] <- difftime(
    as.POSIXct(m2$date.x, tz = "UTC"), 
    as.POSIXct(m2$date.y, tz = "UTC"),
    units = 'secs'
  )
  m2 <- m2[which(abs(m2$dt) < 800), ]
  f1f2 <- m2[m2$sat.x == m2$sat.y, ]
  
  dist <- haverdist(f1f2$lat.x, f1f2$lon.x, f1f2$lat.y, f1f2$lon.y)
  pairdists[[paste(f1, f2)]] <- list(f1 = f1, f2 = f2, d = cbind(f1f2, dist))
}

# oo <- order(pairdists[[2]]$d$date.x)
# pairdists[[2]]$d[oo, ]
# 
# medmat  <- matrix(NA, nfriend, nfriend, dimnames = list(friends, friends))
# nmat    <- matrix(NA, nfriend, nfriend, dimnames = list(friends, friends))
# sdmat   <- matrix(NA, nfriend, nfriend, dimnames = list(friends, friends))
# 
# lapply(pairdists, function(l) {
#   dists <- l$d$dist
#   medmat[l$f1, l$f2] <<- median(dists)
#   nmat[l$f1, l$f2] <<- length(dists)
#   sdmat[l$f1, l$f2] <<- sd(dists)
# })