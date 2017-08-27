dists <- list()

mer <- merge(results_list[[1]], results_list[[2]], by = "day")
dists[[1]] <- data.frame(
	id1 = results_list[[1]]$id[1],
	id2 = results_list[[2]]$id[1], 
	day = mer$day, 
	dist = haverdist(mer$lat.x, mer$lon.x, mer$lat.y, mer$lon.y)
)

mer <- merge(results_list[[1]], results_list[[3]], by = "day")
dists[[2]] <- data.frame(
	id1 = results_list[[1]]$id[1],
	id2 = results_list[[3]]$id[1], 
	day = mer$day, 
	dist = haverdist(mer$lat.x, mer$lon.x, mer$lat.y, mer$lon.y)
)

mer <- merge(results_list[[1]], results_list[[4]], by = "day")
dists[[3]] <- data.frame(
	id1 = results_list[[1]]$id[1],
	id2 = results_list[[4]]$id[1], 
	day = mer$day, 
	dist = haverdist(mer$lat.x, mer$lon.x, mer$lat.y, mer$lon.y)
)
mer <- merge(results_list[[1]], results_list[[5]], by = "day")
dists[[4]] <- data.frame(
	id1 = results_list[[1]]$id[1],
	id2 = results_list[[5]]$id[1], 
	day = mer$day, 
	dist = haverdist(mer$lat.x, mer$lon.x, mer$lat.y, mer$lon.y)
)
mer <- merge(results_list[[2]], results_list[[3]], by = "day")
dists[[5]] <- data.frame(
	id1 = results_list[[2]]$id[1],
	id2 = results_list[[3]]$id[1], 
	day = mer$day, 
	dist = haverdist(mer$lat.x, mer$lon.x, mer$lat.y, mer$lon.y)
)

mer <- merge(results_list[[2]], results_list[[4]], by = "day")
dists[[6]] <- data.frame(
	id1 = results_list[[2]]$id[1],
	id2 = results_list[[4]]$id[1], 
	day = mer$day, 
	dist = haverdist(mer$lat.x, mer$lon.x, mer$lat.y, mer$lon.y)
)

mer <- merge(results_list[[2]], results_list[[5]], by = "day")
dists[[7]] <- data.frame(
	id1 = results_list[[2]]$id[1],
	id2 = results_list[[5]]$id[1], 
	day = mer$day, 
	dist = haverdist(mer$lat.x, mer$lon.x, mer$lat.y, mer$lon.y)
)

mer <- merge(results_list[[3]], results_list[[4]], by = "day")
dists[[8]] <- data.frame(
	id1 = results_list[[3]]$id[1],
	id2 = results_list[[4]]$id[1], 
	day = mer$day, 
	dist = haverdist(mer$lat.x, mer$lon.x, mer$lat.y, mer$lon.y)
)

mer <- merge(results_list[[3]], results_list[[5]], by = "day")
dists[[9]] <- data.frame(
	id1 = results_list[[3]]$id[1],
	id2 = results_list[[5]]$id[1], 
	day = mer$day, 
	dist = haverdist(mer$lat.x, mer$lon.x, mer$lat.y, mer$lon.y)
)

mer <- merge(results_list[[4]], results_list[[5]], by = "day")
dists[[10]] <- data.frame(
	id1 = results_list[[4]]$id[1],
	id2 = results_list[[5]]$id[1], 
	day = mer$day, 
	dist = haverdist(mer$lat.x, mer$lon.x, mer$lat.y, mer$lon.y)
)

distsdf <- do.call('rbind', dists)

pair <- paste(distsdf$id1, distsdf$id2, sep = '-')
upair <- sort(unique(pair))

plot(distsdf$day, distsdf$dist, col = as.factor(pair), log = 'y', las = 1)
legend("topright", legend = upair, col = as.factor(upair), pch = 1)