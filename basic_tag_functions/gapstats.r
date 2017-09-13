###
# gap stats

behlist <- split(beh, beh$DeployID)
gaplist <- lapply(behlist, findgaps)
gaplengths <- lapply(gaplist, function(l) abs(l$gap_diffs))

data.frame(
gaps      = sapply(gaplist, function(l) l$ngaps),
mean      = sapply(gaplengths, function(l) round(mean(l[which(l > 60)])/60/60, 1)),
sd        = sapply(gaplengths, function(l) round(sd  (l[which(l > 60)])/60/60, 1)),
max       = sapply(gaplengths, function(l) round(max (l[which(l > 60)])/60/60, 1)),
totalgaps = sapply(gaplengths, function(l) round(sum (l[which(l > 60)])/60/60, 1))
)

