###
# messing with rawargos to find threshold
#

rawarg <- read.table("~/Downloads/tmp/brs1/MASTER-RawArgos.csv", header = TRUE, sep = ',')
rawarg.list <- split(rawarg, paste(rawarg$Pass, rawarg$PTT))

lags <- sapply(rawarg.list, function(l) {
  dates <- strptime(paste(l$MsgDate, l$MsgTime), format = "%d-%b-%Y %H:%M:%S", tz = "UTC")
  abs(difftime(range(dates)[1], range(dates)[2], units = 'secs'))
})

x11()
hist(lags)
