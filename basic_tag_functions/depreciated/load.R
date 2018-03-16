###################
### DEPRECIATED
###################

###
# loadtag
# 
# loads some useful data streams for futher manipulation
# returns a list of behavior, corrupt, status, all, and argos
#
# ~wrc 20170911
# modified 20180110

loadtag <- function(lookdir) {

ff <- list.files(lookdir)
bf <- ff[grep("-Behavior\\.csv", ff)]
cf <- ff[grep("-Corrupt\\.csv",  ff)]
sf <- ff[grep("-Status\\.csv",   ff)]
af <- ff[grep("-All\\.csv", ff)]
ar <- ff[grep("-Argos\\.csv", ff)]
lo <- ff[grep("-Locations\\.csv", ff)]
sr <- ff[grep("-Series\\.csv", ff)]

pb <- txtProgressBar(style = 3)
beh <- read.table(file.path(lookdir, bf), header = TRUE, sep = ',', stringsAsFactors = FALSE)
setTxtProgressBar(pb, 1/16)
crp <- read.table(file.path(lookdir, cf), header = TRUE, sep = ',', stringsAsFactors = FALSE)
setTxtProgressBar(pb, 2/16)
sta <- read.table(file.path(lookdir, sf), header = TRUE, sep = ',', stringsAsFactors = FALSE)
setTxtProgressBar(pb, 3/16)
alm <- read.table(file.path(lookdir, af), header = TRUE, sep = ',', stringsAsFactors = FALSE)
setTxtProgressBar(pb, 4/16)
arg <- read.table(file.path(lookdir, ar), header = TRUE, sep = ',', stringsAsFactors = FALSE)
setTxtProgressBar(pb, 5/16)
loc <- read.table(file.path(lookdir, lo), header = TRUE, sep = ',', stringsAsFactors = FALSE)
setTxtProgressBar(pb, 6/16)

# ser <- read.table(file.path(lookdir, sr), header = TRUE, sep = ',', stringsAsFactors = FALSE)
setTxtProgressBar(pb, 7/16)

timeformat1 <- "%H:%M:%S %d-%b-%Y"

beh$Start 		<- paste(strptime(beh$Start, 	format = timeformat1), "UTC")
setTxtProgressBar(pb, 8/16)
beh$End 		<- paste(strptime(beh$End, 		format = timeformat1), "UTC")
setTxtProgressBar(pb, 9/16)
crp$Date 		<- paste(strptime(crp$Date, 		format = timeformat1), "UTC")
setTxtProgressBar(pb, 10/16)
arg$Date      	<- paste(strptime(arg$Date, 		format = timeformat1), "UTC")
setTxtProgressBar(pb, 11/16)
sta$Received 	<- paste(strptime(sta$Received, format = timeformat1), "UTC")
setTxtProgressBar(pb, 12/16)
loc$Date      	<- paste(strptime(loc$Date, 		format = timeformat1), "UTC")
setTxtProgressBar(pb, 13/16)

# ser$Date 		<- paste(strptime(paste(ser$Time, ser$Day), format = timeformat1), "UTC")
setTxtProgressBar(pb, 14/16)

# all.csv uses a different date format... do I even have to explain why this is insane?

timeformat2 <- "%m/%d/%Y %H:%M:%S"
alm$Loc..date <- paste(strptime(alm$Loc..date, format = timeformat2), "UTC")
setTxtProgressBar(pb, 15/16)
alm$Msg.Date  <- paste(strptime(alm$Msg.Date,  format = timeformat2), "UTC")
setTxtProgressBar(pb, 16/16)

close(pb)
list(behavior = beh, corrupt = crp, status = sta, allmessages = alm, argos = arg, locations = loc)#, series = ser)

# end of function
}
