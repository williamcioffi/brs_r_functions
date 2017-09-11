###
# load
# 
# loads some useful data streams for futher manipulation
# wrc 20170911

lookdir <- "."

ff <- list.files(lookdir)
bf <- ff[grep("-Behavior\\.csv", ff)]
cf <- ff[grep("-Corrupt\\.csv",  ff)]
sf <- ff[grep("-Status\\.csv",   ff)]
af <- ff[grep("-All\\.csv", ff)]
ar <- ff[grep("-Argos\\.csv", ff)]

beh <- read.table(file.path(lookdir, bf), header = TRUE, sep = ',', stringsAsFactors = FALSE)
crp <- read.table(file.path(lookdir, cf), header = TRUE, sep = ',', stringsAsFactors = FALSE)
sta <- read.table(file.path(lookdir, sf), header = TRUE, sep = ',', stringsAsFactors = FALSE)
alm <- read.table(file.path(lookdir, af), header = TRUE, sep = ',', stringsAsFactors = FALSE)
arg <- read.table(file.path(lookdir, ar), header = TRUE, sep = ',', stringsAsFactors = FALSE)

beh$Start 		<- paste(strptime(beh$Start, 	format = "%H:%M:%S %d-%b-%Y"), "UTC")
beh$End 			<- paste(strptime(beh$End, 		format = "%H:%M:%S %d-%b-%Y"), "UTC")
crp$Date 		  <- paste(strptime(crp$Date, 		format = "%H:%M:%S %d-%b-%Y"), "UTC")
arg$Date      <- paste(strptime(arg$Date, format = "%H:%M:%S %d-%b-%Y"), "UTC")
sta$Received 	<- paste(strptime(sta$Received, format = "%H:%M:%S %d-%b-%Y"), "UTC")


#all uses a different date format... do I even have to explain why this is insane?
alm$Loc..date <- paste(strptime(alm$Loc..date, format = "%m/%d/%Y %H:%M:%S"), "UTC")
alm$Msg.Date  <- paste(strptime(alm$Msg.Date, format = "%m/%d/%Y %H:%M:%S"), "UTC")

