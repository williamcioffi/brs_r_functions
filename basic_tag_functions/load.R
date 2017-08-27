lookdir <- "."

ff <- list.files(lookdir)
bf <- ff[grep("-Behavior\\.csv", ff)]
cf <- ff[grep("-Corrupt\\.csv",  ff)]
sf <- ff[grep("-Status\\.csv",   ff)]

beh <- read.table(file.path(lookdir, bf), header = TRUE, sep = ',', stringsAsFactors = FALSE)
cor <- read.table(file.path(lookdir, cf), header = TRUE, sep = ',', stringsAsFactors = FALSE)
sta <- read.table(file.path(lookdir, sf), header = TRUE, sep = ',', stringsAsFactors = FALSE)

beh$Start 		<- paste(strptime(beh$Start, 	format = "%H:%M:%S %d-%b-%Y"), "UTC")
beh$End 			<- paste(strptime(beh$End, 		format = "%H:%M:%S %d-%b-%Y"), "UTC")
cor$Date 		<- paste(strptime(cor$Date, 		format = "%H:%M:%S %d-%b-%Y"), "UTC")
sta$Received 	<- paste(strptime(sta$Received, format = "%H:%M:%S %d-%b-%Y"), "UTC")
