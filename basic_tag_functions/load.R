lookdir <- "."

ff <- list.files(lookdir)
bf <- ff[grep("-Behavior\\.csv", ff)]
cf <- ff[grep("-Corrupt\\.csv",  ff)]
sf <- ff[grep("-Status\\.csv",   ff)]
af <- ff[grep("-All\\.csv", ff)]

beh <- read.table(file.path(lookdir, bf), header = TRUE, sep = ',', stringsAsFactors = FALSE)
crp <- read.table(file.path(lookdir, cf), header = TRUE, sep = ',', stringsAsFactors = FALSE)
sta <- read.table(file.path(lookdir, sf), header = TRUE, sep = ',', stringsAsFactors = FALSE)
alm <- read.table(file.path(lookdir, af), header = TRUE, sep = ',', stringsAsFactors = FALSE)

beh$Start 		<- paste(strptime(beh$Start, 	format = "%H:%M:%S %d-%b-%Y"), "UTC")
beh$End 			<- paste(strptime(beh$End, 		format = "%H:%M:%S %d-%b-%Y"), "UTC")
crp$Date 		<- paste(strptime(cor$Date, 		format = "%H:%M:%S %d-%b-%Y"), "UTC")
sta$Received 	<- paste(strptime(sta$Received, format = "%H:%M:%S %d-%b-%Y"), "UTC")

#all uses a different date format... do I even have to explain why this is insane?
alm$Loc..date 	<- paste(strptime(alm$Loc..date, format = "%m/%d/%Y %H:%M:%S"), "UTC")
alm$Msg.Date 	<- paste(strptime(alm$Msg.Date, format = "%m/%d/%Y %H:%M:%S"), "UTC")