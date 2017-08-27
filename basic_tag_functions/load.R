ff <- list.files()
dese <- grep("Behavior", ff)
ff <- ff[dese]
nf <- length(ff)

b <- vector()

for(i in 1:nf) {
	tmp <- read.table(ff[i], header = TRUE, sep = ',', stringsAsFactors = FALSE)
	b <- rbind(b, tmp)
}

b$Start <- paste(strptime(b$Start, format = "%H:%M:%S %d-%b-%Y"), "UTC")
b$End <- paste(strptime(b$End, format = "%H:%M:%S %d-%b-%Y"), "UTC")


ff <- list.files()
dese <- grep("Corrupt", ff)
ff <- ff[dese]
nf <- length(ff)

cor <- vector()

for(i in 1:nf) {
	tmp <- read.table(ff[i], header = TRUE, sep = ',', stringsAsFactors = FALSE)
	cor <- rbind(cor, tmp)
}

cor$Date <- paste(strptime(cor$Date, format = "%H:%M:%S %d-%b-%Y"), "UTC")



ff <- list.files()
dese <- grep("Status", ff)
ff <- ff[dese]
nf <- length(ff)

stat <- vector()

for(i in 1:nf) {
	tmp <- read.table(ff[i], header = TRUE, sep = ',', stringsAsFactors = FALSE)
	stat <- rbind(stat, tmp)
}


stat$Received <- paste(strptime(stat$Received, format = "%H:%M:%S %d-%b-%Y"), "UTC")
