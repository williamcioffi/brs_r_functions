rcsv <- function(..., header = TRUE, sep = ',', stringsAsFactors = FALSE)  {
	read.table(..., header = header, sep = sep, stringsAsFactors = stringsAsFactors)
}

wcsv <- function(..., sep = ',', row.names = FALSE) {
	write.table(..., sep = sep, row.names = row.names)
}

date2num <- function(d, tz = 'UTC', format = '%H:%M:%S %d-%b-%Y') {
	as.numeric(as.POSIXct(d, tz = tz, format = format))
}

num2date <- function(d, tz = 'UTC', origin = "1970-01-01") {
	as.POSIXct(d, tz = tz , origin = origin)
}

dateseq <- function(d, hours = FALSE) {
	unit <- 60*60*24 # day in seconds
	if(hours) unit <- 60*60 # hour in seconds

	mind <- min(d, na.rm = TRUE)
	maxd <- max(d, na.rm = TRUE)

	std <- trunc(mind / unit) * unit
	end <- ceiling(maxd / unit) * unit
	
	seq(std, end, by = unit)
}

matchtimes <- function(t1, t2) {
# t1, t2 are numeric
	findInterval(t1, c(-Inf, head(t2, -1)) + c(0, diff(t2)/2))
}
