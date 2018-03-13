###
# read_gonio
#
# read in a raw gonio text file and clean it up into a data frame
# ~wrc 20180313

read_gonio <- function(gfile) {
	g <- readLines(gfile)
	g_nprf <- g[grep("NPRF", g)]
	g_npr <- g[grep("NPR,", g)]
	
	# make new files out of these greped vectors
	nprf_file <- tempfile()
	npr_file <- tempfile()
	writeLines(g_nprf, nprf_file)
	writeLines(g_npr, npr_file)
	
	# load new files in as CSVs
	nprf <- read.csv(nprf_file, header = FALSE, sep = ',')
	npr <- read.csv(npr_file, header = FALSE, sep = ',')
	
	# add dummy columns for the saved average strength and average bearing columns for NPR (this only works on NPRF)
	npr_withcols <- data.frame(npr[, 1:12], rep(NA, nrow(npr)), npr[, 13], rep(NA, nrow(npr)), npr[, 14:ncol(npr)])
	names(npr_withcols) <- paste0("V", 1:20)
	
	# make a new data table with both NPR and NPRF and the columns lining up
	allg <- rbind(npr_withcols, nprf)
	
	# remove excess words from recieved date time
	rec_date <- as.character(allg$V1)
	rec_date_split <- strsplit(rec_date, split = " ")
	rec_date_formatted <- sapply(rec_date_split, function(l) paste(l[1], l[2]))
	
	allg$V1 <- rec_date_formatted
	
	allg
}