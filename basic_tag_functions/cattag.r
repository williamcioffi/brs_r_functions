###
# cat_tag
#
# concatenates all the downloads into big master files
# wrc 20170911


###
# constants
# raw data dir. expected format is a directory containing a bunch of directories which all start with BRS and contain dap extracted files
raw_data_dir <- "."

# this is what the dap processor adds inbetween the prefix (filter) and stream name (e.g., argos, behavior, ...)
DAP_FILE_SEP <- "-"

# this is the regrex for data directories with individual downloads unpacked with dap processor
# DIR_PATTERN <- '^BRS'

# this is the prefix for the master file written out for each stream
OUT_PREFIX <- "MASTER-"


###
# set up
# get list of all directories that start with BRS
setwd(raw_data_dir)
allfiles 	<- list.files()
# dese 		<- grep(DIR_PATTERN, allfiles)
# brsfiles 	<- allfiles[dese]
dese 		<- which(dir.exists(allfiles))
dirs 	<- allfiles[dese]
ndirs 		<- length(dirs)

# take account of what data streams are where
tagfiles 	<- list()
prefix 		<- list()
types 		<- list()

for(i in 1:ndirs) {
	tmpfiles 		<- list.files(file.path(dirs[i]))
	tmpfiles 		<- tmpfiles[grep("\\.csv$", tmpfiles)]
	tagfiles[[i]]	<- tmpfiles
	tagfiles_sp 		<- strsplit(tagfiles[[i]], DAP_FILE_SEP)
	prefix[[i]]	 	<- sapply(tagfiles_sp , '[[', 1)
	types[[i]] 		<- sapply(tagfiles_sp , '[[', 2)
}

###
# concatenate
# unique list of datastream types
utypes 	<- unique(unlist(types))
ntypes 	<- length(utypes)
streams <- list()
downloaded <- list()

# iterate over each datastream type and then over each directory and rbind one file per stream
# RawArgos has extra junk at the end. it seems to always be 5 lines so i've just dropped those

for(i in 1:ntypes) {

	curtype 		<- utypes[i]
	curstream 	<- list()
	
pb <- txtProgressBar(style = 3)	
	for(d in 1:ndirs) {
setTxtProgressBar(pb, d/ndirs)
		curprefix 	<- prefix[[d]][which(types[[d]] == utypes[i])]
		path 		<- file.path(dirs[d], paste(curprefix, curtype, sep = DAP_FILE_SEP))
		
		if(file.exists(path)) {
tryCatch({
			if(curtype != "RawArgos.csv") {
				curstream[[d]] <- read.table(path, header = TRUE, sep = ',', comment.char = "")
			} else {
				curstream[[d]] <- read.table(text = paste0(head(readLines(path), -5)), header = TRUE, sep = ',', comment.char = "")
			}
}, 
error = function(err) {print(paste("type", i, "dir", d, err))},
warning = function(war) {print(paste("type", i, "dir", d, war))},
finally = {})
		}
	}
close(pb)

	streams[[i]] <- do.call("rbind", curstream)
	write.table(streams[[i]], paste(OUT_PREFIX, utypes[i], sep = ""), row.names = FALSE, sep = ',')
}
