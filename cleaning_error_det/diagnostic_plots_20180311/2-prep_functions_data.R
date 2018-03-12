# load neccessary functions and prep data
# ~wrc 20180311

### load neccessary functions

### load neccessary libraries
# install.packages("RCurl") # for sourcing from github
require(RCurl)

prefix1 <- "https://raw.githubusercontent.com/williamcioffi/brs_r_functions/master/basic_tag_functions/"
prefix2 <- "https://raw.githubusercontent.com/williamcioffi/cplots/master/"

brs_functions <- paste(
	getURL(paste0(prefix1, "plot_dives.r"), ssl.verifypeer = FALSE),
	getURL(paste0(prefix1, "findgaps.R"), ssl.verifypeer = FALSE),
	getURL(paste0(prefix2, "cplot.R"), ssl.verifypeer = FALSE),
	sep = "\n"
)

eval(parse(text = brs_functions))

### helper functions
getmsgid <- function(z) {
	k <- 0
	msgid <- vector()
	for(i in 1:nrow(z)) {
		if(z$What[i] == "Message") k <- k + 1
		msgid[i] <- k
	}
	
	msgid
}

statusdepth_filter <- function(s, cutoffnum = 2) {
# expects status s to have a column named sdates
# I add this column in below depending on if it has RTC or Recieved dates (varies by tag)
	deps <- s$Depth[!is.na(s$Depth)]
	cutoff <- NA
	reason <- "no depths"
	
	if(length(deps) > 0) {
		depover10 <- abs(s$Depth[!is.na(s$Depth)]) > 10
		numpastthreshold <- length(which(depover10))
	
		if(numpastthreshold >= cutoffnum) {
			lastgood <- min(which(depover10)) - 1
			if(lastgood > 0) {
				cutoff <- as.character(s$sdates[lastgood])
				reason <- "pressure sensor flagged"
			} else {
				cutoff <- NA # never got a good status message
				reason <- "pressure sensor flagged"
			}
		} else {
			cutoff <- as.character(max(s$sdates[!is.na(s$Depth)]))
			reason <- "last status message"
		}
	}
	
	list(cutoff = cutoff, reason = reason)
}

### load data
beh <- read.table("1-MASTER-Behavior.csv", header = TRUE, sep = ',', stringsAsFactors = FALSE)
sta <- read.table("1-MASTER-Status.csv", header = TRUE, sep = ',', stringsAsFactors = FALSE)

# grab the tags of interest: zcs and gms
beh <- beh[grep("Zc|Gm", beh$DeployID), ]
desetags <- sort(unique(beh$DeployID))
sta <- sta[sta$DeployID %in% desetags, ]

# fix the date and time columns
# this is kind of nuts but posix dates don't travel well because there is no posixt mode
beh$Start <- as.character(as.POSIXct(beh$Start, format = "%H:%M:%S %d-%b-%Y", tz = "UTC"))
beh$End <- as.character(as.POSIXct(beh$End, format = "%H:%M:%S %d-%b-%Y", tz = "UTC"))
sta$Received <- as.character(as.POSIXct(sta$Received, format = "%H:%M:%S %d-%b-%Y", tz = "UTC"))
sta$RTC <- as.character(as.POSIXct(sta$RTC, format = "%H:%M:%S %d-%b-%Y", tz = "UTC"))

# get the max count to make everything the same scale
maxcount <- max(beh$Count)

# convert to list
behl <- split(beh, beh$DeployID)
stal <- split(sta, sta$DeployID)

if(!all(names(behl) == names(stal)))
	stop("uh oh tag lists didn't match up might need some human attention")


# fix by hand the problem with ZcTag029
zc29 <- behl[['ZcTag029']]
msgid <- getmsgid(zc29)
# # take a look at offenders
# plot(zc29$DurationMax)
# zc29[zc29$DurationMax > 2e+05, ]
# # kill msgid 86
behl[['ZcTag029']] <- zc29[msgid != 86, ]

save(beh, behl, cplot, findgaps, maxcount, plot_dives, statusdepth_filter, sta, stal, file = "3-functions_data.RData")