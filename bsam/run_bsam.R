###
# run_bsam
#
# template for running bsam on concatenated data
# ~wrc 20180310

library(bsam)
library(sp)

# save image file name
FNAME <- "bsam_results.RData"

# projection from RS
aeaproj <- CRS("+proj=aea +lat_1=27.33333333333333 +lat_2=40.66666666666666 +lat_0=34 +lon_0=-78 +x_0=0 +y_0=0 ellps=WGS84 +datum=WGS84 +units=m +no_defs")

# prep location file
loc <- read.table("MASTER-Locations.csv", header = TRUE, sep = ',')
loc$Date <- as.POSIXct(loc$Date, format = "%H:%M:%S %d-%b-%Y", tz = "UTC")
loc <- loc[which(loc$Quality %in% c("0", "1", "2", "3", "A", "B")), ]

# take a look
# plot(loc$Longitude, loc$Latitude, col = loc$DeployID)

coordinates(loc) <- ~ Longitude + Latitude
proj4string(loc) <- CRS("+proj=longlat")
loc_proj <- spTransform(loc, aeaproj)

movedata <- data.frame(id = loc$DeployID, date = loc$Date, lc = loc$Quality, lon = loc$Longitude, lat = loc$Latitude)

fit <- fit_ssm(movedata, model = "hDCRWS", tstep = 0.25, adapt = 5000, samples = 5000, thin = 5, span = 1)

bsam_results <- get_summary(fit)
bsam_results_df <- as.data.frame(bsam_results)
save.image(file = FNAME)