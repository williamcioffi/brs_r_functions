### null based on distributions


# only need to run once
ff <- list.files()
dese <- grep("Behavior", ff)
ff <- ff[dese]
nf <- length(ff)

b <- vector()

for(i in 1:nf) {
	tmp <- read.table(ff[i], header = TRUE, sep = ',', stringsAsFactors = FALSE)
	b <- rbind(b, tmp)
}

save("b", file = "behaviorbaseline.RData")
# end: only need to run once


# tester
load("behaviorbaseline.RData")

b1 <- build_null_diver(b, "null01")
b2 <- build_null_diver(b, "null02")
b3 <- build_null_diver(b, "null03")
# end: tester

build_null_diver <- function(baseline, deployid = "nulldiver01", sim_start_time = NULL, sim_end_time = NULL) {
require(truncnorm)

if(is.null(sim_start_time) | is.null(sim_end_time)) {
	sim_start_time <- as.character(min(as.POSIXct(baseline$Start, tz = "GMT")))
	sim_end_time <- as.character(max(as.POSIXct(baseline$Start, tz = "GMT")))
}

dur <- apply(baseline[, c('DurationMax', 'DurationMin')], 1, mean)
dep <- apply(baseline[, c('DepthMax', 'DepthMin')], 1, mean)

divedep <- dep[which(baseline$What == "Dive")]
divedur <- dur[which(baseline$What == "Dive")]

divemod <- lm(divedep ~ divedur + I(divedur^2) + I(divedur^3))
xx <- seq(min(divedur), max(divedur), length = 2000)
cfs <- coef(divemod)
divemod_sig <- sigma(divemod)

simulate_depths <- function(xx, cfs, divemod_sig) {
	deps <- cfs[1] + cfs[2]*xx + cfs[3]*xx^2 + cfs[4]*xx^3 + rnorm(length(xx), mean = 0, sd = divemod_sig)
	deps[which(deps < 0)] <- 0
	deps
}


surf_bar <- mean(dur[which(baseline$What == "Surface")])
surf_sig <- sd  (dur[which(baseline$What == "Surface")])

dive_bar <- mean(dur[which(baseline$What == "Dive")])
dive_sig <- sd  (dur[which(baseline$What == "Dive")])


# nsim <- num_events
# simsurfs <- rtruncnorm(nsim, a = 0, mean = surf_bar, sd = surf_sig)
# simdives <- rtruncnorm(nsim, a = 0, mean = dive_bar, sd = dive_sig)
# simdeps  <- simulate_depths(simdives, cfs, divemod_sig)

# simdurs <- round(c(rbind(simsurfs, simdives)))
# simdeps <- round(c(rbind(rep(NA, nsim), simdeps)))
# simwhat <- rep(c("Surface", "Dive"), nsim)


st <- vector()
en <- vector()
simdurs <- vector()
simdeps <- vector()
simwhat <- vector()
st[1] <- sim_start_time
isdive <- FALSE
i <- 1

while(as.POSIXct(st[i], tz = "GMT") < as.POSIXct(sim_end_time, tz = "GMT")) {
	if(isdive) {
		tmp_simdurs <- rtruncnorm(1, a = 0, mean = dive_bar, sd = dive_sig)
		tmp_simdeps <- simulate_depths(tmp_simdurs, cfs, divemod_sig)
		tmp_simwhat <- "Dive"
		
		isdive <- FALSE
	} else {
		tmp_simdurs <- rtruncnorm(1, a = 0, mean = surf_bar, sd = surf_sig)
		tmp_simdeps <- NA
		tmp_simwhat <- "Surface"
		
		isdive <- TRUE
	}
	
	simdurs[i] <- tmp_simdurs
	simdeps[i] <- tmp_simdeps
	simwhat[i] <- tmp_simwhat
	
	en[i] <-  as.character(as.POSIXct(st[i], tz = "GMT") + tmp_simdurs)
	st[i + 1] <- en[i]
	
	i <- i + 1
}

st <- st[1:length(en)]


b1 <- data.frame(DeployID = deployid, What = simwhat, DurationMax = simdurs, DurationMin = simdurs, DepthMax = simdeps, DepthMin = simdeps, Shape = "U", Start = st, End = en)

#make it all one big message for compatibility with code that expects there to be messages
rbind(data.frame(DeployID = deployid, What = "Message", DurationMax = NA, DurationMin = NA, DepthMax = NA, DepthMin = NA, Shape = NA, Start = st[1], End = en[length(en)]),
b1)

}