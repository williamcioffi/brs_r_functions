build_null_diver_vectorized_nonnormal <- function(baseline, deployid = "nulldiver01", DIVEMINDUR = 33*60) {
# require(truncnorm)

sim_start_time <- as.character(min(as.POSIXct(baseline$Start, tz = "UTC")))

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

surfs <- which(baseline$What == "Surface")
dives <- which(baseline$What == "Dive")
nevents <- max(c(length(surfs), length(dives)))

divef <- approxfun(density(dur[dives]))
divedurseq <- min(dur[dives]):max(dur[dives])

dive_simdurs <- sample(divedurseq, size = nevents, replace = TRUE, prob = divef(divedurseq))
dive_simdeps <- simulate_depths(dive_simdurs, cfs, divemod_sig)
dive_simshps <- rep("U", nevents)
dive_simwhat <- rep("Dive", nevents)

surff <- approxfun(density(dur[surfs]))
surfdurseq <- min(dur[surfs]):max(dur[dives])

surf_simdurs <- sample(surfdurseq, size = nevents, replace = TRUE, prob = surff(surfdurseq))
surf_simdeps <- rep(NA, nevents)
surf_simshps <- rep(NA, nevents)
surf_simwhat <- rep("Surface", nevents)

simdurs <- c(rbind(dive_simdurs, surf_simdurs))
simdeps <- c(rbind(dive_simdeps, surf_simdeps))
simshps <- c(rbind(dive_simshps, surf_simshps))
simwhat <- c(rbind(dive_simwhat, surf_simwhat))

nsimed <- nevents*2
st <- vector(mode = "numeric", length = nsimed)
st[1] <- sim_start_time
en <- as.character(as.POSIXct(st[1], tz = "UTC") +  cumsum(simdurs))
st[2: nsimed] <- en[1:(nsimed - 1)]

op <- options("stringsAsFactors")
options(stringsAsFactors = FALSE)
on.exit(options(op))

b1 <- data.frame(DeployID = deployid, What = simwhat, DurationMax = simdurs, DurationMin = simdurs, DepthMax = simdeps, DepthMin = simdeps, Shape = simshps, Start = st, End = en)

#make it all one big message for compatibility with code that expects there to be messages
rbind(data.frame(DeployID = deployid, What = "Message", DurationMax = NA, DurationMin = NA, DepthMax = NA, DepthMin = NA, Shape = NA, Start = st[1], End = en[length(en)]),
b1)

}