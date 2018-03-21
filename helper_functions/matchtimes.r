###
# tiny helper function
# ~wrc

matchtimes <- function(t1, t2) {
# t1, t2 are numeric
	findInterval(t1, c(-Inf, head(t2, -1)) + c(0, diff(t2)/2))
}