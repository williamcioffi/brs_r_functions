# how do i calculate overlap between two distributions?

calc_overlap <- function(y0, y1, plot = FALSE) {

# y0 <- rnorm(100, mean = 0)
# y1 <- rnorm(100, mean = 2)

y0yy <- density(y0)$y
y1yy <- density(y1)$y

y0xx <- density(y0)$x
y1xx <- density(y1)$x

f0 <- approxfun(y0xx, y0yy, rule = 2)
f1 <- approxfun(y1xx, y1yy, rule = 2)

minx <- min(c(y0xx, y1xx))
maxx <- max(c(y0xx, y1xx))

xx <- seq(minx, maxx, length = 512)

fsub <- approxfun(xx, abs(f0(xx) - f1(xx)))
fmax <- approxfun(xx, apply(cbind(f0(xx), f1(xx)), 1, max))

if(plot) {
	plot(c(y0xx, y1xx), c(y0yy, y1yy), type = 'n')
	lines(y1xx, y1yy, col = "blue")
	lines(y0xx, y0yy, col = "purple")
	lines(seq(minx, maxx, length = 100), abs(fsub(seq(minx, maxx, length = 100))))
	lines(seq(minx, maxx, length = 100), fmax(seq(minx, maxx, length = 100)), lty = 2)
}
overlap <- integrate(fmax, minx, maxx)[[1]] - integrate(fsub, minx, maxx)[[1]]
null <- integrate(f0, minx, maxx)[[1]]
real <- integrate(f1, minx, maxx)[[1]]

list(overlap = overlap, null = null, real = real)
}
