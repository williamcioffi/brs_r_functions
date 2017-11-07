library(foreach)
library(doMC)
library(rbenchmark)

registerDoMC(cores = 4)

benchmark(
outs <- foreach(n = 1:10) %dopar% {
	n1 <- build_null_diver(beh[[1]], deployid = "n1")	
	n2 <- build_null_diver(beh[[2]], deployid = "n2")
	
	nullcompare <- compare_dives(n1, n2)
	areas <- calc_overlap(abs(nullcompare$diff_time), abs(realcompare$diff_time))
	
	list(nullcompare = nullcompare, areas = areas)
},

outs <- foreach(n = 1:10) %do% {
	n1 <- build_null_diver(beh[[1]], deployid = "n1")	
	n2 <- build_null_diver(beh[[2]], deployid = "n2")
	
	nullcompare <- compare_dives(n1, n2)
	areas <- calc_overlap(abs(nullcompare$diff_time), abs(realcompare$diff_time))
	
	list(nullcompare = nullcompare, areas = areas)
},
replications = 1
)