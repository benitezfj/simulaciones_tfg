library(rmoo)
library(tidyverse)

measure_time <- function(object, number_objectives, ...) {
  elapsed_time <- Sys.time() - start_time
  # print(paste("Iteration:", object@iter, "Elapsed Time:", elapsed_time))

  write.table(data.frame("object" = object@iter, "Elapsed Time" = elapsed_time), con, sep = ",", col.names = FALSE, row.names = FALSE)
}

dtlz1_pareto <- 0.5 * rmoo::generate_reference_points(3, 12)

generic_sphere <- function(ref_dirs) {
  norm_ref_dirs <- sqrt(rowSums(ref_dirs^2))
  return(ref_dirs / matrix(rep(norm_ref_dirs, each=ncol(ref_dirs)), nrow=nrow(ref_dirs), byrow=TRUE))
}


dtlz2_pareto <- function(ref_dirs=rmoo::generate_reference_points(4, 5)) {
  return(generic_sphere(ref_dirs))
}


dtlz3_pareto <- function(ref_dirs=rmoo::generate_reference_points(6, 5)) {
  return(generic_sphere(ref_dirs))
}



dtlz7_pareto <- read.csv("~/Simulaciones/DTLZ7-3-PF.csv", header = FALSE)



BOUND_LOW <- 0
BOUND_UP <- 1
NGEN = 500
CXPB = 0.8
MUTPB = 0.2


dtlz1 <- function (x, nobj = 3,...) {
  if (is.null(dim(x))) {
    x <- matrix(x, 1)
  }
  n <- ncol(x)
  y <- matrix(x[, 1:(nobj - 1)], nrow(x))
  z <- matrix(x[, nobj:n], nrow(x))
  g <- 100 * (n - nobj + 1 + rowSums((z - 0.5)^2 - cos(20 *
                                                         pi * (z - 0.5))))
  tmp <- t(apply(y, 1, cumprod))
  tmp <- cbind(t(apply(tmp, 1, rev)), 1)
  tmp2 <- cbind(1, t(apply(1 - y, 1, rev)))
  f <- tmp * tmp2 * 0.5 * (1 + g)
  return(f)
}


NDIM <- 4
NOBJ <- 3
P <- 12
ref_dirs <- rmoo::generate_reference_points(NOBJ, P)
MU <- nrow(ref_dirs)

i <- 10
time_file <- paste0("rmoo_time_nsga2_dtlz1_92_500_3_4-", i, ".csv")
con <- file(time_file, open = "w")
# con <- file("rmoo_time_nsga2_dtlz1_92_500_3_4.csv", open = "w")
start_time <- Sys.time()
res <- rmoo::rmoo(type = "real-valued",
                  algorithm = "NSGA-II",
                  fitness = dtlz1,
                  lower = rep(BOUND_LOW,NDIM),
                  upper = rep(BOUND_UP,NDIM),
                  popSize = MU,
                  maxiter = NGEN,
                  nObj = NOBJ,
                  pcrossover = CXPB,
                  pmutation = MUTPB,
                  monitor = measure_time,
                  summary = FALSE,
                  parallel = FALSE,
                  seed = i)
close(con)
# fitness_file <- paste0("rmoo_fitness_nsga2_dtlz1_92_500_3_4-", i, ".csv")
# write.csv(res1@fitness, file = fitness_file, row.names = FALSE)
# write.csv(res@fitness, file = "rmoo_fitness_nsga2_dtlz1_92_500_3_4.csv", row.names = FALSE)

plot(res)


i <- 10
time_file <- paste0("rmoo_time_nsga3_dtlz1_92_500_3_4-", i, ".csv")
con <- file(time_file, open = "w")
# con <- file("rmoo_time_nsga3_dtlz1_92_500_3_4.csv", open = "w")
start_time <- Sys.time()
res1 <- rmoo::rmoo(type = "real-valued",
                   algorithm = "NSGA-III",
                   fitness = dtlz1,
                   lower = rep(BOUND_LOW,NDIM),
                   upper = rep(BOUND_UP,NDIM),
                   popSize = MU,
                   maxiter = NGEN,
                   nObj = NOBJ,
                   reference_dirs = ref_dirs,
                   pcrossover = CXPB,
                   pmutation = MUTPB,
                   monitor = measure_time,
                   summary = FALSE,
                   parallel = FALSE,
                   seed = i)
close(con)
# fitness_file <- paste0("rmoo_fitness_nsga3_dtlz1_92_500_3_4-", i, ".csv")
# write.csv(res1@fitness, file = fitness_file, row.names = FALSE)
# write.csv(res1@fitness, file = "rmoo_fitness_nsga3_dtlz1_92_500_3_4.csv", row.names = FALSE)

plot(res1)













dtlz2 <- function (x, nobj = 4, ...) {
  if (is.null(dim(x))) {
    x <- matrix(x, 1)
  }
  n <- ncol(x)
  y <- matrix(x[, 1:(nobj - 1)], nrow(x))
  z <- matrix(x[, nobj:n], nrow(x))
  g <- rowSums((z - 0.5)^2)
  tmp <- t(apply(cos(y * pi/2), 1, cumprod))
  tmp <- cbind(t(apply(tmp, 1, rev)), 1)
  tmp2 <- cbind(1, t(apply(sin(y * pi/2), 1, rev)))
  f <- tmp * tmp2 * (1 + g)
}

NDIM <- 5
NOBJ <- 4
P <- 7
ref_dirs <- rmoo::generate_reference_points(NOBJ, P)
MU <- nrow(ref_dirs)

i <- 10
time_file <- paste0("rmoo_time_nsga2_dtlz2_120_500_4_5-", i, ".csv")
con <- file(time_file, open = "w")
# con <- file("rmoo_time_nsga2_dtlz2_120_500_4_5.csv", open = "w")
start_time <- Sys.time()
res <- rmoo::rmoo(type = "real-valued",
                  fitness = dtlz2,
                  algorithm = "NSGA-II",
                  lower = rep(BOUND_LOW,NDIM),
                  upper = rep(BOUND_UP,NDIM),
                  popSize = MU,
                  maxiter = NGEN,
                  nObj = NOBJ,
                  pcrossover = CXPB,
                  pmutation = MUTPB,
                  monitor = measure_time,
                  summary = FALSE,
                  parallel = FALSE,
                  seed = i)
close(con)
# fitness_file <- paste0("rmoo_fitness_nsga2_dtlz2_120_500_4_5-", i, ".csv")
# write.csv(res1@fitness, file = fitness_file, row.names = FALSE)
# write.csv(res@fitness, file = "rmoo_fitness_nsga2_dtlz2_120_500_4_5.csv", row.names = FALSE)

i <- 10
time_file <- paste0("rmoo_time_nsga3_dtlz2_120_500_4_5-", i, ".csv")
con <- file(time_file, open = "w")
# con <- file("rmoo_time_nsga3_dtlz2_120_500_4_5.csv", open = "w")
start_time <- Sys.time()
res <- rmoo::rmoo(type = "real-valued",
                  fitness = dtlz2,
                  algorithm = "NSGA-III",
                  lower = rep(BOUND_LOW,NDIM),
                  upper = rep(BOUND_UP,NDIM),
                  popSize = MU,
                  maxiter = NGEN,
                  nObj = NOBJ,
                  pcrossover = CXPB,
                  pmutation = MUTPB,
                  reference_dirs = ref_dirs,
                  monitor = measure_time,
                  summary = FALSE,
                  parallel = FALSE,
                  seed = i)
close(con)
# fitness_file <- paste0("rmoo_fitness_nsga3_dtlz2_120_500_4_5-", i, ".csv")
# write.csv(res1@fitness, file = fitness_file, row.names = FALSE)
# write.csv(res@fitness, file = "rmoo_fitness_nsga3_dtlz2_120_500_4_5.csv", row.names = FALSE)










dtlz3 <- function (x, nobj = 6, ...) {
  if (is.null(dim(x))) {
    x <- matrix(x, 1)
  }
  n <- ncol(x)
  y <- matrix(x[, 1:(nobj - 1)], nrow(x))
  z <- matrix(x[, nobj:n], nrow(x))
  g <- 100 * (n - nobj + 1 + rowSums((z - 0.5)^2 - cos(20 *
                                                         pi * (z - 0.5))))
  tmp <- t(apply(cos(y * pi/2), 1, cumprod))
  tmp <- cbind(t(apply(tmp, 1, rev)), 1)
  tmp2 <- cbind(1, t(apply(sin(y * pi/2), 1, rev)))
  f <- tmp * tmp2 * (1 + g)
}


NDIM <- 6
NOBJ <- 6
P <- 5
ref_dirs <- rmoo::generate_reference_points(NOBJ, P)
MU <- nrow(ref_dirs)

i <- 10
time_file <- paste0("rmoo_time_nsga2_dtlz3_252_500_6_6-", i, ".csv")
con <- file(time_file, open = "w")
# con <- file("rmoo_time_nsga2_dtlz3_252_500_6_6.csv", open = "w")
start_time <- Sys.time()
res <- rmoo::rmoo(type = "real-valued",
                  algorithm = "NSGA-II",
                  fitness = dtlz3,
                  lower = rep(BOUND_LOW,NDIM),
                  upper = rep(BOUND_UP,NDIM),
                  popSize = MU,
                  maxiter = NGEN,
                  nObj = NOBJ,
                  pcrossover = CXPB,
                  pmutation = MUTPB,
                  monitor = measure_time,
                  summary = FALSE,
                  parallel = FALSE,
                  seed = i)
close(con)
# fitness_file <- paste0("rmoo_fitness_nsga2_dtlz3_252_500_6_6-", i, ".csv")
# write.csv(res1@fitness, file = fitness_file, row.names = FALSE)
# write.csv(res@fitness, file = "rmoo_fitness_nsga2_dtlz3_252_500_6_6.csv", row.names = FALSE)

plot(res)

i <- 10
time_file <- paste0("rmoo_time_nsga3_dtlz3_252_500_6_6-", i, ".csv")
con <- file(time_file, open = "w")
# con <- file("rmoo_time_nsga3_dtlz3_252_500_6_6.csv", open = "w")
start_time <- Sys.time()
res1 <- rmoo::rmoo(type = "real-valued",
                  algorithm = "NSGA-III",
                  fitness = dtlz3,
                  lower = rep(BOUND_LOW,NDIM),
                  upper = rep(BOUND_UP,NDIM),
                  popSize = MU,
                  maxiter = NGEN,
                  nObj = NOBJ,
                  pcrossover = CXPB,
                  pmutation = MUTPB,
                  reference_dirs = ref_dirs,
                  monitor = measure_time,
                  summary = FALSE,
                  parallel = FALSE,
                  seed = i)
close(con)
# fitness_file <- paste0("rmoo_fitness_nsga3_dtlz3_252_500_6_6-", i, ".csv")
# write.csv(res1@fitness, file = fitness_file, row.names = FALSE)
# write.csv(res1@fitness, file = "rmoo_fitness_nsga3_dtlz3_252_500_6_6.csv", row.names = FALSE)

plot(res1)




dtlz7 <- function (x, nobj = 3,...) {
  if (is.null(dim(x))) {
    x <- matrix(x, 1)
  }
  n <- ncol(x)
  y <- matrix(x[, 1:(nobj - 1)], nrow(x))
  z <- matrix(x[, nobj:n], nrow(x))
  g <- 1 + 9 * rowSums(z/(1:(n - nobj + 1)))
  tmp <- cbind(y, 1)
  tmp2 <- cbind(matrix(1, nrow(x), nobj - 1), (1 + g) * (nobj -
                                                           rowSums(y/(1 + g) * (1 + sin(3 * pi * y)))))
  f <- tmp * tmp2
}

NDIM <- 10
NOBJ <- 3
P <- 12
ref_dirs <- rmoo::generate_reference_points(NOBJ, P)
MU <- nrow(ref_dirs)

i <- 10
time_file <- paste0("rmoo_time_nsga2_dtlz7_92_500_3_10-", i, ".csv")
con <- file(time_file, open = "w")
# con <- file("rmoo_time_nsga2_dtlz7_92_500_3_10.csv", open = "w")
start_time <- Sys.time()
res <- rmoo::rmoo(type = "real-valued",
                  algorithm = "NSGA-II",
                  fitness = dtlz7,
                  lower = rep(BOUND_LOW,NDIM),
                  upper = rep(BOUND_UP,NDIM),
                  popSize = MU,
                  maxiter = NGEN,
                  nObj = NOBJ,
                  pcrossover = CXPB,
                  pmutation = MUTPB,
                  monitor = measure_time,
                  summary = FALSE,
                  parallel = FALSE,
                  seed = i)
close(con)
# fitness_file <- paste0("rmoo_fitness_nsga2_dtlz7_92_500_3_10-", i, ".csv")
# write.csv(res1@fitness, file = fitness_file, row.names = FALSE)
# write.csv(res@fitness, file = "rmoo_fitness_nsga2_dtlz7_92_500_3_10.csv", row.names = FALSE)

plot(res)
plot(res, type="pcp")


i <- 10
time_file <- paste0("rmoo_time_nsga3_dtlz7_92_500_3_10-", i, ".csv")
con <- file(time_file, open = "w")
# con <- file("rmoo_time_nsga3_dtlz7_92_500_3_10.csv", open = "w")
start_time <- Sys.time()
res1 <- rmoo::rmoo(type = "real-valued",
                   algorithm = "NSGA-III",
                   fitness = dtlz7,
                   lower = rep(BOUND_LOW,NDIM),
                   upper = rep(BOUND_UP,NDIM),
                   popSize = MU,
                   maxiter = NGEN,
                   nObj = NOBJ,
                   pcrossover = CXPB,
                   pmutation = MUTPB,
                   reference_dirs = ref_dirs,
                   monitor = measure_time,
                   summary = FALSE,
                   parallel = FALSE,
                   seed = i)
close(con)
# fitness_file <- paste0("rmoo_fitness_nsga3_dtlz7_92_500_3_10-", i, ".csv")
# write.csv(res1@fitness, file = fitness_file, row.names = FALSE)
# write.csv(res1@fitness, file = "rmoo_fitness_nsga3_dtlz7_92_500_3_10.csv", row.names = FALSE)

plot(res1)
plot(res1, type="pcp")
