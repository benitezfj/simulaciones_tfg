library(rmoo)
library(tidyverse)
library(ggplot2)

measure_time <- function(object, number_objectives, ...) {
  elapsed_time <- Sys.time() - start_time
  # print(paste("Iteration:", object@iter, "Elapsed Time:", elapsed_time))

  write.table(data.frame("object" = object@iter, "Elapsed Time" = elapsed_time), con, sep = ",", col.names = FALSE, row.names = FALSE)
}

NOBJ <- 2
MU <- 100
BOUND_LOW <- 0
BOUND_UP <- 1
NGEN = 500
CXPB = 0.8
MUTPB = 0.2

ref_points <- generate_reference_points(2,99)

# zdt1 <- function (x) {
#   if (is.null(dim(x))) {
#     x <- matrix(x, nrow = 1)
#   }
#   n <- ncol(x)
#   g <- 1 + rowSums(x[, 2:n, drop = FALSE]) * 9/(n - 1)
#   return(cbind(x[, 1], g * (1 - sqrt(x[, 1]/g))))
# }

zdt1 <- function(x) {
  n = length(x)
  f1 = x[1]
  g = 1 + 9 * sum(x[2:n])/(n - 1)
  h = 1 - sqrt(f1/g)
  f2 = g * h
  return(c(f1, f2))
}

NDIM <- 3

# con <- file("rmoo_time_nsga3_zdt1_100_500_2_3.csv", open = "w")
# start_time <- Sys.time()
res <- rmoo::rmoo(type = "real-valued",
                  algorithm = "NSGA-III",
                  fitness = zdt1,
                  lower = rep(BOUND_LOW,NDIM),
                  upper = rep(BOUND_UP,NDIM),
                  popSize = MU,
                  maxiter = 50000,
                  nObj = NOBJ,
                  pcrossover = 1,
                  pmutation = 1,
                  reference_dirs = ref_points,
                  monitor = FALSE,
                  summary = FALSE,
                  parallel = FALSE,
                  seed = 43)
plot(res)
write.csv(res@fitness, file = "rmoo_fitness_nsga3_zdt1_100_500_2_3.csv", row.names = FALSE)


res1 <- rmoo::rmoo(type = "real-valued",
                  algorithm = "NSGA-II",
                  fitness = zdt1,
                  lower = rep(BOUND_LOW,NDIM),
                  upper = rep(BOUND_UP,NDIM),
                  popSize = MU,
                  maxiter = 50000,
                  nObj = NOBJ,
                  pcrossover = 1,
                  pmutation = 1,
                  monitor = FALSE,
                  summary = FALSE,
                  parallel = FALSE,
                  seed = 43)
close(con)
plot(res1)
write.csv(res1@fitness, file = "rmoo_fitness_nsga2_zdt1_100_500_2_3.csv", row.names = FALSE)

opt <- read.csv("C:/Users/Maria/Downloads/Tesis/Simulaciones/resultados/ZDT/deap_nsga2_zdt1.csv")




# zdt2 <- function (x) {
#   if (is.null(dim(x))) {
#     x <- matrix(x, nrow = 1)
#   }
#   n <- ncol(x)
#   g <- 1 + rowSums(x[, 2:n, drop = FALSE]) * 9/(n - 1)
#   return(cbind(x[, 1], g * (1 - (x[, 1]/g)^2)))
# }

zdt2 <- function(x) {
  n = length(x)
  f1 = x[1]
  g = 1 + 9 * sum(x[2:n])/(n - 1)
  h = 1 - (f1/g)^2
  f2 = g * h
  return(c(f1, f2))
}

NDIM <- 4

# con <- file("rmoo_time_nsga3_zdt2_100_500_2_4.csv", open = "w")
# start_time <- Sys.time()
res <- rmoo::rmoo(type = "real-valued",
                  algorithm = "NSGA-III",
                  fitness = zdt2,
                  lower = rep(BOUND_LOW,NDIM),
                  upper = rep(BOUND_UP,NDIM),
                  popSize = MU,
                  maxiter = 50000,
                  nObj = NOBJ,
                  pcrossover = 0.9,
                  pmutation = 0.1,
                  reference_dirs = ref_points,
                  monitor = FALSE,
                  summary = FALSE,
                  parallel = FALSE,
                  seed = 54)
# close(con)
plot(res, optimal=opt)
write.csv(res@fitness, file = "rmoo_fitness_nsga3_zdt2_100_500_2_4.csv", row.names = FALSE)


res1 <- rmoo::rmoo(type = "real-valued",
                  algorithm = "NSGA-II",
                  fitness = zdt2,
                  lower = rep(BOUND_LOW,NDIM),
                  upper = rep(BOUND_UP,NDIM),
                  popSize = MU,
                  maxiter = 50000,
                  nObj = NOBJ,
                  pcrossover = 1,
                  pmutation = 1,
                  monitor = FALSE,
                  summary = FALSE,
                  parallel = FALSE,
                  seed = 7)

plot(res1, optimal=opt)

write.csv(res1@fitness, file = "rmoo_fitness_nsga2_zdt2_100_500_2_4.csv", row.names = FALSE)

opt <- read.csv("C:/Users/Maria/Downloads/Tesis/Simulaciones/resultados/ZDT/deap_nsga2_zdt2.csv")

# zdt3 <- function (x) {
#   if (is.null(dim(x))) {
#     x <- matrix(x, nrow = 1)
#   }
#   n <- ncol(x)
#   g <- 1 + rowSums(x[, 2:n, drop = FALSE]) * 9/(n - 1)
#   return(cbind(x[, 1], g * (1 - sqrt(x[, 1]/g) - x[, 1]/g *
#                               sin(10 * pi * x[, 1]))))
# }

zdt3 <- function(x) {
  n = length(x)
  f1 = x[1]
  g = 1 + 9 * sum(x[2:n])/(n - 1)
  h = 1 - sqrt(f1/g) - (f1/g) * sin(10 * pi * f1)
  f2 = g * h
  return(c(f1, f2))
}


NDIM <- 5
con <- file("rmoo_time_nsga2_zdt3_100_500_2_5.csv", open = "w")
start_time <- Sys.time()
res <- rmoo::rmoo(type = "real-valued",
                  algorithm = "NSGA-II",
                  fitness = zdt3,
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
                  seed = 54)
close(con)
plot(res, optimal=opt)
write.csv(res@fitness, file = "rmoo_fitness_nsga2_zdt3_100_500_2_5.csv", row.names = FALSE)


con <- file("rmoo_time_nsga3_zdt3_100_500_2_5.csv", open = "w")
start_time <- Sys.time()
res1 <- rmoo::rmoo(type = "real-valued",
                  algorithm = "NSGA-III",
                  fitness = zdt3,
                  lower = rep(BOUND_LOW,NDIM),
                  upper = rep(BOUND_UP,NDIM),
                  popSize = MU,
                  maxiter = NGEN,
                  nObj = NOBJ,
                  pcrossover = CXPB,
                  pmutation = MUTPB,
                  monitor = measure_time,
                  reference_dirs = ref_points,
                  summary = FALSE,
                  parallel = FALSE,
                  seed = 54)
close(con)
plot(res1, optimal=opt)
write.csv(res1@fitness, file = "rmoo_fitness_nsga3_zdt3_100_500_2_5.csv", row.names = FALSE)

opt <- read.csv("C:/Users/Maria/Downloads/Tesis/Simulaciones/resultados/ZDT/deap_nsga2_zdt3.csv")

# zdt4 <- function (x) {
#   if (is.null(dim(x))) {
#     x <- matrix(x, nrow = 1)
#   }
#   n <- ncol(x)
#   g <- 1 + 10 * (n - 1) + rowSums((x[, 2:n, drop = FALSE] *
#                                      10 - 5)^2 - 10 * cos(4 * pi * (x[, 2:n, drop = FALSE] *
#                                                                       10 - 5)))
#   return(cbind(x[, 1], g * (1 - sqrt(x[, 1]/g))))
# }

zdt4 <- function(x) {
  n = length(x)
  f1 = x[1]
  g = 1 + 10 * (n - 1) + sum(x[2:n]^2 - 10 * cos(4 * pi *
                                                   x[2:n]))
  h = 1 - sqrt(f1/g)
  f2 = g * h
  return(c(f1, f2))
}

NDIM <- 7
# con <- file("rmoo_time_nsga3_zdt4_100_500_2_7.csv", open = "w")
# start_time <- Sys.time()

res <- rmoo::rmoo(type = "real-valued",
                  algorithm = "NSGA-II",
                  fitness = zdt4,
                  lower = rep(BOUND_LOW,NDIM),
                  upper = rep(BOUND_UP,NDIM),
                  popSize = MU,
                  maxiter = 5000,
                  nObj = NOBJ,
                  pcrossover = 0.9,
                  pmutation = 0.1,
                  monitor = FALSE,
                  summary = FALSE,
                  parallel = FALSE,
                  seed = 54)
# close(con)
plot(res, optimal=opt)

write.csv(res@fitness, file = "rmoo_fitness_nsga2_zdt4_100_500_2_7.csv", row.names = FALSE)


res1 <- rmoo::rmoo(type = "real-valued",
                  algorithm = "NSGA-III",
                  fitness = zdt4,
                  lower = rep(BOUND_LOW,NDIM),
                  upper = rep(BOUND_UP,NDIM),
                  popSize = MU,
                  maxiter = 5000,
                  nObj = NOBJ,
                  pcrossover = 0.9,
                  pmutation = 0.1,
                  monitor = FALSE,
                  reference_dirs = ref_points,
                  summary = FALSE,
                  parallel = FALSE,
                  seed = 54)
# close(con)
plot(res1, optimal=opt)
write.csv(res1@fitness, file = "rmoo_fitness_nsga3_zdt4_100_500_2_7.csv", row.names = FALSE)

opt <- read.csv("C:/Users/Maria/Downloads/Tesis/Simulaciones/resultados/ZDT/deap_nsga2_zdt4.csv")



zdt5 <- function (x, m = 10, n = 5, normal = TRUE, ...) {
  if (length(x)%%5 != 0 && length(x) < 35)
  {
    stop("Bit vector's length must contain at least 35 digits.")
  }
  x1 <- x[1:30]
  xm <- x[31:length(x)]
  g <- 0
  vu <- function(v){
    if (v < 5){
      x <- 2 + v
    }else if (v == 5) {
      x <- 1
    }
    return(x)
  }

  for (i in seq(m)) {
    x <- sum(xm[((n * (i - 1)) + 1):(n * i)])
    v <- vu(x)
    g <- g + v
  }
  f1 <- 1 + sum(x1 == 1)
  f2 <- g * (1 / f1)

  normalize <- function(x, x_min, x_max) {
    # calculate the denominator
    denom <- x_max - x_min
    # we can not divide by zero -> plus small epsilon
    denom <- denom + 1e-30
    # normalize the actual values
    N <- (x - x_min) / denom
    return(N)
  }

  if (normal == TRUE){
    f1 <- normalize(f1, 1, 30)
    f2 <- normalize(f2, (m - 1) * 1 / 30, (m - 1))
  }
  return(cbind(f1,f2))
}

NDIM <- 80
con <- file("rmoo_time_nsga3_zdt5_100_500_2_80.csv", open = "w")
start_time <- Sys.time()
res <- rmoo(type = "binary",
            fitness = zdt5,
            algorithm = "NSGA-III",
            nBits = NDIM,
            crossover = pointCrossover,
            mutation = rmoobin_fbMutation,
            popSize = MU,
            maxiter = NGEN,
            nObj = NOBJ,
            pcrossover = CXPB,
            pmutation = MUTPB,
            reference_dirs = ref_points,
            monitor = measure_time,
            summary = FALSE,
            seed = 1)
close(con)
write.csv(res@fitness, file = "rmoo_fitness_nsga3_zdt5_100_500_2_80.csv", row.names = FALSE)





# zdt6 <- function (x) {
#   if (is.null(dim(x))) {
#     x <- matrix(x, nrow = 1)
#   }
#   n <- ncol(x)
#   f1 <- 1 - exp(-4 * x[, 1]) * (sin(6 * pi * x[, 1]))^6
#   g <- 1 + 9 * (1/(n - 1) * rowSums(x[, 2:n, drop = FALSE]))^(0.25)
#   return(cbind(f1, g * (1 - (f1/g)^2)))
# }
zdt6 <- function(x) {
  n = length(x)
  f1 = 1 - exp(-4 * x[1]) * (sin(6 * pi * x[1]))^6
  g = 1 + 9 * (sum(x[2:n])/(n - 1))^(0.25)
  h = 1 - (f1/g)^2
  f2 = g * h
  return(c(f1, f2))
}

NDIM <- 10
# con <- file("rmoo_time_nsga3_zdt6_100_500_2_10.csv", open = "w")
# start_time <- Sys.time()
res <- rmoo::rmoo(type = "real-valued",
                  algorithm = "NSGA-II",
                  fitness = zdt6,
                  lower = rep(BOUND_LOW,NDIM),
                  upper = rep(BOUND_UP,NDIM),
                  popSize = MU,
                  maxiter = 5000,
                  nObj = NOBJ,
                  pcrossover = 0.9,
                  pmutation = 0.1,
                  monitor = FALSE,
                  summary = FALSE,
                  parallel = FALSE,
                  seed = 274)
# close(con)
plot(res, optimal = opt)
write.csv(res@fitness, file = "rmoo_fitness_nsga2_zdt6_100_500_2_10.csv", row.names = FALSE)


res1 <- rmoo::rmoo(type = "real-valued",
                  algorithm = "NSGA-III",
                  fitness = zdt6,
                  lower = rep(BOUND_LOW,NDIM),
                  upper = rep(BOUND_UP,NDIM),
                  popSize = MU,
                  maxiter = 5000,
                  nObj = NOBJ,
                  pcrossover = 0.9,
                  pmutation = 0.1,
                  monitor = FALSE,
                  reference_dirs = ref_points,
                  summary = FALSE,
                  parallel = FALSE,
                  seed = 9)
# close(con)
plot(res1, optimal = opt)
write.csv(res1@fitness, file = "rmoo_fitness_nsga3_zdt6_100_500_2_10.csv", row.names = FALSE)


opt <- read.csv("C:/Users/Maria/Downloads/Tesis/Simulaciones/resultados/ZDT/pymoo_nsga2_zdt6.csv")



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

BOUND_LOW <- 0
BOUND_UP <- 1
NGEN = 500
CXPB = 0.8
MUTPB = 0.2

ref_dirs <- rmoo::generate_reference_points(NOBJ, P)
MU <- nrow(ref_dirs)

# con <- file("rmoo_time_nsga2_dtlz1_92_500_3_4.csv", open = "w")
# start_time <- Sys.time()
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
                  seed = 54)
plot(res)


res1 <- rmoo::rmoo(type = "real-valued",
                  algorithm = "NSGA-III",
                  fitness = dtlz1,
                  lower = rep(BOUND_LOW,NDIM),
                  upper = rep(BOUND_UP,NDIM),
                  popSize = MU,
                  maxiter = 10000,
                  nObj = NOBJ,
                  reference_dirs = ref_dirs,
                  pcrossover = 0.9,
                  pmutation = 0.2,
                  monitor = FALSE,
                  summary = FALSE,
                  parallel = FALSE,
                  seed = 73)

plot(res1)
plot(res1, optimal=(0.5 * rmoo::generate_reference_points(3, 12)))
plot(res1, type="pcp")

# close(con)
write.csv(res1@fitness, file = "rmoo_fitness_nsga3_dtlz1_92_500_3_4.csv", row.names = FALSE)

# suggestions = rbind(c(0.66666905, 0.74999224, 0.50001427, 0.49999913),
#                     c(0.99999949, 0.41661999, 0.50001196, 0.49999905),
#                     c(0.66672483, 0.99993724, 0.50001426, 0.49999904)),




# res <- rmoo::nsga3(type = "real-valued",
#             fitness = dtlz1,
#             lower = rep(BOUND_LOW,NDIM),
#             upper = rep(BOUND_UP,NDIM),
#             popSize = MU,
#             maxiter = NGEN,
#             nObj = NOBJ,
#             pcrossover = CXPB,
#             pmutation = MUTPB,
#             reference_dirs = ref_dirs,
#             monitor = measure_time,
#             summary = FALSE,
#             parallel = FALSE,
#             seed = 1)










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

BOUND_LOW <- 0
BOUND_UP <- 1
NGEN = 500
CXPB = 0.8
MUTPB = 0.2

ref_dirs <- rmoo::generate_reference_points(NOBJ, P)
MU <- nrow(ref_dirs)

con <- file("rmoo_time_nsga2_dtlz2_120_500_4_5.csv", open = "w")
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
                  # reference_dirs = ref_dirs,
                  monitor = measure_time,
                  summary = FALSE,
                  parallel = FALSE,
                  seed = 1)

# res <- rmoo::nsga3(type = "real-valued",
#                    fitness = dtlz2,
#                    lower = rep(BOUND_LOW,NDIM),
#                    upper = rep(BOUND_UP,NDIM),
#                    popSize = MU,
#                    maxiter = NGEN,
#                    nObj = NOBJ,
#                    pcrossover = CXPB,
#                    pmutation = MUTPB,
#                    reference_dirs = ref_dirs,
#                    monitor = measure_time,
#                    summary = FALSE,
#                    parallel = FALSE,
#                    seed = 1)
close(con)
write.csv(res@fitness, file = "rmoo_fitness_nsga2_dtlz2_120_500_4_5.csv", row.names = FALSE)










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

BOUND_LOW <- 0
BOUND_UP <- 1
NGEN = 500
CXPB = 0.8
MUTPB = 0.2

ref_dirs <- rmoo::generate_reference_points(NOBJ, P)
MU <- nrow(ref_dirs)

con <- file("rmoo_time_nsga2_dtlz3_252_500_6_6.csv", open = "w")
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
                  # reference_dirs = ref_dirs,
                  monitor = measure_time,
                  summary = FALSE,
                  parallel = FALSE,
                  seed = 1)

# res <- rmoo::nsga3(type = "real-valued",
#                    fitness = dtlz3,
#                    lower = rep(BOUND_LOW,NDIM),
#                    upper = rep(BOUND_UP,NDIM),
#                    popSize = MU,
#                    maxiter = NGEN,
#                    nObj = NOBJ,
#                    pcrossover = CXPB,
#                    pmutation = MUTPB,
#                    reference_dirs = ref_dirs,
#                    monitor = measure_time,
#                    summary = FALSE,
#                    parallel = FALSE,
#                    seed = 1)
close(con)
write.csv(res@fitness, file = "rmoo_fitness_nsga2_dtlz3_252_500_6_6.csv", row.names = FALSE)






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

BOUND_LOW <- 0
BOUND_UP <- 1
NGEN = 500
CXPB = 0.8
MUTPB = 0.2

ref_dirs <- rmoo::generate_reference_points(NOBJ, P)
MU <- nrow(ref_dirs)

# con <- file("rmoo_time_nsga2_dtlz7_92_500_3_10.csv", open = "w")
# start_time <- Sys.time()
res <- rmoo::rmoo(type = "real-valued",
                  algorithm = "NSGA-II",
                  fitness = dtlz7,
                  lower = rep(BOUND_LOW,NDIM),
                  upper = rep(BOUND_UP,NDIM),
                  popSize = MU,
                  maxiter = 10000,
                  nObj = NOBJ,
                  pcrossover = 0.9,
                  pmutation = 0.1,
                  # reference_dirs = ref_dirs,
                  monitor = FALSE,
                  summary = FALSE,
                  parallel = FALSE,
                  seed = 54)
plot(res)
plot(res, type="pcp")

write.csv(res@fitness, file = "rmoo_fitness_nsga2_dtlz7_92_500_3_10.csv", row.names = FALSE)


res1 <- rmoo::rmoo(type = "real-valued",
                  algorithm = "NSGA-III",
                  fitness = dtlz7,
                  lower = rep(BOUND_LOW,NDIM),
                  upper = rep(BOUND_UP,NDIM),
                  popSize = MU,
                  maxiter = 5000,
                  nObj = NOBJ,
                  pcrossover = 0.9,
                  pmutation = 0.1,
                  reference_dirs = ref_dirs,
                  monitor = FALSE,
                  summary = FALSE,
                  parallel = FALSE,
                  seed = 54)

plot(res1)
plot(res1, type="pcp")
# close(con)
write.csv(res1@fitness, file = "rmoo_fitness_nsga3_dtlz7_92_500_3_10.csv", row.names = FALSE)


# ----------------MOEADr--------------
# ----------------Multi-objective Config--------------
NOBJ      <- 2
MU        <- 100
NGEN      <- 500
CXPB      <- 0.8
MUTPB     <- 0.2
BOUND_LOW <- 0
BOUND_UP  <- 1

# ---------ZDT-1-------
NDIM      <- 3

ZDT1 <- make_vectorized_smoof(prob.name  = "ZDT1",
                              dimensions = NDIM) #Number of decision variables

## 2: set input parameters
problem   <- list(name       = "ZDT1", # Name of the problem list
                  xmin       = rep(BOUND_LOW, NDIM), #Lower Bounds
                  xmax       = rep(BOUND_UP, NDIM), #Upper Bounds
                  m          = NOBJ) #Number of objectives

decomp    <- list(name       = "SLD", H = 99) # Descomposition approach: Using Simplex-lattice design

neighbors <- list(name       = "lambda",
                  T          = 20,
                  delta.p    = 1)

aggfun    <- list(name       = "wt")

variation <- list(list(name  = "sbx", #SBX crossover
                       etax  = 20,
                       pc = CXPB), #Crossover probability: 80%
                  list(name  = "polymut", #Polynomial Mutation
                       etam  = 20,
                       pm = MUTPB),
                  list(name  = "truncate"))

update    <- list(name       = "standard",
                  UseArchive = TRUE)

scaling   <- list(name       = "none")
constraint<- list(name       = "none")
stopcrit  <- list(list(name  = "maxiter",
                       maxiter  = NGEN))
showpars  <- list(show.iters = "dots",
                  showevery  = 10)
seed      <- 1

## MOEA/D
out <- moead(problem = problem,
              decomp = decomp, aggfun = aggfun, neighbors = neighbors, variation = variation,
              update = update, constraint = constraint, scaling = scaling, stopcrit = stopcrit,
              showpars = showpars, seed = seed)

# Plot output:
plot(out$Y[,1], out$Y[,2], type = "p", pch = 20)
write.csv(out$Y, file = "moeadr_fitness_moead_zdt1_100_500_2_3.csv", row.names = FALSE)


# -------ZDT-2-----------
NDIM      <- 4

ZDT2 <- make_vectorized_smoof(prob.name  = "ZDT2",
                              dimensions = NDIM) #Number of decision variables

## 2: set input parameters
problem   <- list(name       = "ZDT2", # Name of the problem list
                  xmin       = rep(BOUND_LOW, NDIM), #Lower Bounds
                  xmax       = rep(BOUND_UP, NDIM), #Upper Bounds
                  m          = NOBJ) #Number of objectives

# decomp    <- list(name       = "SLD", H = 99) # Descomposition approach: Using Simplex-lattice design
#
# neighbors <- list(name       = "lambda",
#                   T          = 20,
#                   delta.p    = 1)
#
# aggfun    <- list(name       = "wt")
#
# variation <- list(list(name  = "sbx", #SBX crossover
#                        etax  = 20,
#                        pc = CXPB), #Crossover probability: 80%
#                   list(name  = "polymut", #Polynomial Mutation
#                        etam  = 20,
#                        pm = MUTPB),
#                   list(name  = "truncate"))
#
# update    <- list(name       = "standard",
#                   UseArchive = TRUE)
#
# scaling   <- list(name       = "none")
# constraint<- list(name       = "none")
# stopcrit  <- list(list(name  = "maxiter",
#                        maxiter  = NGEN))
# showpars  <- list(show.iters = "dots",
#                   showevery  = 10)
# seed      <- 1

## MOEA/D
out <- moead(problem = problem,
             decomp = decomp, aggfun = aggfun, neighbors = neighbors, variation = variation,
             update = update, constraint = constraint, scaling = scaling, stopcrit = stopcrit,
             showpars = showpars, seed = seed)

# Plot output:
plot(out$Y[,1], out$Y[,2], type = "p", pch = 20)
write.csv(out$Y, file = "moeadr_fitness_moead_zdt2_100_500_2_4.csv", row.names = FALSE)


# -------ZDT-3-----------
NDIM      <- 5

ZDT3 <- make_vectorized_smoof(prob.name  = "ZDT3",
                              dimensions = NDIM) #Number of decision variables

## 2: set input parameters
problem   <- list(name       = "ZDT3", # Name of the problem list
                  xmin       = rep(BOUND_LOW, NDIM), #Lower Bounds
                  xmax       = rep(BOUND_UP, NDIM), #Upper Bounds
                  m          = NOBJ) #Number of objectives

#decomp    <- list(name       = "SLD",
#                  H = 99) # Descomposition approach: Using Simplex-lattice design

# neighbors <- list(name       = "lambda",
#                   T          = 20,
#                   delta.p    = 1)

# aggfun    <- list(name       = "wt")

# variation <- list(list(name  = "sbx", #SBX crossover
#                        etax  = 20,
#                        pc = CXPB), #Crossover probability: 80%
#                   list(name  = "polymut", #Polynomial Mutation
#                        etam  = 20,
#                        pm = MUTPB),
#                   list(name  = "truncate"))

# update    <- list(name       = "standard",
#                   UseArchive = TRUE)

# scaling   <- list(name       = "none")
# constraint<- list(name       = "none")
# stopcrit  <- list(list(name  = "maxiter",
#                        maxiter  = NGEN))
# showpars  <- list(show.iters = "dots",
#                   showevery  = 10)
# seed      <- 1

## MOEA/D
out <- moead(problem = problem,
             decomp = decomp, aggfun = aggfun, neighbors = neighbors, variation = variation,
             update = update, constraint = constraint, scaling = scaling, stopcrit = stopcrit,
             showpars = showpars, seed = seed)

# Plot output:
plot(out$Y[,1], out$Y[,2], type = "p", pch = 20)
write.csv(out$Y, file = "moeadr_fitness_moead_zdt3_100_500_2_5.csv", row.names = FALSE)


# -------ZDT-4-----------
NDIM      <- 7

ZDT4 <- make_vectorized_smoof(prob.name  = "ZDT4",
                              dimensions = NDIM) #Number of decision variables

## 2: set input parameters
problem   <- list(name       = "ZDT4", # Name of the problem list
                  xmin       = rep(BOUND_LOW, NDIM), #Lower Bounds
                  xmax       = rep(BOUND_UP, NDIM), #Upper Bounds
                  m          = NOBJ) #Number of objectives

## MOEA/D
out <- moead(problem = problem,
             decomp = decomp, aggfun = aggfun, neighbors = neighbors, variation = variation,
             update = update, constraint = constraint, scaling = scaling, stopcrit = stopcrit,
             showpars = showpars, seed = seed)

# Plot output:
plot(out$Y[,1], out$Y[,2], type = "p", pch = 20)
write.csv(out$Y, file = "moeadr_fitness_moead_zdt4_100_500_2_7.csv", row.names = FALSE)


# -------ZDT-6-----------
NDIM      <- 10

ZDT6 <- make_vectorized_smoof(prob.name  = "ZDT6",
                              dimensions = NDIM) #Number of decision variables

## 2: set input parameters
problem   <- list(name       = "ZDT6", # Name of the problem list
                  xmin       = rep(BOUND_LOW, NDIM), #Lower Bounds
                  xmax       = rep(BOUND_UP, NDIM), #Upper Bounds
                  m          = NOBJ) #Number of objectives

## MOEA/D
out <- moead(problem = problem,
             decomp = decomp, aggfun = aggfun, neighbors = neighbors, variation = variation,
             update = update, constraint = constraint, scaling = scaling, stopcrit = stopcrit,
             showpars = showpars, seed = seed)

# Plot output:
plot(out$Y[,1], out$Y[,2], type = "p", pch = 20)
write.csv(out$Y, file = "moeadr_fitness_moead_zdt6_100_500_2_10.csv", row.names = FALSE)




# ----------------Many objective Config--------------
NGEN      <- 500
CXPB      <- 0.8
MUTPB     <- 0.2
BOUND_LOW <- 0
BOUND_UP  <- 1


# -------DTLZ-1-----------
NOBJ      <- 3
NDIM      <- 4
MU        <- 92
P <- 12

DTLZ1 <- make_vectorized_smoof(prob.name  = "DTLZ1",
                               n.objectives = NOBJ,
                               dimensions = NDIM) #Number of decision variables

## 2: set input parameters
problem   <- list(name       = "DTLZ1", # Name of the problem list
                  xmin       = rep(BOUND_LOW, NDIM), #Lower Bounds
                  xmax       = rep(BOUND_UP, NDIM), #Upper Bounds
                  m          = NOBJ) #Number of objectives

decomp    <- list(name       = "SLD", H = P) # Descomposition approach: Using Simplex-lattice design

neighbors <- list(name       = "lambda",
                  T          = 20,
                  delta.p    = 1)

aggfun    <- list(name       = "wt")

variation <- list(list(name  = "sbx", #SBX crossover
                       etax  = 20,
                       pc = CXPB), #Crossover probability: 80%
                  list(name  = "polymut", #Polynomial Mutation
                       etam  = 20,
                       pm = MUTPB),
                  list(name  = "truncate"))

update    <- list(name       = "standard",
                  UseArchive = TRUE)

scaling   <- list(name       = "none")
constraint<- list(name       = "none")
stopcrit  <- list(list(name  = "maxiter",
                       maxiter  = NGEN))
showpars  <- list(show.iters = "dots",
                  showevery  = 10)
seed      <- 1

## MOEA/D
out <- moead(problem = problem,
             decomp = decomp, aggfun = aggfun, neighbors = neighbors, variation = variation,
             update = update, constraint = constraint, scaling = scaling, stopcrit = stopcrit,
             showpars = showpars, seed = seed)

# Plot output:
scatterplot3d(out$Y[,1], out$Y[,2], out$Y[,3], color = "blue", pch = 16, main = "DTLZ 1")
write.csv(out$Y, file = "moeadr_fitness_moead_dtlz1_92_500_3_4.csv", row.names = FALSE)



# ----------DTLZ 2----------
NOBJ      <- 4
NDIM      <- 5
MU        <- 120
P <- 7

DTLZ2 <- make_vectorized_smoof(prob.name = "DTLZ2",
                               dimensions = NDIM,
                               n.objectives = NOBJ)

problem.dtlz2 <- list(name = "DTLZ2",
                      xmin = rep(BOUND_LOW, NDIM),
                      xmax = rep(BOUND_UP, NDIM),
                      m = NOBJ)

results.dtlz.2 <- moead(problem = problem.dtlz2,
                      preset = preset_moead("original"),
                      decomp = list(name = "SLD", H = P), stopcrit = stopcrit, seed = 42)

write.csv(results.dtlz.2$Y, file = "moeadr_fitness_moead_dtlz2_120_500_4_5.csv", row.names = FALSE)


# ----------DTLZ 3----------
NOBJ      <- 6
NDIM      <- 6
MU        <- 252
P <- 5

DTLZ3 <- make_vectorized_smoof(prob.name = "DTLZ3",
                               dimensions = NDIM,
                               n.objectives = NOBJ)

problem.dtlz3 <- list(name = "DTLZ3",
                      xmin = rep(BOUND_LOW, NDIM),
                      xmax = rep(BOUND_UP, NDIM),
                      m = NOBJ)

results.dtlz.3 <- moead(problem = problem.dtlz3,
                        preset = preset_moead("original"),
                        decomp = list(name = "SLD", H = P), stopcrit = stopcrit, seed = 42)


write.csv(results.dtlz.3$Y, file = "moeadr_fitness_moead_dtlz3_252_500_6_6.csv", row.names = FALSE)



# ----------DTLZ 3----------
NOBJ      <- 3
NDIM      <- 7
MU        <- 92
P <- 12

DTLZ7 <- make_vectorized_smoof(prob.name = "DTLZ7",
                               dimensions = NDIM,
                               n.objectives = NOBJ)

problem.dtlz7 <- list(name = "DTLZ7",
                      xmin = rep(BOUND_LOW, NDIM),
                      xmax = rep(BOUND_UP, NDIM),
                      m = NOBJ)

results.dtlz.7 <- moead(problem = problem.dtlz7,
                        preset = preset_moead("original"),
                        decomp = list(name = "SLD", H = P), stopcrit = stopcrit, seed = 42)


write.csv(results.dtlz.7$Y, file = "moeadr_fitness_moead_dtlz3_252_500_3_7.csv", row.names = FALSE)









zdt1_pareto <- function(n_pareto_points=100) {
  return(cbind(seq(0, 1, length.out=n_pareto_points),
               1 - sqrt(seq(0, 1, length.out=n_pareto_points))))
}

zdt2_pareto <- function(n_pareto_points=100) {
  return(cbind(seq(0, 1, length.out=n_pareto_points), 1 - (seq(0, 1, length.out=n_pareto_points))^2))
}

zdt3_pareto <- function(n_points=100, flatten=TRUE) {
  regions <- list(c(0, 0.0830015349),
                  c(0.182228780, 0.2577623634),
                  c(0.4093136748, 0.4538821041),
                  c(0.6183967944, 0.6525117038),
                  c(0.8233317983, 0.8518328654))

  pf <- list()

  for (r in regions) {
    x1 <- seq(r[1], r[2], length.out=n_points / length(regions))
    x2 <- 1 - sqrt(x1) - x1 * sin(10 * pi * x1)
    pf[[length(pf)+1]] <- cbind(x1, x2)
  }

  if (flatten) {
    pf <- do.call(rbind, pf)
  }

  return(pf)
}

zdt4_pareto <- function(n_pareto_points=100) {
  return(cbind(seq(0, 1, length.out=n_pareto_points),
               1 - sqrt(seq(0, 1, length.out=n_pareto_points))))
}

zdt5_pareto <- function(n_pareto_points=100, m = 11) {
  x <- 1 + seq(0, 1, (1 - 0)/n_pareto_points) * 30
  pf <- cbind(x, (m - 1)/x)
  normalize <- function(x) {
    x_min <- min(x)
    x_max <- max(x)
    denom <- x_max - x_min
    denom <- denom + 1e-30
    N <- (x - x_min) / denom
    return(N)
  }
  pf <- normalize(x = pf)
  return(pf)
}

zdt6_pareto <- function(n_pareto_points=100) {
  x <- seq(0.2807753191, 1, length.out=n_pareto_points)
  return(cbind(x, 1 - x^2))
}





library(ggplot2)

A <- read.csv("C:/Users/Maria/Downloads/Tesis/Simulaciones/Multi-objective/deap_nsga2_zdt6.csv")
B <- read.csv("C:/Users/Maria/Downloads/Tesis/Simulaciones/Multi-objective/moeadr_moead_zdt6.csv")
C <- read.csv("C:/Users/Maria/Downloads/Tesis/Simulaciones/Multi-objective/pymoo_nsga2_zdt6.csv")
D <- as.data.frame(res@fitness)
colnames(D) <- c("f1","f2")

A$framework <- "deap"
B$framework <- "moeadr"
C$framework <- "pymoo"
D$framework <- "rmoo"

# Combine all datasets into one
all_data <- rbind(A, B, C, D)

# Create the scatter plot
ggplot(all_data, aes(x=f1, y=f2, color=framework, shape=framework)) +
  geom_point() +
  scale_shape_manual(values=c(16, 17, 18, 19)) +  # Set the shapes manually
  theme_minimal() +
  labs(title="ZDT 6", x="X", y="Y", color="Framework", shape="Framework")


write.csv(res@fitness, file = "rmoo_fitness_nsga2_zdt4_100_500_2_7.csv", row.names = FALSE)





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



dtlz7_pareto <- read.csv("C:/Users/Maria/Downloads/Tesis/Simulaciones/DTLZ7-3-PF.csv", header = FALSE)
# read.csv("C:/Users/Maria/Downloads/Tesis/Simulaciones/DTLZ7.3D.pf.csv", header = FALSE)



calculate_metrics <- function(libraries, problems){
  pattern <- "%s_nsga3_%s.csv"
  moeadr_pattern <- "%s_moead_%s.csv"
  pareto_pattern <- "%s_pareto"

  for (i in problems) {
    pareto_points <- get(sprintf(pareto_pattern, i))

    for (j in libraries) {
      if(i == "dtlz1"){
        reference_point <- rep(1.5, nObj)
      } else if(i == "dtlz2" || i == "dtlz3"){
        reference_point <- rep(1.5, nObj)
      } else if(i == "dtlz7"){
        reference_point <- rep(1, 1, 10)
      }

      if(j == "moeadr"){
        dataset_name <- sprintf(moeadr_pattern, j, i)
      } else{
        dataset_name <- sprintf(pattern, j, i)
      }

      content <- read.csv(dataset_name)

      if (i == "dtlz7") {
        metrics <- cbind(
          emoa::dominated_hypervolume(points = t(content), ref = reference_point),
          ecr::computeGenerationalDistance(t(content), t(pareto_points)),
          eaf::igd_plus(data = content, reference = pareto_points)
        )
      } else{
        metrics <- cbind(
          emoa::dominated_hypervolume(points = t(content), ref = reference_point),
          ecr::computeGenerationalDistance(t(content), t(pareto_points())),
          eaf::igd_plus(data = content, reference = pareto_points())
        )
      }



      write.csv(metrics, paste0(j,"_nsga2_metrics_",i,".csv"), row.names = FALSE)
    }
  }
}
libraries <- c("deap", "moeadr", "pymoo", "rmoo")
problems <- c("dtlz1", "dtlz2","dtlz3","dtlz7")
calculate_metrics(libraries, problems, reference_point)




A <- read.csv("C:/Users/Maria/Downloads/Tesis/Simulaciones/Many Objective/deap_nsga3_dtlz2.csv")
A <- as.matrix(A)
B <- read.csv("C:/Users/Maria/Downloads/Tesis/Simulaciones/Many Objective/moeadr_moead_dtlz2.csv")
B <- as.matrix(B)
C <- read.csv("C:/Users/Maria/Downloads/Tesis/Simulaciones/Many Objective/pymoo_nsga3_dtlz2.csv")
C <- as.matrix(C)
# D <- read.csv("C:/Users/Maria/Downloads/Tesis/Simulaciones/Many Objective/rmoo_nsga3_dtlz2.csv")


# normalize <- function(x) {
#   return ((x - min(x)) / (max(x) - min(x)))
# }


A <- apply(A, 2, normalize)
B <- apply(B, 2, normalize)
C <- apply(C, 2, normalize)
D <- apply(res@fitness, 2, normalize)

A$group <- "deap"
B$group <- "moeadr"
C$group <- "pymoo"
D$group <- "rmoo"
all_data <- rbind(A, B, C, D)


library(plotly)
plot_ly(all_data, x = ~f1, y = ~f2, z = ~f3, color = ~group, symbol = ~group, mode = "markers") %>%
  layout(scene = list(xaxis = list(title = 'f1'),
                      yaxis = list(title = 'f2'),
                      zaxis = list(title = 'f3')))




cbind(
  emoa::dominated_hypervolume(points = t(A), ref = c(1,1,16)),
  emoa::dominated_hypervolume(points = t(B), ref = c(1,1,16)),
  emoa::dominated_hypervolume(points = t(C), ref = c(1,1,16)),
  emoa::dominated_hypervolume(points = t(D), ref = c(1,1,16))
)

cbind(
  ecr::computeGenerationalDistance(t(A), t(dtlz7_pareto)),
  ecr::computeGenerationalDistance(t(B), t(dtlz7_pareto)),
  ecr::computeGenerationalDistance(t(C), t(dtlz7_pareto)),
  ecr::computeGenerationalDistance(t(D), t(dtlz7_pareto))
)

cbind(
  eaf::igd_plus(data = A, reference = dtlz7_pareto),
  eaf::igd_plus(data = B, reference = dtlz7_pareto),
  eaf::igd_plus(data = C, reference = dtlz7_pareto),
  eaf::igd_plus(data = D, reference = dtlz7_pareto)
)






# Install and load the profvis package
library(profvis)
profvis::profvis({
  res <- rmoo::nsga3(type = "real-valued",
                     fitness = dtlz1,
                     lower = rep(BOUND_LOW,NDIM),
                     upper = rep(BOUND_UP,NDIM),
                     popSize = MU,
                     maxiter = NGEN,
                     nObj = NOBJ,
                     pcrossover = CXPB,
                     pmutation = MUTPB,
                     reference_dirs = ref_dirs,
                     monitor = FALSE,
                     summary = FALSE,
                     parallel = FALSE,
                     seed = 1)
})


# Start profiling
Rprof(filename = "dtlz1-execution.out", memory.profiling = TRUE)
res <- rmoo::nsga3(type = "real-valued",
                   fitness = dtlz1,
                   lower = rep(BOUND_LOW,NDIM),
                   upper = rep(BOUND_UP,NDIM),
                   popSize = MU,
                   maxiter = NGEN,
                   nObj = NOBJ,
                   pcrossover = CXPB,
                   pmutation = MUTPB,
                   reference_dirs = ref_dirs,
                   monitor = FALSE,
                   summary = FALSE,
                   parallel = FALSE,
                   seed = 1)
Rprof(NULL)
summaryRprof("dtlz1-execution.out")










calculateDistArray <- function(data) {
  data <- as.matrix(dist(data, method = "euclidean", diag = TRUE, upper = TRUE))
  #data[upper.tri(data, diag = TRUE)] <- 0
  return(data)
}

kroA100dist <- calculateDistArray(rmoo::kroA100)
kroB100dist <- calculateDistArray(rmoo::kroB100)
kroC100dist <- calculateDistArray(rmoo::kroC100)

distArray <- array(NA_real_, dim=c(100, 100, 3))

distArray[,,1] <- kroA100dist
distArray[,,2] <- kroB100dist
distArray[,,3] <- kroC100dist

tourLength <- function(tour, distArray) {
  route <- c(tour, tour[1])
  route <- embed(route, 2)[,2:1]
  sums <- apply(distArray, 3, function(distMat) sum(distMat[route]))
  return(sums)
}


motspFitness <- function(tour, distArray) 100000/tourLength(tour, distArray)

# tour <- sample(100, replace = FALSE)
# tourLength(tour, distArray)


NDIM <- 100
NOBJ <- 3
P <- 12

BOUND_LOW <- 1
BOUND_UP <- 100
NGEN = 5000
CXPB = 0.8
MUTPB = 0.2

ref_dirs <- rmoo::generate_reference_points(NOBJ, P)
MU <- nrow(ref_dirs)

res <- rmoo::rmoo(type = "permutation",
                  algorithm = "NSGA-III",
                  fitness = motspFitness,
                  lower = BOUND_LOW,
                  upper = BOUND_UP,
                  popSize = MU,
                  maxiter = NGEN,
                  nObj = NOBJ,
                  pcrossover = CXPB,
                  pmutation = MUTPB,
                  reference_dirs = ref_dirs,
                  monitor = measure_time,
                  summary = FALSE,
                  parallel = FALSE,
                  seed = 1,
                  distArray = distArray)

write.csv(res@population, file = "rmoo_pop_nsga3_motsp_91_500_3_100.csv", row.names = FALSE)
write.csv(res@fitness, file = "rmoo_fitness_nsga3_motsp_91_500_3_100.csv", row.names = FALSE)



start_time <- Sys.time()
res <- rmoo::rmoo(type = "permutation",
                  algorithm = "NSGA-II",
                  fitness = motspFitness,
                  lower = BOUND_LOW,
                  upper = BOUND_UP,
                  popSize = MU,
                  maxiter = NGEN,
                  nObj = NOBJ,
                  pcrossover = CXPB,
                  pmutation = MUTPB,
                  monitor = measure_time,
                  summary = FALSE,
                  parallel = FALSE,
                  seed = 1,
                  distArray = distArray)

write.csv(res@population, file = "rmoo_pop_nsga2_motsp_91_500_3_100.csv", row.names = FALSE)
write.csv(res@fitness, file = "rmoo_fitness_nsga2_motsp_91_500_3_100.csv", row.names = FALSE)



A <- read.csv("C:/Users/Maria/Downloads/Tesis/Simulaciones/Many Objective/deap_fitness_nsga3_motsp_91_500_3_100.csv")
A <- as.matrix(A)
B <- read.csv("C:/Users/Maria/Downloads/Tesis/Simulaciones/Many Objective/pymoo_fitness_nsga3_motsp_91_500_3_100.csv")
B <- as.matrix(B)



pcp_plot <- function(fitness) {
  fitness <- as.data.frame(fitness)
  nObj <- ncol(fitness)
  colnames(fitness) <- sprintf("f_%s",seq(nObj))
  fitness$color <- grDevices::rainbow(nrow(fitness))
  fitness <- reshape2::melt(fitness)
  fitness <- dplyr::rename(fitness,
                           'Objective_Value' = value,
                           'Objective_No' = variable)
  ggplot2::ggplot(fitness, aes(x = Objective_No,
                               y = Objective_Value,
                               group = color, colour=factor(color))) +
    ggplot2::geom_line(show.legend = FALSE) +
    ggplot2::theme_classic()
}

scatter_plotting <- function(fitness) {
  objective_values <- fitness
  colnames(objective_values) <- c("f_1", "f_2", "f_3")
  objective_values <- as.data.frame(objective_values)

  fig <- plotly::plot_ly(objective_values, x = ~f_1,
                         y = ~f_2,
                         z = ~f_3,
                         colors = '#0C4B8E', size = 1)
  fig <- fig %>% plotly::add_markers() #New plotly add
  fig <- fig %>% plotly::layout(scene = list(xaxis = list(title = 'f_1'),
                                             yaxis = list(title = 'f_2'),
                                             zaxis = list(title = 'f_3')))
  fig
}


cbind(emoa::dominated_hypervolume(points = t(A), ref = c(1,1,1)),
  ecr::computeGenerationalDistance(t(A), t(generate_reference_points(3,12))),
  eaf::igd_plus(data = A, reference = generate_reference_points(3,12)))













zdt1 <- function(x, pause = 2) {
  Sys.sleep(pause)
  n = length(x)
  f1 = x[1]
  g = 1 + 9 * sum(x[2:n])/(n - 1)
  h = 1 - sqrt(f1/g)
  f2 = g * h
  return(c(f1, f2))
}

NDIM <- 2
ncores <- 4

con <- file("rmoo_time_nsga2_zdt1_50_100_2_2_parallel.csv", open = "w")
start_time <- Sys.time()
res <- rmoo::rmoo(type = "real-valued",
                  algorithm = "NSGA-II",
                  fitness = zdt1,
                  lower = rep(0,2),
                  upper = rep(1,2),
                  popSize = 50,
                  maxiter = 100,
                  nObj = 2,
                  pcrossover = CXPB,
                  pmutation = MUTPB,
                  monitor = measure_time,
                  summary = FALSE,
                  parallel = ncores,
                  seed = 1)
close(con)

# write.csv(res@fitness, file = "rmoo_fitness_nsga2_zdt1_100_500_2_3.csv", row.names = FALSE)





#El argumento object recibirá la instancia del algoritmo ejecutado, la lista de atributo que se guardarán en esta instancia en cada ejecución se encuentran en la Figura 3.1.
#El argumento ... recibe cual otro parametro adicional que se pase a la función rmoo y no este definida explicitamente, en este ejemplo lo utilizaremos para pasar el punto de referencia con el que medir el hipervolumen
monitor_measure <- function(object, ...) {
  #Del argumento ... verificaremos si se pasaron los argumentos correspondiente para ser evaluados durante la ejecución
  callArgs <- as.list(...)
  nullST <- is.null(callArgs$startTime)
  nullRP <- is.null(callArgs$ref_points)
  first_front <- object@fitness[object@f[[1]], ]

  # Validamos que se haya pasado un punto de referencia con el cual medir el hipervolumen, caso contrario se imprime una excepción y se detienen la ejecuación
  if (!nullRP) {
    hv_value <- emoa::dominated_hypervolume(points = t(first_front), ref = callArgs$ref_points)
  } else{
    stop("Favor definir el punto de referencia (ref_points) para poder calcular el HV")
  }

  # Del mismo modo, validamos que se haya pasado como argumento el tiempo con el que se inició la ejecución, esto se hace pasando la función Sys.time() como parametro
  if (!nullST) {
    elapsedTime <- as.numeric(difftime(Sys.time() , callArgs$startTime, units = "secs"))
  } else{
    stop("Favor definir al argumento startTime la función Sys.time()")
  }

  # Si es la primera generación deberá imprimir un titulo mostrando los valores a evaluar en cada generación
  if (object@iter == 1) {
    cat("|   Iter   |  HV  |  N° PF  |  Time  |\n")
  }
  # Formateamos los resultados para que todos tengan la misma cantidad de digitos
  cat("|  ", formatC(object@iter, format = "f", digits = 0), "  |  ", formatC(hv_value, format = "f", digits = 2), "  |  ", formatC(nrow(first_front), format = "f", digits = 0), "  |  ", formatC(elapsedTime, format = "f", digits = 2), "  |\n")
}

NOBJ <- 6

res <- rmoo::rmoo(type = "real-valued",
                  algorithm = "NSGA-III",
                  fitness = dtlz3,
                  lower = rep(BOUND_LOW,NDIM),
                  upper = rep(BOUND_UP,NDIM),
                  popSize = 100,
                  maxiter = 500,
                  nObj = NOBJ,
                  pcrossover = 0.8,
                  pmutation = 0.2,
                  reference_dirs = ref_dirs,
                  monitor = monitor_measure,
                  summary = FALSE,
                  parallel = FALSE,
                  seed = 1,
                  ref_points = rep(1.5,NOBJ),
                  startTime = Sys.time())







# Parelelización



library(rmoo)

measure_time <- function(object, ...) {
  elapsed_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  print(paste("Iteration: ", object@iter, "\n"))
  write.table(data.frame("Iteration" = object@iter,
                         "Elapsed Time" = elapsed_time),
              con,
              sep = ",",
              col.names = FALSE,
              row.names = FALSE)
}


zdt1 <- function(x, pause = 2) {
  Sys.sleep(pause)
  n = length(x)
  f1 = x[1]
  g = 1 + 9 * sum(x[2:n])/(n - 1)
  h = 1 - sqrt(f1/g)
  f2 = g * h
  return(c(f1, f2))
}

ncores <- c(1, 2, 3, 4) # number of cores/processors
pause <- c(0.01, 0.1, 1, 2) # pause during fitness evaluation
nrep <- 10 # number of simulation replications
# seed_number <- sample.int(100,1)



NOBJ <- 2
MU <- 100
BOUND_LOW <- 0
BOUND_UP <- 1
NDIM <- 2
NGEN <- 100
CXPB <- 0.8
MUTPB <- 0.2
ncore <- c(2) # number of cores/processors
pause <- c(1) # pause during fitness evaluation
seed_number <- 55

con <- file(paste0("rmoo_time_par_nsga2_zdt1_",ncore,"_",pause,"_",seed_number,".csv"), open = "w")
start_time <- Sys.time()
res_par <- rmoo::rmoo(type = "real-valued",
                      algorithm = "NSGA-II",
                      fitness = zdt1,
                      lower = rep(BOUND_LOW,NDIM),
                      upper = rep(BOUND_UP,NDIM),
                      popSize = MU,
                      maxiter = NGEN,
                      nObj = NOBJ,
                      pcrossover = CXPB,
                      pmutation = MUTPB,
                      monitor = measure_time,
                      summary = FALSE,
                      parallel = ncore,
                      seed = seed_number)
close(con)


ggplot(all_parallel_simulation, aes(x=Generation, y=Time, color=cores, shape=cores)) +
  geom_point() +
  facet_wrap(~pause) +
  theme_minimal()

all_parallel_simulation_001 <- subset(all_parallel_simulation, pause %in% c("0.01"))
all_parallel_simulation_01 <- subset(all_parallel_simulation, pause %in% c("0.1"))
all_parallel_simulation_1 <- subset(all_parallel_simulation, pause %in% c("1"))
all_parallel_simulation_2 <- subset(all_parallel_simulation, pause %in% c("2"))

all_parallel_simulation_sub <- subset(all_parallel_simulation, Generation %in% c(1, 10, 20,30,40,50,60,70,80,90,100))

ggplot(all_parallel_simulation_001, aes(x=Generation, y=Time, color=cores, shape=cores)) +
  geom_point() +
  facet_wrap(~pause) +
  theme_minimal()

all_parallel_simulation_2 <- subset(all_parallel_simulation, pause %in% c("2"))

ggplot(all_parallel_simulation_2, aes(x=Generation, y=Time, color=cores, shape=cores)) +
  geom_line() +
  facet_wrap(~pause)


all_parallel_simulation_time <- subset(all_parallel_simulation, Generation %in% c("100"))

ggplot(all_parallel_simulation_time, aes(x=cores, y=Time, color=`Pausa (Seg.)`, group=`Pausa (Seg.)`, shape=`Pausa (Seg.)`)) +
  geom_point() +
  geom_line() +
  labs(title="Tiempo de Ejecución para un N° de Núcleos", x="N° de Núcleos", y="Tiempo de Ejecución (Seg.)") +
  scale_shape_manual(values=1:4)



ggplot(results_df, aes(x=f1, y=f2, color=framework, shape=framework)) +
  geom_point() +
  scale_shape_manual(values=(seq_along(unique(results_df$framework)) + 15)) +
  theme_minimal() +
  labs(title=toupper(problem), x="f1", y="f2", color="Framework", shape="Framework")




df <- data.frame(x = 1:4, y = 1:4, z = c("a", "a", "b", "b"))
base_f <- ggplot(df, aes(x, y)) + geom_point() + facet_wrap(~z)

base_f
base_f + theme(panel.spacing = unit(0.5, "in"))
base_f + theme(
  strip.text = element_text(colour = "white"),
  strip.background = element_rect(
    fill = "grey20",
    color = "grey80",
    linewidth = 1
  )
)




#---------------------------ZDT----------------------------
library(rmoo)
library(tidyverse)

rmoobin_fbMutation <- function(object, parent, indpb=0.2) {
  mutate <- parent <- as.vector(object@population[parent, ])

  mutate <- as.logical(mutate)
  for (i in seq_along(parent)) {
    if (runif(1) < indpb) {
      mutate[i] <- !mutate[i]
    }
  }
  mutate <- as.numeric(mutate)
  storage.mode(mutate) <- "integer"
  return(mutate)
}


pointCrossover <- function(object, parents, n_points=2) {
  parents <- object@population[parents, ]
  n_matings <- nrow(parents)
  n <- ncol(parents)

  fitnessChildren <- matrix(NA_real_, ncol = ncol(object@fitness))

  r <- t(replicate(n_matings, sample(n - 1)))
  r <- r[, 1:n_points]
  r <- apply(r, 1, sort)
  r <- cbind(r, rep(n, n_matings))

  M <- matrix(FALSE, nrow = n_matings, ncol = n)

  for (i in seq_len(n_matings)) {
    j <- 1
    while (j < (ncol(r) - 1)) {
      a <- r[i, j]
      b <- r[i, j + 1]
      M[i, a:b] <- TRUE
      j <- j + 1
    }
  }

  children <- crossover_mask(parents, M)

  storage.mode(children) <- "integer"
  out <- list(children = children,
              fitness = fitnessChildren)
  return(out)
}


crossover_mask <- function(X, M) {
  parent <- X
  parent[1,][M[2,]] <- X[2,][M[2,]]
  parent[2,][M[1,]] <- X[1,][M[1,]]
  return(parent)
}

zdt1_pareto <- function(n_pareto_points=100) {
  return(cbind(seq(0, 1, length.out=n_pareto_points),
               1 - sqrt(seq(0, 1, length.out=n_pareto_points))))
}

zdt2_pareto <- function(n_pareto_points=100) {
  return(cbind(seq(0, 1, length.out=n_pareto_points), 1 - (seq(0, 1, length.out=n_pareto_points))^2))
}

zdt3_pareto <- function(n_points=100, flatten=TRUE) {
  regions <- list(c(0, 0.0830015349),
                  c(0.182228780, 0.2577623634),
                  c(0.4093136748, 0.4538821041),
                  c(0.6183967944, 0.6525117038),
                  c(0.8233317983, 0.8518328654))

  pf <- list()

  for (r in regions) {
    x1 <- seq(r[1], r[2], length.out=n_points / length(regions))
    x2 <- 1 - sqrt(x1) - x1 * sin(10 * pi * x1)
    pf[[length(pf)+1]] <- cbind(x1, x2)
  }

  if (flatten) {
    pf <- do.call(rbind, pf)
  }

  return(pf)
}

zdt4_pareto <- function(n_pareto_points=100) {
  return(cbind(seq(0, 1, length.out=n_pareto_points),
               1 - sqrt(seq(0, 1, length.out=n_pareto_points))))
}

zdt5_pareto <- function(n_pareto_points=100, m = 11) {
  x <- 1 + seq(0, 1, (1 - 0)/n_pareto_points) * 30
  pf <- cbind(x, (m - 1)/x)
  normalize <- function(x) {
    x_min <- min(x)
    x_max <- max(x)
    denom <- x_max - x_min
    denom <- denom + 1e-30
    N <- (x - x_min) / denom
    return(N)
  }
  pf <- normalize(x = pf)
  return(pf)
}

zdt6_pareto <- function(n_pareto_points=100) {
  x <- seq(0.2807753191, 1, length.out=n_pareto_points)
  return(cbind(x, 1 - x^2))
}

n_iterations <- 10

for (i in seq_len(n_iterations)) {

  NOBJ <- 2
  MU <- 100
  BOUND_LOW <- 0
  BOUND_UP <- 1
  NGEN = 500
  CXPB = 0.8
  MUTPB = 0.2
  ref_points <- generate_reference_points(2,99)


  zdt1 <- function(x) {
    n = length(x)
    f1 = x[1]
    g = 1 + 9 * sum(x[2:n])/(n - 1)
    h = 1 - sqrt(f1/g)
    f2 = g * h
    return(c(f1, f2))
  }


  NDIM <- 3
  res <- rmoo::rmoo(type = "real-valued",
                    algorithm = "NSGA-III",
                    fitness = zdt1,
                    lower = rep(BOUND_LOW,NDIM),
                    upper = rep(BOUND_UP,NDIM),
                    popSize = MU,
                    maxiter = NGEN,
                    nObj = NOBJ,
                    pcrossover = CXPB,
                    pmutation = MUTPB,
                    reference_dirs = ref_points,
                    monitor = FALSE,
                    summary = FALSE,
                    parallel = FALSE,
                    seed = i)

  write.csv(res1@fitness, file = paste0("rmoo_fitness_nsga3_zdt1_100_500_2_3-", i, ".csv"), row.names = FALSE)

  res1 <- rmoo::rmoo(type = "real-valued",
                     algorithm = "NSGA-II",
                     fitness = zdt1,
                     lower = rep(BOUND_LOW,NDIM),
                     upper = rep(BOUND_UP,NDIM),
                     popSize = MU,
                     maxiter = NGEN,
                     nObj = NOBJ,
                     pcrossover = CXPB,
                     pmutation = MUTPB,
                     monitor = FALSE,
                     summary = FALSE,
                     parallel = FALSE,
                     seed = i)

  write.csv(res1@fitness, file = paste0("rmoo_fitness_nsga2_zdt1_100_500_2_3-", i, ".csv"), row.names = FALSE)


  zdt2 <- function(x) {
    n = length(x)
    f1 = x[1]
    g = 1 + 9 * sum(x[2:n])/(n - 1)
    h = 1 - (f1/g)^2
    f2 = g * h
    return(c(f1, f2))
  }

  NDIM <- 4

  res <- rmoo::rmoo(type = "real-valued",
                    algorithm = "NSGA-III",
                    fitness = zdt2,
                    lower = rep(BOUND_LOW,NDIM),
                    upper = rep(BOUND_UP,NDIM),
                    popSize = MU,
                    maxiter = NGEN,
                    nObj = NOBJ,
                    pcrossover = CXPB,
                    pmutation = MUTPB,
                    reference_dirs = ref_points,
                    monitor = FALSE,
                    summary = FALSE,
                    parallel = FALSE,
                    seed = i)

  write.csv(res@fitness, file = paste0("rmoo_fitness_nsga3_zdt2_100_500_2_4-", i, ".csv"), row.names = FALSE)


  res1 <- rmoo::rmoo(type = "real-valued",
                     algorithm = "NSGA-II",
                     fitness = zdt2,
                     lower = rep(BOUND_LOW,NDIM),
                     upper = rep(BOUND_UP,NDIM),
                     popSize = MU,
                     maxiter = NGEN,
                     nObj = NOBJ,
                     pcrossover = CXPB,
                     pmutation = MUTPB,
                     monitor = FALSE,
                     summary = FALSE,
                     parallel = FALSE,
                     seed = i)
  write.csv(res1@fitness, file = paste0("rmoo_fitness_nsga2_zdt2_100_500_2_4-", i, ".csv"), row.names = FALSE)


  zdt3 <- function(x) {
    n = length(x)
    f1 = x[1]
    g = 1 + 9 * sum(x[2:n])/(n - 1)
    h = 1 - sqrt(f1/g) - (f1/g) * sin(10 * pi * f1)
    f2 = g * h
    return(c(f1, f2))
  }


  NDIM <- 5
  res <- rmoo::rmoo(type = "real-valued",
                    algorithm = "NSGA-II",
                    fitness = zdt3,
                    lower = rep(BOUND_LOW,NDIM),
                    upper = rep(BOUND_UP,NDIM),
                    popSize = MU,
                    maxiter = NGEN,
                    nObj = NOBJ,
                    pcrossover = CXPB,
                    pmutation = MUTPB,
                    monitor = FALSE,
                    summary = FALSE,
                    parallel = FALSE,
                    seed = i)
  write.csv(res@fitness, file = paste0("rmoo_fitness_nsga2_zdt3_100_500_2_5-", i, ".csv"), row.names = FALSE)

  res1 <- rmoo::rmoo(type = "real-valued",
                     algorithm = "NSGA-III",
                     fitness = zdt3,
                     lower = rep(BOUND_LOW,NDIM),
                     upper = rep(BOUND_UP,NDIM),
                     popSize = MU,
                     maxiter = NGEN,
                     nObj = NOBJ,
                     pcrossover = CXPB,
                     pmutation = MUTPB,
                     reference_dirs = ref_points,
                     monitor = FALSE,
                     summary = FALSE,
                     parallel = FALSE,
                     seed = i)
  write.csv(res1@fitness, file = paste0("rmoo_fitness_nsga3_zdt3_100_500_2_5-", i, ".csv"), row.names = FALSE)

  zdt4 <- function(x) {
    n = length(x)
    f1 = x[1]
    g = 1 + 10 * (n - 1) + sum(x[2:n]^2 - 10 * cos(4 * pi *
                                                     x[2:n]))
    h = 1 - sqrt(f1/g)
    f2 = g * h
    return(c(f1, f2))
  }

  NDIM <- 7

  res <- rmoo::rmoo(type = "real-valued",
                    algorithm = "NSGA-II",
                    fitness = zdt4,
                    lower = rep(BOUND_LOW,NDIM),
                    upper = rep(BOUND_UP,NDIM),
                    popSize = MU,
                    maxiter = NGEN,
                    nObj = NOBJ,
                    pcrossover = CXPB,
                    pmutation = MUTPB,
                    monitor = FALSE,
                    summary = FALSE,
                    parallel = FALSE,
                    seed = i)
  write.csv(res@fitness, file = paste0("rmoo_fitness_nsga2_zdt4_100_500_2_7-", i, ".csv"), row.names = FALSE)


  res1 <- rmoo::rmoo(type = "real-valued",
                     algorithm = "NSGA-III",
                     fitness = zdt4,
                     lower = rep(BOUND_LOW,NDIM),
                     upper = rep(BOUND_UP,NDIM),
                     popSize = MU,
                     maxiter = NGEN,
                     nObj = NOBJ,
                     pcrossover = CXPB,
                     pmutation = MUTPB,
                     monitor = FALSE,
                     reference_dirs = ref_points,
                     summary = FALSE,
                     parallel = FALSE,
                     seed = i)
  write.csv(res1@fitness, file = paste0("rmoo_fitness_nsga3_zdt4_100_500_2_7-", i, ".csv"), row.names = FALSE)

  zdt5 <- function (x, m = 10, n = 5, normal = TRUE, ...) {
    if (length(x)%%5 != 0 && length(x) < 35)
    {
      stop("Bit vector's length must contain at least 35 digits.")
    }
    x1 <- x[1:30]
    xm <- x[31:length(x)]
    g <- 0
    vu <- function(v){
      if (v < 5){
        x <- 2 + v
      }else if (v == 5) {
        x <- 1
      }
      return(x)
    }

    for (i in seq(m)) {
      x <- sum(xm[((n * (i - 1)) + 1):(n * i)])
      v <- vu(x)
      g <- g + v
    }
    f1 <- 1 + sum(x1 == 1)
    f2 <- g * (1 / f1)

    normalize <- function(x, x_min, x_max) {
      # calculate the denominator
      denom <- x_max - x_min
      # we can not divide by zero -> plus small epsilon
      denom <- denom + 1e-30
      # normalize the actual values
      N <- (x - x_min) / denom
      return(N)
    }

    if (normal == TRUE){
      f1 <- normalize(f1, 1, 30)
      f2 <- normalize(f2, (m - 1) * 1 / 30, (m - 1))
    }
    return(cbind(f1,f2))
  }

  NDIM <- 80

  res <- rmoo(type = "binary",
              fitness = zdt5,
              algorithm = "NSGA-III",
              nBits = NDIM,
              crossover = pointCrossover,
              mutation = rmoobin_fbMutation,
              popSize = MU,
              maxiter = NGEN,
              nObj = NOBJ,
              pcrossover = CXPB,
              pmutation = MUTPB,
              reference_dirs = ref_points,
              monitor = FALSE,
              summary = FALSE,
              seed = i)
  write.csv(res@fitness, file = paste0("rmoo_fitness_nsga3_zdt5_100_500_2_80-", i, ".csv"), row.names = FALSE)

  res1 <- rmoo(type = "binary",
               fitness = zdt5,
               algorithm = "NSGA-II",
               nBits = NDIM,
               crossover = pointCrossover,
               mutation = rmoobin_fbMutation,
               popSize = MU,
               maxiter = NGEN,
               nObj = NOBJ,
               pcrossover = CXPB,
               pmutation = MUTPB,
               monitor = FALSE,
               summary = FALSE,
               seed = i)
  write.csv(res1@fitness, file = paste0("rmoo_fitness_nsga2_zdt5_100_500_2_80-", i, ".csv"), row.names = FALSE)

  zdt6 <- function(x) {
    n = length(x)
    f1 = 1 - exp(-4 * x[1]) * (sin(6 * pi * x[1]))^6
    g = 1 + 9 * (sum(x[2:n])/(n - 1))^(0.25)
    h = 1 - (f1/g)^2
    f2 = g * h
    return(c(f1, f2))
  }

  NDIM <- 10
  res <- rmoo::rmoo(type = "real-valued",
                    algorithm = "NSGA-II",
                    fitness = zdt6,
                    lower = rep(BOUND_LOW,NDIM),
                    upper = rep(BOUND_UP,NDIM),
                    popSize = MU,
                    maxiter = NGEN,
                    nObj = NOBJ,
                    pcrossover = CXPB,
                    pmutation = MUTPB,
                    monitor = FALSE,
                    summary = FALSE,
                    parallel = FALSE,
                    seed = i)
  write.csv(res@fitness, file = paste0("rmoo_fitness_nsga2_zdt6_100_500_2_10-", i, ".csv"), row.names = FALSE)

  res1 <- rmoo::rmoo(type = "real-valued",
                     algorithm = "NSGA-III",
                     fitness = zdt6,
                     lower = rep(BOUND_LOW,NDIM),
                     upper = rep(BOUND_UP,NDIM),
                     popSize = MU,
                     maxiter = NGEN,
                     nObj = NOBJ,
                     pcrossover = CXPB,
                     pmutation = MUTPB,
                     monitor = FALSE,
                     reference_dirs = ref_points,
                     summary = FALSE,
                     parallel = FALSE,
                     seed = i)
  write.csv(res1@fitness, file = paste0("rmoo_fitness_nsga3_zdt6_100_500_2_10-", i, ".csv"), row.names = FALSE)

  print(paste0("Iteration: ", i, "\n"))
}



#---------------------------------- DTLZ-----------------------------

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


n_iterations <- 10

for (i in seq_len(n_iterations)) {

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
                    monitor = FALSE,
                    summary = FALSE,
                    parallel = FALSE,
                    seed = i)
  write.csv(res@fitness, file = paste0("rmoo_fitness_nsga2_dtlz1_92_500_3_4-", i, ".csv"), row.names = FALSE)

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
                     monitor = FALSE,
                     summary = FALSE,
                     parallel = FALSE,
                     seed = i)
  write.csv(res1@fitness, file = paste0("rmoo_fitness_nsga3_dtlz1_92_500_3_4-", i, ".csv"), row.names = FALSE)

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
                    monitor = FALSE,
                    summary = FALSE,
                    parallel = FALSE,
                    seed = i)
  write.csv(res@fitness, file = paste0("rmoo_fitness_nsga2_dtlz2_120_500_4_5-", i, ".csv"), row.names = FALSE)

  res1 <- rmoo::rmoo(type = "real-valued",
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
                     monitor = FALSE,
                     summary = FALSE,
                     parallel = FALSE,
                     seed = i)
  write.csv(res1@fitness, file = paste0("rmoo_fitness_nsga3_dtlz2_120_500_4_5-", i, ".csv"), row.names = FALSE)



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
                    monitor = FALSE,
                    summary = FALSE,
                    parallel = FALSE,
                    seed = i)
  write.csv(res@fitness, file = paste0("rmoo_fitness_nsga2_dtlz3_252_500_6_6-", i, ".csv"), row.names = FALSE)

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
                     monitor = FALSE,
                     summary = FALSE,
                     parallel = FALSE,
                     seed = i)
  write.csv(res1@fitness, file = paste0("rmoo_fitness_nsga3_dtlz3_252_500_6_6-", i, ".csv"), row.names = FALSE)


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
                    monitor = FALSE,
                    summary = FALSE,
                    parallel = FALSE,
                    seed = i)
  write.csv(res@fitness, file = paste0("rmoo_fitness_nsga2_dtlz7_92_500_3_10-", i, ".csv"), row.names = FALSE)


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
                     monitor = FALSE,
                     summary = FALSE,
                     parallel = FALSE,
                     seed = i)
  write.csv(res1@fitness, file = paste0("rmoo_fitness_nsga3_dtlz7_92_500_3_10-", i, ".csv"), row.names = FALSE)
  print(paste0("Iteration: ", i, "\n"))
}


#----------------------------MOTSP------------------------------------
calculateDistArray <- function(data) {
  data <- as.matrix(dist(data, method = "euclidean", diag = TRUE, upper = TRUE))
  return(data)
}

kroA100dist <- calculateDistArray(rmoo::kroA100)
kroB100dist <- calculateDistArray(rmoo::kroB100)
kroC100dist <- calculateDistArray(rmoo::kroC100)

distArray <- array(NA_real_, dim=c(100, 100, 3))

distArray[,,1] <- kroA100dist
distArray[,,2] <- kroB100dist
distArray[,,3] <- kroC100dist

tourLength <- function(tour, distArray) {
  route <- c(tour, tour[1])
  route <- embed(route, 2)[,2:1]
  sums <- apply(distArray, 3, function(distMat) sum(distMat[route]))
  return(sums)
}


motspFitness <- function(tour, distArray) 100000/tourLength(tour, distArray)


NDIM <- 100
NOBJ <- 3
P <- 12

BOUND_LOW <- 1
BOUND_UP <- 100
NGEN <- 500
CXPB <- 0.8
MUTPB <- 0.2

n_iterations <- 10

for (i in seq_len(n_iterations)) {

  ref_dirs <- rmoo::generate_reference_points(NOBJ, P)
  MU <- 92

  res <- rmoo::rmoo(type = "permutation",
                    algorithm = "NSGA-III",
                    fitness = motspFitness,
                    lower = BOUND_LOW,
                    upper = BOUND_UP,
                    popSize = MU,
                    maxiter = NGEN,
                    nObj = NOBJ,
                    pcrossover = CXPB,
                    pmutation = MUTPB,
                    reference_dirs = ref_dirs,
                    monitor = FALSE,
                    summary = FALSE,
                    parallel = FALSE,
                    seed = i,
                    distArray = distArray)

  write.csv(res@fitness, file = paste0("rmoo_fitness_nsga3_motsp_92_500_3_100-", i, ".csv"), row.names = FALSE)


  res1 <- rmoo::rmoo(type = "permutation",
                     algorithm = "NSGA-II",
                     fitness = motspFitness,
                     lower = BOUND_LOW,
                     upper = BOUND_UP,
                     popSize = MU,
                     maxiter = NGEN,
                     nObj = NOBJ,
                     pcrossover = CXPB,
                     pmutation = MUTPB,
                     monitor = FALSE,
                     summary = FALSE,
                     parallel = FALSE,
                     seed = i,
                     distArray = distArray)
  write.csv(res1@fitness, file = paste0("rmoo_fitness_nsga2_motsp_92_500_3_100-", i, ".csv"), row.names = FALSE)

  print(paste0("Iteration: ", i, "\n"))
}
