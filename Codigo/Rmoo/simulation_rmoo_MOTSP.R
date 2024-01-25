library(rmoo)
library(tidyverse)


measure_time <- function(object, number_objectives, ...) {
  elapsed_time <- Sys.time() - start_time
  # print(paste("Iteration:", object@iter, "Elapsed Time:", elapsed_time))

  write.table(data.frame("object" = object@iter, "Elapsed Time" = elapsed_time), con, sep = ",", col.names = FALSE, row.names = FALSE)
}


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
NGEN = 500
CXPB = 0.8
MUTPB = 0.2

ref_dirs <- rmoo::generate_reference_points(NOBJ, P)
MU <- nrow(ref_dirs)

i <- 10
time_file <- paste0("rmoo_time_nsga3_motsp_92_500_3_100-", i, ".csv")
con <- file(time_file, open = "w")
# con <- file("rmoo_time_nsga3_motsp_92_500_3_100.csv", open = "w")
start_time <- Sys.time()
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
                  seed = i,
                  distArray = distArray)
close(con)
# fitness_file <- paste0("rmoo_fitness_nsga3_motsp_92_500_3_100-", i, ".csv")
# write.csv(res1@fitness, file = fitness_file, row.names = FALSE)
# write.csv(res@population, file = "rmoo_pop_nsga3_motsp_92_500_3_100.csv", row.names = FALSE)
# write.csv(res@fitness, file = "rmoo_fitness_nsga3_motsp_92_500_3_100.csv", row.names = FALSE)


i <- 10
time_file <- paste0("rmoo_time_nsga2_motsp_92_500_3_100-", i, ".csv")
con <- file(time_file, open = "w")
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
                  seed = i,
                  distArray = distArray)
close(con)
# fitness_file <- paste0("rmoo_fitness_nsga2_motsp_92_500_3_100-", i, ".csv")
# write.csv(res1@fitness, file = fitness_file, row.names = FALSE)
# write.csv(res@population, file = "rmoo_pop_nsga2_motsp_92_500_3_100.csv", row.names = FALSE)
# write.csv(res@fitness, file = "rmoo_fitness_nsga2_motsp_92_500_3_100.csv", row.names = FALSE)
