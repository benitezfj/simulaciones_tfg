library(rmoo)
library(tidyverse)

measure_time <- function(object, number_objectives) {
  elapsed_time <- Sys.time() - start_time
# print(paste("Iteration:", object@iter, "Elapsed Time:", elapsed_time))

  write.table(data.frame("object" = object@iter, "Elapsed Time" = elapsed_time), con, sep = ",", col.names = FALSE, row.names = FALSE)
}

n_iterations <- 10

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
con <- file("rmoo_time_nsga3_zdt1_100_500_2_3.csv", open = "w")
start_time <- Sys.time()
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
                  monitor = measure_time,
                  summary = FALSE,
                  parallel = FALSE,
                  seed = 43)
close(con)
write.csv(res@fitness, file = "rmoo_fitness_nsga3_zdt1_100_500_2_3.csv", row.names = FALSE)

plot(res)

con <- file("rmoo_time_nsga2_zdt1_100_500_2_3.csv", open = "w")
start_time <- Sys.time()
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
                   monitor = measure_time,
                   summary = FALSE,
                   parallel = FALSE,
                   seed = 50)
close(con)
write.csv(res1@fitness, file = "rmoo_fitness_nsga2_zdt1_100_500_2_3.csv", row.names = FALSE)

plot(res1)

# opt <- read.csv("C:/Users/Maria/Downloads/Tesis/Simulaciones/resultados/ZDT/deap_nsga2_zdt1.csv")


zdt2 <- function(x) {
  n = length(x)
  f1 = x[1]
  g = 1 + 9 * sum(x[2:n])/(n - 1)
  h = 1 - (f1/g)^2
  f2 = g * h
  return(c(f1, f2))
}

NDIM <- 4
con <- file("rmoo_time_nsga3_zdt2_100_500_2_4.csv", open = "w")
start_time <- Sys.time()
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
                  monitor = measure_time,
                  summary = FALSE,
                  parallel = FALSE,
                  seed = 45)
close(con)
write.csv(res@fitness, file = "rmoo_fitness_nsga3_zdt2_100_500_2_4.csv", row.names = FALSE)

plot(res)


# for (i in seq_len(n_iterations)) {
  # con <- file("rmoo_time_nsga2_zdt2_100_500_2_4.csv", open = "w")
i <- 10
time_file <- paste0("rmoo_time_nsga2_zdt2_100_500_2_4-", i, ".csv")
con <- file(time_file, open = "w")

start_time <- Sys.time()
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
                 monitor = measure_time,
                 summary = FALSE,
                 parallel = FALSE,
                 seed = i)
close(con)

# fitness_file <- paste0("rmoo_fitness_nsga2_zdt2_100_500_2_4-", i, ".csv")
# write.csv(res1@fitness, file = fitness_file, row.names = FALSE)
# cat("Iteration", i)
# }

plot(res1)



# opt <- read.csv("C:/Users/Maria/Downloads/Tesis/Simulaciones/resultados/ZDT/deap_nsga2_zdt2.csv")

zdt3 <- function(x) {
  n = length(x)
  f1 = x[1]
  g = 1 + 9 * sum(x[2:n])/(n - 1)
  h = 1 - sqrt(f1/g) - (f1/g) * sin(10 * pi * f1)
  f2 = g * h
  return(c(f1, f2))
}


NDIM <- 5

i <- 10
time_file <- paste0("rmoo_time_nsga2_zdt3_100_500_2_5-", i, ".csv")
con <- file(time_file, open = "w")
# con <- file("rmoo_time_nsga2_zdt3_100_500_2_5-.csv", open = "w")
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
                  seed = i)
close(con)
# fitness_file <- paste0("rmoo_fitness_nsga2_zdt3_100_500_2_5-", i, ".csv")
# write.csv(res1@fitness, file = fitness_file, row.names = FALSE)
# cat("Iteration", i)
# }
# write.csv(res@fitness, file = "rmoo_fitness_nsga2_zdt3_100_500_2_5.csv", row.names = FALSE)

# plot(res)

i <- 10
time_file <- paste0("rmoo_time_nsga3_zdt3_100_500_2_5-", i, ".csv")
con <- file(time_file, open = "w")
# con <- file("rmoo_time_nsga3_zdt3_100_500_2_5.csv", open = "w")
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
                   monitor = measure_time,
                   summary = FALSE,
                   parallel = FALSE,
                   seed = i)
close(con)
# fitness_file <- paste0("rmoo_fitness_nsga3_zdt3_100_500_2_5-", i, ".csv")
# write.csv(res1@fitness, file = fitness_file, row.names = FALSE)
# cat("Iteration", i)
# }
# write.csv(res1@fitness, file = "rmoo_fitness_nsga3_zdt3_100_500_2_5.csv", row.names = FALSE)

plot(res1)

# opt <- read.csv("C:/Users/Maria/Downloads/Tesis/Simulaciones/resultados/ZDT/deap_nsga2_zdt3.csv")


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

i <- 10
time_file <- paste0("rmoo_time_nsga2_zdt4_100_500_2_7-", i, ".csv")
con <- file(time_file, open = "w")
# con <- file("rmoo_time_nsga2_zdt4_100_500_2_7.csv", open = "w")
start_time <- Sys.time()
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
                  monitor = measure_time,
                  summary = FALSE,
                  parallel = FALSE,
                  seed = i)
close(con)
# fitness_file <- paste0("rmoo_fitness_nsga2_zdt4_100_500_2_7-", i, ".csv")
# write.csv(res1@fitness, file = fitness_file, row.names = FALSE)
# cat("Iteration", i)
# }
# write.csv(res@fitness, file = "rmoo_fitness_nsga2_zdt4_100_500_2_7.csv", row.names = FALSE)

plot(res)

i <- 10
time_file <- paste0("rmoo_time_nsga3_zdt4_100_500_2_7-", i, ".csv")
con <- file(time_file, open = "w")
# con <- file("rmoo_time_nsga3_zdt4_100_500_2_7.csv", open = "w")
start_time <- Sys.time()
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
                   monitor = measure_time,
                   reference_dirs = ref_points,
                   summary = FALSE,
                   parallel = FALSE,
                   seed = 54)
close(con)
# fitness_file <- paste0("rmoo_fitness_nsga3_zdt4_100_500_2_7-", i, ".csv")
# write.csv(res1@fitness, file = fitness_file, row.names = FALSE)
# cat("Iteration", i)
# }
# write.csv(res1@fitness, file = "rmoo_fitness_nsga3_zdt4_100_500_2_7.csv", row.names = FALSE)

plot(res1)

# opt <- read.csv("C:/Users/Maria/Downloads/Tesis/Simulaciones/resultados/ZDT/deap_nsga2_zdt4.csv")



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

i <- 10
time_file <- paste0("rmoo_time_nsga3_zdt5_100_500_2_80-", i, ".csv")
con <- file(time_file, open = "w")
# con <- file("rmoo_time_nsga3_zdt5_100_500_2_80.csv", open = "w")
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
            seed = i)
close(con)
# fitness_file <- paste0("rmoo_fitness_nsga3_zdt5_100_500_2_80-", i, ".csv")
# write.csv(res1@fitness, file = fitness_file, row.names = FALSE)
# cat("Iteration", i)
# }
# write.csv(res@fitness, file = "rmoo_fitness_nsga3_zdt5_100_500_2_80.csv", row.names = FALSE)

plot(res)


i <- 10
time_file <- paste0("rmoo_time_nsga2_zdt5_100_500_2_80-", i, ".csv")
con <- file(time_file, open = "w")
# con <- file("rmoo_time_nsga2_zdt5_100_500_2_80.csv", open = "w")
start_time <- Sys.time()
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
            monitor = measure_time,
            summary = FALSE,
            seed = i)
close(con)
# fitness_file <- paste0("rmoo_fitness_nsga2_zdt5_100_500_2_80-", i, ".csv")
# write.csv(res1@fitness, file = fitness_file, row.names = FALSE)
# cat("Iteration", i)
# }
# write.csv(res1@fitness, file = "rmoo_fitness_nsga2_zdt5_100_500_2_80.csv", row.names = FALSE)

plot(res1)


zdt6 <- function(x) {
  n = length(x)
  f1 = 1 - exp(-4 * x[1]) * (sin(6 * pi * x[1]))^6
  g = 1 + 9 * (sum(x[2:n])/(n - 1))^(0.25)
  h = 1 - (f1/g)^2
  f2 = g * h
  return(c(f1, f2))
}

NDIM <- 10

i <- 10
time_file <- paste0("rmoo_time_nsga2_zdt6_100_500_2_10-", i, ".csv")
con <- file(time_file, open = "w")
# con <- file("rmoo_time_nsga2_zdt6_100_500_2_10.csv", open = "w")
start_time <- Sys.time()
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
                  monitor = measure_time,
                  summary = FALSE,
                  parallel = FALSE,
                  seed = i)
close(con)
# fitness_file <- paste0("rmoo_fitness_nsga2_zdt6_100_500_2_10-", i, ".csv")
# write.csv(res1@fitness, file = fitness_file, row.names = FALSE)
# cat("Iteration", i)
# }
# write.csv(res@fitness, file = "rmoo_fitness_nsga2_zdt6_100_500_2_10.csv", row.names = FALSE)

# plot(res)

i <- 10
time_file <- paste0("rmoo_time_nsga3_zdt6_100_500_2_10-", i, ".csv")
con <- file(time_file, open = "w")
# con <- file("rmoo_time_nsga3_zdt6_100_500_2_10.csv", open = "w")
start_time <- Sys.time()
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
                   monitor = measure_time,
                   reference_dirs = ref_points,
                   summary = FALSE,
                   parallel = FALSE,
                   seed = i)
close(con)
# fitness_file <- paste0("rmoo_fitness_nsga2_zdt5_100_500_2_80-", i, ".csv")
# write.csv(res1@fitness, file = fitness_file, row.names = FALSE)
# cat("Iteration", i)
# }
# write.csv(res1@fitness, file = "rmoo_fitness_nsga3_zdt6_100_500_2_10.csv", row.names = FALSE)
# plot(res1, optimal = opt)

# opt <- read.csv("C:/Users/Maria/Downloads/Tesis/Simulaciones/resultados/ZDT/pymoo_nsga2_zdt6.csv")



ggplot(df, aes(x=f_1,y=f_2, shape=Etiqueta, color=Etiqueta)) +
  geom_point(data=df[df$Etiqueta != "Frente de Pareto",]) +
  geom_line(data=df[df$Etiqueta == "Frente de Pareto",]) +
  theme_minimal() +
  labs(title = "ZDT 2", x="f_1", y="f_2", color="Etiqueta") +
  scale_shape_manual(values = c("NSGA-III" = 16,"R-NSGA-II" = 17, "Puntos de Referencia"=4)) +
  scale_color_manual(values = c("NSGA-III" = "green", "R-NSGA-II" = "#00AFBB", "Puntos de Referencia"="red", "Frente de Pareto"="black"))
