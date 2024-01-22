library(MOEADr)
library(smoof)

# ----------------MOEADr--------------

# ----------------Multi-objective Config--------------
NOBJ      <- 2
MU        <- 100
NGEN      <- 500
CXPB      <- 0.8
MUTPB     <- 0.2
BOUND_LOW <- 0
BOUND_UP  <- 1

decomp    <- list(name       = "SLD", H = 99) # Descomposition approach: Using Simplex-lattice design

neighbors <- list(name       = "lambda",
                  T          = 20, #number of neighbors in the neighborhood
                  delta.p    = 1)

aggfun    <- list(name       = "wt")

variation <- list(list(name  = "sbx", #SBX crossover
                       etax  = 20,
                       pc = CXPB), #Crossover probability: 80%
                  list(name  = "polymut", #Polynomial Mutation
                       etam  = 20,
                       pm = MUTPB), #Mutation probability: 80%
                  list(name  = "truncate"))

update    <- list(name       = "standard",
                  UseArchive = TRUE)

scaling   <- list(name       = "none")
constraint<- list(name       = "none")
stopcrit  <- list(list(name  = "maxiter",
                       maxiter  = NGEN)) #Number of Generation
showpars  <- list(show.iters = "dots",
                  showevery  = 10)
seed      <- 1


# ---------ZDT-1-------
NDIM      <- 3
ZDT1 <- make_vectorized_smoof(prob.name  = "ZDT1",
                              dimensions = NDIM) #Number of decision variables

problem   <- list(name       = "ZDT1", # Name of the problem list
                  xmin       = rep(BOUND_LOW, NDIM), #Lower Bounds
                  xmax       = rep(BOUND_UP, NDIM), #Upper Bounds
                  m          = NOBJ) #Number of objectives

## MOEA/D
# out <- moead(problem = problem,
#              decomp = decomp, aggfun = aggfun, neighbors = neighbors, variation = variation,
#              update = update, constraint = constraint, scaling = scaling, stopcrit = stopcrit,
#              showpars = showpars, seed = seed)
results.zdt.1 <- moead(problem = problem,
                        preset = preset_moead("original"),
                        decomp = list(name = "SLD", H = 99),
                        stopcrit = stopcrit, showpars = showpars, seed = seed)

# Plot output:
plot(out$Y[,1], out$Y[,2], type = "p", pch = 20)
write.csv(out$Y, file = "moeadr_fitness_moead_zdt1_100_500_2_3.csv", row.names = FALSE)


# -------ZDT-2-----------
NDIM      <- 4
ZDT2 <- make_vectorized_smoof(prob.name  = "ZDT2",
                              dimensions = NDIM) #Number of decision variables

problem   <- list(name       = "ZDT2", # Name of the problem list
                  xmin       = rep(BOUND_LOW, NDIM), #Lower Bounds
                  xmax       = rep(BOUND_UP, NDIM), #Upper Bounds
                  m          = NOBJ) #Number of objectives

## MOEA/D
# out <- moead(problem = problem,
#              decomp = decomp, aggfun = aggfun, neighbors = neighbors, variation = variation,
#              update = update, constraint = constraint, scaling = scaling, stopcrit = stopcrit,
#              showpars = showpars, seed = seed)
results.zdt.2 <- moead(problem = problem,
                       preset = preset_moead("original"),
                       decomp = list(name = "SLD", H = 99),
                       stopcrit = stopcrit, showpars = showpars, seed = seed)

# Plot output:
plot(out$Y[,1], out$Y[,2], type = "p", pch = 20)
write.csv(out$Y, file = "moeadr_fitness_moead_zdt2_100_500_2_4.csv", row.names = FALSE)


# -------ZDT-3-----------
NDIM      <- 5

ZDT3 <- make_vectorized_smoof(prob.name  = "ZDT3",
                              dimensions = NDIM) #Number of decision variables

problem   <- list(name       = "ZDT3", # Name of the problem list
                  xmin       = rep(BOUND_LOW, NDIM), #Lower Bounds
                  xmax       = rep(BOUND_UP, NDIM), #Upper Bounds
                  m          = NOBJ) #Number of objectives

## MOEA/D
# out <- moead(problem = problem,
#              decomp = decomp, aggfun = aggfun, neighbors = neighbors, variation = variation,
#              update = update, constraint = constraint, scaling = scaling, stopcrit = stopcrit,
#              showpars = showpars, seed = seed)
results.zdt.3 <- moead(problem = problem,
                       preset = preset_moead("original"),
                       decomp = list(name = "SLD", H = 99),
                       stopcrit = stopcrit, showpars = showpars, seed = seed)

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
# out <- moead(problem = problem,
#              decomp = decomp, aggfun = aggfun, neighbors = neighbors, variation = variation,
#              update = update, constraint = constraint, scaling = scaling, stopcrit = stopcrit,
#              showpars = showpars, seed = seed)
results.zdt.4 <- moead(problem = problem,
                       preset = preset_moead("original"),
                       decomp = list(name = "SLD", H = 99),
                       stopcrit = stopcrit, showpars = showpars, seed = seed)
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
# out <- moead(problem = problem,
#              decomp = decomp, aggfun = aggfun, neighbors = neighbors, variation = variation,
#              update = update, constraint = constraint, scaling = scaling, stopcrit = stopcrit,
#              showpars = showpars, seed = seed)
results.zdt.6 <- moead(problem = problem,
                       preset = preset_moead("original"),
                       decomp = list(name = "SLD", H = 99),
                       stopcrit = stopcrit, showpars = showpars, seed = seed)
# Plot output:
plot(out$Y[,1], out$Y[,2], type = "p", pch = 20)
write.csv(out$Y, file = "moeadr_fitness_moead_zdt6_100_500_2_10.csv", row.names = FALSE)




# ----------------Many objective Config--------------
NGEN      <- 500
CXPB      <- 0.8
MUTPB     <- 0.2
BOUND_LOW <- 0
BOUND_UP  <- 1

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

## MOEA/D
# out <- moead(problem = problem,
#              decomp = decomp, aggfun = aggfun, neighbors = neighbors, variation = variation,
#              update = update, constraint = constraint, scaling = scaling, stopcrit = stopcrit,
#              showpars = showpars, seed = seed)
results.dtlz.1 <- moead(problem = problem,
                       preset = preset_moead("original"),
                       decomp = list(name = "SLD", H = P),
                       stopcrit = stopcrit, showpars = showpars, seed = seed)

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
                        decomp = list(name = "SLD", H = P),
                        stopcrit = stopcrit, showpars = showpars, seed = seed)


# out <- moead(problem = problem.dtlz2,
#              decomp = list(name = "SLD", H = P),
#              aggfun = aggfun,
#              neighbors = neighbors, variation = variation,
#              update = update, constraint = constraint,
#              scaling = scaling, stopcrit = stopcrit,
#              showpars = showpars, seed = seed)

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
                        decomp = list(name = "SLD", H = P),
                        stopcrit = stopcrit, showpars = showpars, seed = seed)

write.csv(results.dtlz.3$Y, file = "moeadr_fitness_moead_dtlz3_252_500_6_6.csv", row.names = FALSE)


# ----------DTLZ 7----------
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
                        decomp = list(name = "SLD", H = P),
                        stopcrit = stopcrit, showpars = showpars, seed = seed)

write.csv(results.dtlz.7$Y, file = "moeadr_fitness_moead_dtlz3_252_500_3_7.csv", row.names = FALSE)

