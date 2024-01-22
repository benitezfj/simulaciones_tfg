library(rmoo)
library(tidyverse)

calculate_metrics <- function(frameworks, problems, algorithms, reference_point){
  pattern <- "%s_nsga2_%s.csv"
  moeadr_pattern <- "%s_moead_%s.csv"
  pareto_pattern <- "%s_pareto"

  for (i in problems) {
    pareto_points <- get(sprintf(pareto_pattern, i))

    for (j in libraries) {
      if (i == "zdt5" && j == "moeadr") {
        next
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

reference_point <- c(2,2)
libraries <- c("deap", "moeadr", "pymoo", "rmoo")
problems <- c("zdt1", "zdt2","zdt3","zdt4","zdt5","zdt6")
calculate_metrics(libraries, problems, reference_point)











calculate_metrics <- function(problems, frameworks, algorithms, reference_point, input_dir){
  pattern <- paste0(input_dir,"/","%s_%s_%s.csv")
  for (problem in problems) {
    pareto_points <- get(paste0(problem, "_pareto"))
    result_problem <- list()
    for (algorithm in algorithms) {
      for (framework in frameworks) {
        if ((problem == "zdt5" || problem == "motsp") && framework == "moeadr" && algorithm == "moead") {
          next
        } else if ((framework == "pymoo" || framework == "deap" || framework == "rmoo") && algorithm == "moead") {
          next
        } else if (framework == "moeadr" && (algorithm == "nsga2" || algorithm == "nsga3")){
          next
        }

        dataset_name <- sprintf(pattern, framework,algorithm,problem)
        content <- read.csv(dataset_name)

        ref_point <- rep(reference_point, ncol(content))

        if (problem == "dtlz7" || problem == "dtlz1" || problem == "motsp") {
          problem_metric <- data.frame(Framework = framework,
                                       Algorithm = algorithm,
                                       Problem = problem,
                                       HV = emoa::dominated_hypervolume(points = t(content), ref = ref_point),
                                       GD = ecr::computeGenerationalDistance(t(content), t(pareto_points)),
                                       IGD = eaf::igd_plus(data = content, reference = pareto_points))

          # metrics <- cbind(
          #   emoa::dominated_hypervolume(points = t(content), ref = ref_point),
          #   ecr::computeGenerationalDistance(t(content), t(pareto_points)),
          #   eaf::igd_plus(data = content, reference = pareto_points)
          # )
        } else{
          problem_metric <- data.frame(Framework = framework,
                                       Algorithm = algorithm,
                                       Problem = problem,
                                       HV = emoa::dominated_hypervolume(points = t(content), ref = ref_point),
                                       GD = ecr::computeGenerationalDistance(t(content), t(pareto_points())),
                                       IGD = eaf::igd_plus(data = content, reference = pareto_points()))

          # metrics <- cbind(
          #   emoa::dominated_hypervolume(points = t(content), ref = ref_point),
          #   ecr::computeGenerationalDistance(t(content), t(pareto_points())),
          #   eaf::igd_plus(data = content, reference = pareto_points())
          # )
        }

        result_problem[[length(result_problem) + 1]] <- problem_metric

        cat(framework,"_",algorithm,"_",problem,"\n")
        cat("\n")

      }
    }
    all_result_problem <- do.call(rbind, result_problem)
    matrix_name_problem <- paste0("metrics_",problem,".csv")
    write.csv(all_result_problem, matrix_name_problem, row.names = FALSE)
  }
}

problems <- c("dtlz1","dtlz2","dtlz3","dtlz7")
input_dir  <- "C:/Users/Maria/Downloads/Tesis/Simulaciones/resultados/DTLZ"

problems <- c("zdt1","zdt2","zdt3","zdt4","zdt5","zdt6")
input_dir  <- "C:/Users/Maria/Downloads/Tesis/Simulaciones/resultados/ZDT"


problems <- "motsp"
input_dir  <- "C:/Users/Maria/Downloads/Tesis/Simulaciones/resultados/MOTSP"

frameworks <- c("deap", "moeadr", "pymoo", "rmoo")

algorithms <- c("nsga2","nsga3", "moead")

reference_point <- 1

calculate_metrics(problems, frameworks, algorithms, reference_point, input_dir)





