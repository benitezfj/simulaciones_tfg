library(rmoo)
library(tidyverse)
library(ggplot2)
library(plotly)
library(gg3D)

scatter_fitness_ggplot <- function(problems, frameworks, algorithms, input_dir, output_dir){
  pattern <- paste0(input_dir,"/","%s_%s_%s.csv")
  # pattern <- "C:/Users/Maria/Downloads/Tesis/Simulaciones/resultados/ZDT/%s_%s_%s.csv"
  # moeadr_pattern <- "C:/Users/Maria/Downloads/Tesis/Simulaciones/resultados/ZDT/%s_moead_%s.csv"
  for (problem in problems) {
    results <- list()
    for (algorithm in algorithms) {
      for (framework in frameworks) {
        if (problem == "zdt5" && framework == "moeadr" && algorithm == "moead") {
          next
        } else if ((framework == "pymoo" || framework == "deap" || framework == "rmoo") && algorithm == "moead") {
          next
        } else if (framework == "moeadr" && (algorithm == "nsga2" || algorithm == "nsga3")){
          next
        }

        dataset_name <- sprintf(pattern, framework,algorithm,problem)

        # if(framework == "moeadr"){
        #   dataset_name <- sprintf(moeadr_pattern, framework, problem)
        # } else{
        #   dataset_name <- sprintf(pattern, framework,algorithm,problem)
        # }

        content <- read.csv(dataset_name)
        content$framework <- paste(toupper(framework),toupper(algorithm), sep = "-")

        results[[length(results) + 1]] <- content

      }
    }
    results_df <- do.call(rbind, results)

    p <- ggplot(results_df, aes(x=f1, y=f2, color=framework, shape=framework)) +
      geom_point() +
      scale_shape_manual(values=(seq_along(unique(results_df$framework)) + 15)) +
      theme_minimal() +
      labs(title=toupper(problem), x="f1", y="f2", color="Framework", shape="Framework")

    ggsave(filename=file.path(output_dir, paste0("Results of ", problem, ".png")), plot=p)
  }

}

problems <- c("zdt1","zdt2","zdt3","zdt4","zdt5","zdt6")
frameworks <- c("deap", "moeadr", "pymoo", "rmoo")
algorithms <- c("nsga2","nsga3", "moead")
input_dir <- "C:/Users/Maria/Downloads/Tesis/Simulaciones/resultados/ZDT"
output_dir <- "C:/Users/Maria/Downloads/Tesis/Simulaciones/resultados/scatter"

scatter_fitness_ggplot(problems, frameworks, algorithms, output_dir)




pcp_fitness_ggplot <- function(problems, frameworks, algorithms, input_dir, output_dir){
  pattern <- paste0(input_dir,"/","%s_%s_%s.csv")

  for (problem in problems) {
    for (algorithm in algorithms) {
      for (framework in frameworks) {
        if ((framework == "pymoo" || framework == "deap" || framework == "rmoo") && algorithm == "moead") {
          next
        } else if (framework == "moeadr" && (algorithm == "nsga2" || algorithm == "nsga3")){
          next
        }

        dataset_name <- sprintf(pattern, framework,algorithm,problem)
        content <- read.csv(dataset_name)

        nObj <- ncol(content)
        colnames(content) <- sprintf("f_%s",seq(nObj))
        content$color <- grDevices::rainbow(nrow(content))
        content <- reshape2::melt(content, id.vars = "color")
        content <- dplyr::rename(content,
                                 'Valor_Objetivo' = value,
                                 'No_Objetivo' = variable)
        p <- ggplot2::ggplot(content, aes(x = No_Objetivo,
                                     y = Valor_Objetivo,
                                     group = color, colour=factor(color))) +
          ggplot2::geom_line(show.legend = FALSE) +
          ggplot2::theme_classic()
        ggsave(filename=file.path(output_dir, paste0(problem,"-", framework,"-",algorithm,".png")), plot=p)
        # ggsave(filename=file.path(output_dir, paste0("Results of ", problem, " using ", algorithm, " with ",framework,".png")), plot=p)

      }
    }
  }
}

problems <- c("dtlz1","dtlz2","dtlz3","dtlz7")
frameworks <- c("deap", "moeadr", "pymoo", "rmoo")
algorithms <- c("nsga2","nsga3", "moead")
input_dir <- "C:/Users/Maria/Downloads/Tesis/Simulaciones/resultados/DTLZ"
output_dir <- "C:/Users/Maria/Downloads/Tesis/Simulaciones/resultados/pcp"

pcp_fitness_ggplot(problems, frameworks, algorithms, input_dir, output_dir)




scatter_3d_fitness_ggplot <- function(problems, frameworks, algorithms, output_dir){
  pattern <- "C:/Users/Maria/Downloads/Tesis/Simulaciones/resultados/MOTSP/%s_%s_%s.csv"
  for (problem in problems) {
    for (algorithm in algorithms) {
      for (framework in frameworks) {
        if ((framework == "pymoo" || framework == "deap" || framework == "rmoo") && algorithm == "moead") {
          next
        } else if (framework == "moeadr" && (algorithm == "nsga2" || algorithm == "nsga3")){
          next
        }

        dataset_name <- sprintf(pattern, framework,algorithm,problem)
        content <- read.csv(dataset_name)

        nObj <- ncol(content)
        colnames(content) <- sprintf("f_%s",seq(nObj))

        p <- ggplot(content, aes(x=f_1, y=f_2, z=f_3)) +
          axes_3D() +
          stat_3D(color = "#020385") +
          labs_3D(labs=c("f_1", "f_2", "f_3"),
                  hjust=c(1,1,1.5),
                  vjust=c(1, 1, 1),
                  angle=c(0, 0, 0)) +
          theme_void()

        ggsave(filename=file.path(output_dir, paste0(problem,"-", framework,"-",algorithm,".png")), plot=p)

      }
    }
  }
}

problems <- c("motsp")
frameworks <- c("deap", "pymoo", "rmoo")
algorithms <- c("nsga2","nsga3")

output_dir <- "C:/Users/Maria/Downloads/Tesis/Simulaciones/resultados/scatter"

scatter_3d_fitness_ggplot(problems, frameworks, algorithms, output_dir)






moeadr_moead <- read.csv("C:/Users/Maria/Downloads/Tesis/Simulaciones/resultados/ZDT/moeadr_moead_zdt1.csv")
deap_nsga2 <- read.csv("C:/Users/Maria/Downloads/Tesis/Simulaciones/resultados/ZDT/deap_nsga2_zdt1.csv")
pymoo_nsga2 <- read.csv("C:/Users/Maria/Downloads/Tesis/Simulaciones/resultados/ZDT/pymoo_nsga2_zdt1.csv")
rmoo_nsga2 <- read.csv("C:/Users/Maria/Downloads/Tesis/Simulaciones/resultados/ZDT/rmoo_nsga2_zdt1.csv")
deap_nsga3 <- read.csv("C:/Users/Maria/Downloads/Tesis/Simulaciones/resultados/ZDT/deap_nsga2_zdt1.csv")
pymoo_nsga3 <- read.csv("C:/Users/Maria/Downloads/Tesis/Simulaciones/resultados/ZDT/pymoo_nsga2_zdt1.csv")
rmoo_nsga3 <- read.csv("C:/Users/Maria/Downloads/Tesis/Simulaciones/resultados/ZDT/rmoo_nsga2_zdt1.csv")
optimal <- as.data.frame(zdt1_pareto())
colnames(optimal) <- c("f1","f2")

moeadr_moead$framework <- "MOEADr-MOEA/D"
deap_nsga2$framework <- "DEAP-NSGA-II"
pymoo_nsga2$framework <- "PYMOO-NSGA-II"
rmoo_nsga2$framework <- "RMOO-NSGA-II"
deap_nsga3$framework <- "DEAP-NSGA-III"
pymoo_nsga3$framework <- "PYMOO-NSGA-III"
rmoo_nsga3$framework <- "RMOO-NSGA-III"
optimal$framework <- "Optimo"

shapes <- c("MOEADr-MOEA/D" = 19, "DEAP-NSGA-II" = 17,"PYMOO-NSGA-II"=18, "RMOO-NSGA-II"=16,"DEAP-NSGA-III"=15,"PYMOO-NSGA-III"=14,"RMOO-NSGA-III"=13,'Optimo' = NA)

colors <- c("MOEADr-MOEA/D" = '#53b400',
            "DEAP-NSGA-II" = '#f8766d',
            "PYMOO-NSGA-II"='#00c094',
            "RMOO-NSGA-II"='#00b6eb',
            "DEAP-NSGA-III"='#5D8098',
            "PYMOO-NSGA-III"='#a58aff',
            "RMOO-NSGA-III"='#fb61d7',
            'Optimo' = 'black')


# Combine all datasets into one
all_data <- rbind(moeadr_moead,deap_nsga2,pymoo_nsga2,rmoo_nsga2,deap_nsga3,pymoo_nsga3,rmoo_nsga3,optimal)
# Create a 2-D scatter plot with all_data
ggplot(all_data, aes(x=f1, y=f2, color=framework, shape=framework)) +
  geom_point() +
  scale_shape_manual(values=(seq_along(unique(all_data$framework)) + 15)) +  # Set the shapes manually
  theme_minimal() +
  labs(title="ZDT 1", x="X", y="Y", color="Framework", shape="Framework")

p <- ggplot(all_data, aes(x = f1, y = f2, color = framework, shape = framework)) +
            geom_point(data = subset(all_data, framework != 'Optimo')) +
            geom_line(data = subset(all_data, framework == 'Optimo')) +
            scale_shape_manual(values = shapes) +
            scale_color_manual(values = colors) +
            labs(title="ZDT 1", x="X", y="Y", color="Framework", shape="Framework") +
            theme_minimal()

ggsave(filename=file.path("C:/Users/Maria/Downloads/Tesis/Simulaciones/resultados/scatter",
                          paste0("ZDT1.png")), plot=p)

# ggplot(all_data, aes(x = f1, y = f2, color = framework)) +
#   geom_point(data = subset(all_data, framework != 'Optimo')) +
#   geom_line(data = subset(all_data, framework == 'Optimo')) +
#   scale_shape_manual(values=(seq_along(unique(all_data$framework)))) +
#   labs(title="ZDT 1", x="X", y="Y", color="Framework", shape="Framework") +
#   theme_minimal()


pcp_plot <- function(fitness) {
  fitness <- as.data.frame(fitness)
  nObj <- ncol(fitness)
  colnames(fitness) <- sprintf("f_%s",seq(nObj))
  fitness$color <- grDevices::rainbow(nrow(fitness))
  fitness <- reshape2::melt(fitness, id.vars = "color")
  fitness <- dplyr::rename(fitness,
                           'Valor_Objetivo' = value,
                           'No_Objetivo' = variable)
  ggplot2::ggplot(fitness, aes(x = No_Objetivo,
                               y = Valor_Objetivo,
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





# moeadr_moead <- read.csv("C:/Users/Maria/Downloads/Tesis/Simulaciones/resultados/DTLZ/moeadr_moead_dtlz7.csv")
deap_nsga2 <- read.csv("C:/Users/Maria/Downloads/Tesis/Simulaciones/resultados/MOTSP/deap_nsga2_motsp.csv")
pymoo_nsga2 <- read.csv("C:/Users/Maria/Downloads/Tesis/Simulaciones/resultados/MOTSP/pymoo_nsga2_motsp.csv")
rmoo_nsga2 <- read.csv("C:/Users/Maria/Downloads/Tesis/Simulaciones/resultados/MOTSP/rmoo_nsga2_motsp.csv")
deap_nsga3 <- read.csv("C:/Users/Maria/Downloads/Tesis/Simulaciones/resultados/MOTSP/deap_nsga3_motsp.csv")
pymoo_nsga3 <- read.csv("C:/Users/Maria/Downloads/Tesis/Simulaciones/resultados/MOTSP/pymoo_nsga3_motsp.csv")
rmoo_nsga3 <- read.csv("C:/Users/Maria/Downloads/Tesis/Simulaciones/resultados/MOTSP/rmoo_nsga3_motsp.csv")


moeadr_moead$framework <- "MOEADr-MOEA/D"
deap_nsga2$framework <- "DEAP-NSGA-II"
pymoo_nsga2$framework <- "PYMOO-NSGA-II"
rmoo_nsga2$framework <- "RMOO-NSGA-II"
deap_nsga3$framework <- "DEAP-NSGA-III"
pymoo_nsga3$framework <- "PYMOO-NSGA-III"
rmoo_nsga3$framework <- "RMOO-NSGA-III"
pareto$framework <- "Optimal"

all_data <- rbind(moeadr_moead,deap_nsga2,pymoo_nsga2,rmoo_nsga2,deap_nsga3,pymoo_nsga3,rmoo_nsga3,dtlz1_pareto)

all_data <- rbind(moeadr_moead,pareto)
all_data <- rbind(deap_nsga2,pareto)
all_data <- rbind(pymoo_nsga2,pareto)
all_data <- rbind(rmoo_nsga2,pareto)
all_data <- rbind(deap_nsga3,pareto)
all_data <- rbind(pymoo_nsga3,pareto)
all_data <- rbind(rmoo_nsga3,pareto)


ggplot(all_data, aes(x=f1, y=f2, z=f3, color=framework)) +
  axes_3D() +
  stat_3D() +
  labs_3D(labs=c("f_1", "f_2", "f_3"),
          hjust=c(1,1,1.5),
          vjust=c(1, 1, 1),
          angle=c(0, 0, 0)) +
  theme(legend.title = element_blank())


# fig <- plotly::plot_ly(all_data, x = ~f1,
#                        y = ~f2,
#                        z = ~f3,
#                        color = ~framework,
#                        colors = c('#0C4B8E', '#BF382A'),
#                        size = 1)
# fig <- fig %>% plotly::add_markers() #New plotly add
# fig <- fig %>% plotly::layout(scene = list(xaxis = list(title = 'f_1'),
#                                            yaxis = list(title = 'f_2'),
#                                            zaxis = list(title = 'f_3')))
# fig
