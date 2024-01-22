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



# Read the data from the CSV files
deap.nsga2 <- read.csv("C:/Users/Maria/Downloads/Tesis/Simulaciones/Many Objective/DEAP/deap_time_nsga2_motsp_92_500_3_100.csv")
deap.nsga3 <- read.csv("C:/Users/Maria/Downloads/Tesis/Simulaciones/Many Objective/DEAP/deap_time_nsga3_motsp_92_500_3_100.csv")
pymoo.nsga2 <- read.csv("C:/Users/Maria/Downloads/Tesis/Simulaciones/Many Objective/pymoo/pymoo_time_nsga2_motsp_92_500_3_100.csv")
pymoo.nsga3 <- read.csv("C:/Users/Maria/Downloads/Tesis/Simulaciones/Many Objective/pymoo/pymoo_time_nsga3_motsp_92_500_3_100.csv")
rmoo.nsga2 <- read.csv("C:/Users/Maria/Downloads/Tesis/Simulaciones/Many Objective/rmoo/rmoo_time_nsga2_motsp_92_500_3_100.csv")
rmoo.nsga3 <- read.csv("C:/Users/Maria/Downloads/Tesis/Simulaciones/Many Objective/rmoo/rmoo_time_nsga3_motsp_92_500_3_100.csv")
# moeadr.moead <- read.csv("C:/Users/Maria/Downloads/Tesis/Simulaciones/Many Objective/MOEADr/moeadr_time_moead_dtlz7_92_500_3_10.csv")

# Add a new column to each data frame that represents the framework
rmoo.nsga2$Framework <- "RMOO-NSGA-II"
rmoo.nsga3$Framework <- "RMOO-NSGA-III"
pymoo.nsga2$Framework <- "PYMOO-NSGA-II"
pymoo.nsga3$Framework <- "PYMOO-NSGA-III"
deap.nsga3$Framework <- "DEAP-NSGA-III"
deap.nsga2$Framework <- "DEAP-NSGA-II"
moeadr.moead$Framework <- "MOEADr-MOEA/D"
# Combine all the data frames into one
combined_data <- rbind(rmoo.nsga2,
                       rmoo.nsga3,
                       pymoo.nsga2,
                       pymoo.nsga3,
                       deap.nsga3,
                       deap.nsga2,
                       moeadr.moead)

# Create the plot
ggplot(combined_data, aes(x=Generacion, y=Tiempo, color=Framework)) +
  geom_line() +
  labs(title="Tiempos de Ejecución en MOTSP",
       x="Generación",
       y="Tiempo Ejecución (Seg.)",
       color="Framework")


rm(rmoo.nsga2,
      rmoo.nsga3,
      pymoo.nsga2,
      pymoo.nsga3,
      deap.nsga3,
      deap.nsga2,
      moeadr.moead)
