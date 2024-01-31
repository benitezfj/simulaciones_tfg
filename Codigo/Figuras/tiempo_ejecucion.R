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
deap.nsga2 <- read.csv("C:/Users/Maria/Downloads/Tesis/Simulaciones/Multi-objective/DEAP/deap_time_nsga2_zdt5_100_500_2_80.csv")
deap.nsga3 <- read.csv("C:/Users/Maria/Downloads/Tesis/Simulaciones/Multi-objective/DEAP/deap_time_nsga3_zdt5_100_500_2_80.csv")
pymoo.nsga2 <- read.csv("C:/Users/Maria/Downloads/Tesis/Simulaciones/Multi-objective/pymoo/pymoo_time_nsga2_zdt5_100_500_2_80.csv")
pymoo.nsga3 <- read.csv("C:/Users/Maria/Downloads/Tesis/Simulaciones/Multi-objective/pymoo/pymoo_time_nsga3_zdt5_100_500_2_80.csv")
rmoo.nsga2 <- read.csv("C:/Users/Maria/Downloads/Tesis/Simulaciones/Multi-objective/rmoo/rmoo_time_nsga2_zdt5_100_500_2_80.csv")
rmoo.nsga3 <- read.csv("C:/Users/Maria/Downloads/Tesis/Simulaciones/Multi-objective/rmoo/rmoo_time_nsga3_zdt5_100_500_2_80.csv")
# moeadr.moead <- read.csv("C:/Users/Maria/Downloads/Tesis/Simulaciones/Many Objective/MOEADr/moeadr_time_moead_motsp_92_500_3_100.csv")

# Add a new column to each data frame that represents the framework
rmoo.nsga2$Framework <- "RMOO-NSGA-II"
rmoo.nsga3$Framework <- "RMOO-NSGA-III"
pymoo.nsga2$Framework <- "PYMOO-NSGA-II"
pymoo.nsga3$Framework <- "PYMOO-NSGA-III"
deap.nsga3$Framework <- "DEAP-NSGA-III"
deap.nsga2$Framework <- "DEAP-NSGA-II"
# moeadr.moead$Framework <- "MOEADr-MOEA/D"

framework_colors <- c("RMOO-NSGA-II" = "#baa6ff",
"RMOO-NSGA-III" = "#fc89e0",
"PYMOO-NSGA-II" = "#41d0ae",
"PYMOO-NSGA-III" = "#40c9f1",
"DEAP-NSGA-II" = "#fa9990",
"DEAP-NSGA-III" = "#d3b241",
"MOEADr-MOEA/D" = "#7ec740")


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
  labs(title="Tiempos de Ejecución en DTLZ 1",
       x="Generación",
       y="Tiempo Ejecución (Seg.)",
       color="Framework") + theme_minimal()


ggplot(combined_data, aes(x=Generacion, y=Tiempo, color=Framework)) +
  geom_line() +
  scale_color_manual(values = framework_colors) +
  labs(title="Tiempos de Ejecución en ZDT 5",
       x="Generación",
       y="Tiempo Ejecución (Seg.)",
       color="Framework") + theme_minimal()

rm(rmoo.nsga2,
      rmoo.nsga3,
      pymoo.nsga2,
      pymoo.nsga3,
      deap.nsga3,
      deap.nsga2,
      moeadr.moead)




library(ggplot2)
library(dplyr)
library(reshape2)

input_dir <- "C:/Users/Maria/Downloads/Tesis/Simulaciones/Many Objective/results"
files <- list.files(pattern = "*.csv")

last_iter_times <- c()
df <- data.frame()

for (file in files) {
  df <- read.csv(file)
  print(head(df))
  # last_iter <- df$Execution.Time[length(df$Execution.Time)]
  # last_iter <- df[df$Generacion == max(df$Generacion), "Tiempo"]
  # last_iter_times <- c(last_iter_times, last_i0ter)
}

# Calculate max, mean, and min of the vector
max_val <- max(last_iter_times)
mean_val <- mean(last_iter_times)
min_val <- min(last_iter_times)

# Print the max, mean, and min
print(paste0(max_val," ",mean_val," ",min_val))

df <- data.frame(Tiempo = last_iter_times)

# Create a box plot
ggplot(df, aes(x = "PYMOO NSGA-III", y = Tiempo)) +
  geom_boxplot() +
  labs(x = "", y = "Tiempo de Ejecución (Seg.)", title = "ZDT 6") + theme_minimal() + theme(plot.title = element_text(hjust = 0.5))




library(ggplot2)

input_dir <- "C:/Users/Maria/Downloads/Tesis/Simulaciones/Multi-objective/results"
files <- list.files(input_dir, pattern = "*.csv")

data <- data.frame(Value=numeric(),Prefix=character())

for (file in files) {
  prefix <- strsplit(file, "-")[[1]][1]
  df <- read.csv(file)
  if(length(colnames(df))==3){
    last_iter <- df[df$gen == max(df$gen), "Tiempo"]
  } else if (length(colnames(df))==2){
    last_iter <- df[df$Generacion == max(df$Generacion), "Tiempo"]
  } else{
    last_iter <- df$Execution.Time[length(df$Execution.Time)]
  }
  data <- rbind(data, data.frame(Value=last_iter,Prefix=prefix))
}
colnames(data) <- c("Tiempo","Implementacion")
write.csv(data, file = "zdt6_tiempos_boxplot_data.csv",row.names = FALSE)
data <- read.csv(file = "zdt6_tiempos_boxplot_data.csv")

colors <- c("MOEADr-MOEA/D" = '#53b400',
            "DEAP-NSGA-II" = '#f8766d',
            "PYMOO-NSGA-II"='#00c094',
            "RMOO-NSGA-II"='#00b6eb',
            "DEAP-NSGA-III"='#5D8098',
            "PYMOO-NSGA-III"='#a58aff',
            "RMOO-NSGA-III"='#fb61d7')

# Create a boxplot with a legend on the right
ggplot(data, aes(x=Implementacion, y=Tiempo, fill=Implementacion)) +
  geom_boxplot() +
  scale_fill_manual(values=colors) +
  theme_minimal() +
  theme(legend.position="right",
        plot.title = element_text(hjust = 0.5)) +
  labs(x = "", y = "Tiempo de Ejecución (Seg.)", title = "ZDT 6")

