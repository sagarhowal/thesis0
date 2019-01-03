#This file is for reproducing all the Clustering Results for the Research Project 
#CLUSTERING


#Set Working Directory Before Executing
#setwd("")

#Load all libraries
library(TSrepr)
library(cluster)
library(clusterCrit)
library(ggplot2)


#Load Data
DT_wide_50 <- read_feather("DT_wide_50")


# Time Series Representations
data_seasprof <- repr_matrix(DT_wide_50, func = repr_seas_profile,
                             args = list(freq = 48, func = mean),
                             normalise = TRUE, func_norm = norm_z)



#Clustering
clusterings <- lapply(c(2:8), function(x)
  pam(data_seasprof, x))

#Get Daview Boulding values for all the Cluster combinations we tried 
DB_values <- sapply(seq_along(clusterings), function(x) 
  intCriteria(data_seasprof, as.integer(clusterings[[x]]$clustering),
              c("Davies_Bouldin")))

# Plot a graph for the Davies-Bouldin Values
ggplot(data.table(Clusters = 2:7, DBindex = unlist(DB_values)),
       aes(Clusters, DBindex)) +
  geom_line(size = 1) +
  geom_point(size = 3)
#somehow there are 2 clusters of 2. 
#one is good. One isn't 


# prepare data for plotting
data_plot <- data.table(melt(data.table(class = as.factor(clusterings[[1]]$clustering),
                                        data_seasprof)))
data_plot[, Time := rep(1:ncol(data_seasprof), each = nrow(data_seasprof))]
data_plot[, ID := rep(1:nrow(data_seasprof), ncol(data_seasprof))]

# prepare medoids
centers <- data.table(melt(clusterings[[1]]$medoids))
setnames(centers, c("Var1", "Var2"), c("class", "Time"))
centers[, ID := class]

# plot the results
ggplot(data_plot, aes(Time, value, group = ID)) +
  facet_wrap(~class, ncol = 2, scales = "free_y") +
  geom_line(color = "grey10", alpha = 0.65) +
  geom_line(data = centers, aes(Time, value),
            color = "firebrick1", alpha = 0.80, size = 1.2) +
  labs(x = "Time", y = "Load (normalised)")


#Direct Fourier Transform
data_dft <- repr_matrix(DT_wide_50, func = repr_dft, args = list(coef = 48),
                        normalise = TRUE, func_norm = norm_z)
dim(data_dft)
# 25730    48

#Clustering
clusterings <- lapply(c(2:5), function(x)
  pam(data_dft, x))

DB_values2 <- sapply(seq_along(clusterings), function(x) 
  intCriteria(data_dft, as.integer(clusterings[[x]]$clustering),
              c("Davies_Bouldin")))


ggplot(data.table(Clusters = 2:5, DBindex = unlist(DB_values2)),
       aes(Clusters, DBindex)) +
  geom_line(size = 1) +
  geom_point(size = 3)

# Plot Clusters
data_plot <- data.table(melt(data.table(class = as.factor(clusterings[[1]]$clustering),
                                        data_dft)))
data_plot[, Time := rep(1:ncol(data_dft), each = nrow(data_dft))]
data_plot[, ID := rep(1:nrow(data_dft), ncol(data_dft))]

# prepare medoids
centers <- data.table(melt(clusterings[[1]]$medoids))
setnames(centers, c("Var1", "Var2"), c("class", "Time"))
centers[, ID := class]

# plot the results
ggplot(data_plot, aes(Time, value, group = ID)) +
  facet_wrap(~class, ncol = 2, scales = "free_y") +
  geom_line(color = "grey10", alpha = 0.65) +
  geom_line(data = centers, aes(Time, value),
            color = "firebrick1", alpha = 0.80, size = 1.2) +
  labs(x = "Time", y = "Load (normalised)")










