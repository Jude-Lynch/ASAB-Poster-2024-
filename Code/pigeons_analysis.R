rm(list = ls())
library(swaRmverse)
library(dplyr)

################################################
#Pigeons----

#santos----
santosPigeon <- read.csv("../Data/Leadership in homing pigeon flocks (Columba livia) (data from Santos et al. 2014) (3).csv")
santosPigeon$dateTime <- as.POSIXct(santosPigeon$timestamp, format = '%d/%m/%Y %H:%M:%OS', tz = "Europe/Paris")
unique(santosPigeon$individual.local.identifier)

santosPigeon <- santosPigeon %>% distinct(individual.local.identifier,
                      dateTime, .keep_all = T)

data_santos <- set_data_format(raw_x = santosPigeon$location.long,
                           raw_y = santosPigeon$location.lat,
                           raw_t = santosPigeon$dateTime,
                           raw_id = santosPigeon$individual.local.identifier,
                           origin = min(santosPigeon$dateTime),
                           tz = "Europe/Paris"
)

head(data_santos)

tail(data_santos)
plot(data_santos$x, data_santos$y)
is_geo <- TRUE
data_dfs_santos <- add_velocities(data_santos,
                       geo = is_geo,
                       verbose = TRUE,
                       parallelize = FALSE
)
hist(data_dfs_santos[[1]]$speed)
hist(santosPigeon$ground.speed)

print(paste("Velocity information added for", length(data_dfs_santos), "sets."))

sampling_timestep <- 1
time_window <-  10 # seconds

smoothing_time_window <- time_window / sampling_timestep

g_metr_santos <- group_metrics_per_set(data_list = data_dfs_santos,
                                mov_av_time_window = smoothing_time_window,
                                step2time = sampling_timestep,
                                geo = is_geo,
                                parallelize = FALSE
)
head(g_metr_santos)
hist(g_metr_santos$speed)
hist(g_metr_santos$pol)

data_santos <- pairwise_metrics(data_list = data_dfs_santos,
                            geo = is_geo,
                            verbose = TRUE,
                            parallelize = FALSE,
                            add_coords = FALSE # could be set to FALSE if the relative positions of neighbors are not needed
)

head(data_santos)

palfordens <- wesanderson::wes_palette('Zissou1', 5, type = 'continuous')

data_santos <- swaRmverse::add_rel_pos_coords(data_santos)
head(data_santos)
hist(data_santos$bangl)

quantile(data_santos$nnd)
ggplot2::ggplot(data_santos[data_santos$nnd < 3,], ggplot2::aes(x = nnx, y = nny)) +
  ggplot2::stat_density_2d(
    geom = "tile",
    ggplot2::aes(fill = ..density..),
   # n = c(50, 4),
    contour = F
  ) +
  ggplot2::labs(x = 'Nearest Neighbour - X',
                y = 'Nearest Neighbour - Y',
                fill = 'Density') +
  ggplot2::scale_fill_gradientn(colours = palfordens) +
  ggplot2::geom_point(x = 0, y = 0, size = 3, shape = 17, color = 'black' ) +
  ggplot2::theme_bw() +
  ggplot2::scale_x_continuous(expand = c(0,0))+
  ggplot2::scale_y_continuous(expand = c(0,0))+
  ggplot2::theme(legend.position = c(0.93, 0.82),
                 plot.title = ggplot2::element_blank(),
                 panel.grid = ggplot2::element_blank(),
                 axis.title = ggplot2::element_text( size = 14,  color = 'black',family = 'Palatino Linotype'),
                 axis.text = ggplot2::element_text( size = 14, color = 'black', family = 'Palatino Linotype'),
                 legend.title = ggplot2::element_text( size = 16,  color = 'black',family = 'Palatino Linotype'),
                 legend.text = ggplot2::element_text( size = 10,  color = 'black',family = 'Palatino Linotype') )




santos_metrics <- col_motion_metrics(data_santos,
                                            global_metrics = g_metr_santos,
                                            step2time = sampling_timestep,
                                            verbose = TRUE,
                                            speed_lim = 15,
                                            pol_lim = 0.97,
                                            noise_thresh = 5)
# Percentile: Speed 20% (0.55), Pol 90% (0.97). 7179 events over 150.2 mins.

santos_metrics$species <- "Columba livia (Santos)"
head(santos_metrics)
summary(santos_metrics$event_dur)
hist(santos_metrics$mean_mean_nnd)
write.csv(santos_metrics, '../Data/santos_metrics.csv', row.names = F)

######################################
#gagliardo----

gagPigeon <- read.csv("../Data/Pigeon (Gagliardo)/Use of visual familiar cues in anosmic pigeons.csv")
gagPigeon$dateTime <- as.POSIXct(x = gagPigeon$timestamp, format = '%d/%m/%Y %H:%M:%S', tz = 'Europe/Rome')

data_gag <- set_data_format(
  raw_x = gagPigeon$location.long,
  raw_y = gagPigeon$location.lat,
  raw_t = gagPigeon$dateTime,
  raw_id = gagPigeon$individual.local.identifier,
  tz = "Europe/Rome"
 )
head(data_gag)
unique(data_gag$id)
plot(data_gag$x, data_gag$y)

is_geo <- TRUE

data_dfs_gag <- add_velocities(data_gag,
                        geo = is_geo,
                        verbose = TRUE,
                        parallelize = FALSE
 )

head(data_dfs_gag[[1]])
print(paste("Velocity information added for", length(data_dfs_gag), "sets."))

hist(data_dfs_gag[[1]]$speed)
hist(g_metr_santos$pol)

sampling_timestep <- 1
time_window <- 10 # seconds
smoothing_time_window <- time_window / sampling_timestep

g_metr_gag <- group_metrics_per_set(
  data_list = data_dfs_gag,
  mov_av_time_window = smoothing_time_window,
  step2time = sampling_timestep,
  geo = is_geo,
  parallelize = FALSE
)

g_metr_gag %>% group_by(set) %>% summarise(n_timesteps = n(), N = mean(N)) #N; mean of N, splits data into set, summaries sets. n() number of elements. maxtime - meantime
g_metr_gag <- g_metr_gag[g_metr_gag$N > 1, ]
g_metr_gag <- g_metr_gag[g_metr_gag$speed > 5, ]
g_metr_gag <- g_metr_gag[g_metr_gag$speed < 50, ]
hist(g_metr_gag$speed)

data_dfs_gag <- data_dfs_gag[unique(g_metr_gag$set)]
data_gag <- pairwise_metrics(data_list = data_dfs_gag,
                             geo = is_geo,
                             verbose = TRUE,
                             parallelize = FALSE,
                             add_coords = FALSE # could be set to FALSE if the relative positions of neighbors are not needed
)

hist(g_metr_gag$speed)
data_gag <- data_gag[data_gag$set ]
head(data_gag)
tail(data_gag)
summary(data_gag)

gag_metrics <- col_motion_metrics(
  data_gag,
  global_metrics = g_metr_gag,
  step2time = sampling_timestep,
  verbose = TRUE,
  speed_lim = NA,
  pol_lim = NA, noise_thresh = 5)
# Percentile: Speed 55% (0.08), Pol 40% (0.97). 941 events over 190.65 mins.

head(gag_metrics)
summary(gag_metrics$event_dur)
gag_metrics <- gag_metrics[gag_metrics$event_dur > 10, ]
summary(gag_metrics)
nrow(gag_metrics)

gag_metrics$species <- "Columba livia (Gagliardo)"
write.csv(gag_metrics, '../Data/gag_metrics.csv', row.names = F)


#THE ENDING POINT OF EACH DATASET ANALYSIS FILE

#All dataframes done:
gag_metrics <- read.csv('data/gag_metrics.csv')
santos_metrics <- read.csv('data/santos_metrics.csv')

#rbind----
all_data <- rbind(gag_metrics, santos_metrics)
write.csv(all_data, '../Data/metrics_pigeons.csv', row.names = FALSE)

#PLOTS----


#PCA----
new_pca <- swarm_space(metrics_data = all_data,
                       space_type = "pca"
)

ggplot2::ggplot(new_pca$swarm_space,
                ggplot2::aes(x = PC1, y = PC2, color = species)
) +
  ggplot2::geom_point() +
  ggplot2::theme_bw()

summary(new_pca$pca)
new_pca$pca$rotation
pcs_info <- new_pca$pca$rotation[, new_pca$pca$sdev > 1]

print(pcs_info)

summary(new_pca$pca)

#autpolot PCA----

install.packages("ggfortify")

library(ggfortify)

colnames(all_data)

toplot <- all_data[,c(5:14, 16)]

myPCA <- prcomp(scores)

autoplot(new_pca$pca,
         data = toplot, colour = 'species', loadings = TRUE, loadings.label = TRUE, loadings.label.size  = 6,
         loadings.label.colour = 'grey10'
)


new_pca$pca

tored ~ all_data

swaRmverse::swarm_space







