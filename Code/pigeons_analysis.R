rm(list = ls())
installed.packages("swaRmverse")
library(swaRmverse)
library(dplyr)
library(tidyverse)
################################################
#Pigeons----

#santos----
santosPigeon <- read.csv("../Data/Datasets/Leadership in homing pigeon flocks (Columba livia) (data from Santos et al. 2014) (3).csv")
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

santos_metrics$species <- "Columba livia (Santos)"
head(santos_metrics)
summary(santos_metrics$event_dur)
hist(santos_metrics$mean_mean_nnd)
write.csv(santos_metrics, '../Data/santos_metrics.csv', row.names = F)

################################################################################
#Sankey Pigeons----
#Predators----
flockP <- read.csv("../Data/Datasets/Marina/empirical/transformed/all_self_p.csv")

flockP$column_label <- as.POSIXct(flockP$column_label, format = "%Y-%m-%d %H:%M:%S", tz = 'GMT')

flockP <- dplyr::mutate(flockP,
                     date_time = column_label + lubridate::seconds(time))
flockP$column_label <- as.factor(flockP$column_label)
levels(flockP$column_label) <- 1:length(unique(flockP$column_label))

data_flockP <- set_data_format(
  raw_x = flockP$posx,
  raw_y = flockP$posy,
  raw_t = flockP$time,
  raw_id = flockP$id,
  flightid = flockP$column_label,
  origin = min(flockP$time),
  tz = "GMT"
)

head(data_flockP)
unique(flockP$id)
plot(flockP$posx, flockP$posy)

is_geo <- TRUE

data_dfs_flockP <- add_velocities(data_flockP,
                               geo = is_geo,
                               verbose = TRUE,
                               parallelize = FALSE
)

head(data_dfs_flockP[[1]])
print(paste("Velocity information added for", length(data_dfs_flockP), "sets."))

hist(data_dfs_flockP[[1]]$speed)

sampling_timestep <- 1
time_window <- 10 # seconds
smoothing_time_window <- time_window / sampling_timestep

g_metr_flockP <- group_metrics_per_set(
  data_list = data_dfs_flockP,
  mov_av_time_window = smoothing_time_window,
  step2time = sampling_timestep,
  geo = is_geo,
  parallelize = FALSE
)

g_metr_flockP %>% group_by(set) %>% summarise(n_timesteps = n(), N = mean(N)) #N; mean of N, splits data into set, summaries sets. n() number of elements. maxtime - meantime
g_metr_flockP <- g_metr_flockP[g_metr_flockP$N > 1, ]
g_metr_flockP <- g_metr_flockP[g_metr_flockP$speed > 5, ]
g_metr_flockP <- g_metr_flockP[g_metr_flockP$speed < 50, ]
hist(g_metr_flockP$speed)
hist(g_metr_flockP$pol)

head(g_metr_flockP)
tail(g_metr_flockP)
data_dfs_flockP <- data_dfs_flockP[unique(g_metr_flockP$set)]
head(data_dfs_flockP[1])
data_flockP <- pairwise_metrics(data_list = data_dfs_flockP,
                             geo = is_geo,
                             verbose = TRUE,
                             parallelize = FALSE,
                             add_coords = FALSE # could be set to FALSE if the relative positions of neighbors are not needed
)

hist(g_metr_flockP$speed)
data_flockP <- data_flockP[data_flockP$set]
head(g_metr_flockP)
tail(g_metr_flockP)
summary(g_metr_flockP)

flockP_metrics <- col_motion_metrics(
  data_flockP,
  global_metrics = g_metr_flockP,
  step2time = sampling_timestep,
  verbose = TRUE,
  speed_lim = 15,
  pol_lim = 0.97, noise_thresh = 5)

head(g_metr_flockP)
summary(flockP_metrics$event_dur)
flockP_metrics <- flockP_metrics[flockP_metrics$event_dur > 10, ]
summary(flockP_metrics)
nrow(flockP_metrics)

flockP_metrics$species <- "Columba livia (Sankey (P))"
write.csv(flockP_metrics, '../Data/flockP_metrics.csv', row.names = F)

#No Predators----

flockC <- read.csv("../Data/Datasets/Marina/empirical/transformed/all_self_c.csv")

head(flockC)

flockC$column_label <- as.POSIXct(flockC$column_label, format = "%Y-%m-%d %H:%M:%S", tz = 'GMT')

data_flockC <- set_data_format(
  raw_x = flockC$posx,
  raw_y = flockC$posy,
  raw_t = flockC$time,
  raw_id = flockC$id,
  flightid = flockC$column_label,
  origin = min(flockC$time),
  tz = "GMT"
)

head(data_flockC)
unique(flockC$id)
plot(flockC$posx,flockC$posy)

is_geo <- TRUE

data_dfs_flockC <- add_velocities(data_flockC,
                                  geo = is_geo,
                                  verbose = TRUE,
                                  parallelize = FALSE
)

head(data_dfs_flockC[[1]])
print(paste("Velocity information added for", length(data_dfs_flockC), "sets."))

hist(data_dfs_flockC[[1]]$speed)

sampling_timestep <- 1
time_window <- 10 # seconds
smoothing_time_window <- time_window / sampling_timestep

g_metr_flockC <- group_metrics_per_set(
  data_list = data_dfs_flockC,
  mov_av_time_window = smoothing_time_window,
  step2time = sampling_timestep,
  geo = is_geo,
  parallelize = FALSE
)

g_metr_flockC %>% group_by(set) %>% summarise(n_timesteps = n(), N = mean(N)) #N; mean of N, splits data into set, summaries sets. n() number of elements. maxtime - meantime
g_metr_flockC <- g_metr_flockC[g_metr_flockC$N > 1, ]
g_metr_flockC <- g_metr_flockC[g_metr_flockC$speed > 5, ]
g_metr_flockC <- g_metr_flockC[g_metr_flockC$speed < 50, ]
hist(g_metr_flockC$speed)
hist(g_metr_flockC$pol)

head(g_metr_flockC)
tail(g_metr_flockC)
data_dfs_flockC <- data_dfs_flockC[unique(g_metr_flockC$set)]
head(data_dfs_flockC[1])

data_flockC <- pairwise_metrics(data_list = data_dfs_flockC,
                                geo = is_geo,
                                verbose = TRUE,
                                parallelize = FALSE,
                                add_coords = FALSE # could be set to FALSE if the relative positions of neighbors are not needed
)

hist(g_metr_flockC$speed)
hist(g_metr_flockC$pol)
data_flockC <- data_flockC[data_flockC$set]
head(g_metr_flockC)
tail(g_metr_flockC)
summary(g_metr_flockC)

flockC_metrics <- col_motion_metrics(
  data_flockC,
  global_metrics = g_metr_flockC,
  step2time = sampling_timestep,
  verbose = TRUE,
  speed_lim = 15,
  pol_lim = 0.97, noise_thresh = 5)

head(g_metr_flockC)
summary(flockC_metrics$event_dur)
flockC_metrics <- flockC_metrics[flockC_metrics$event_dur > 10, ]
summary(flockC_metrics)
nrow(flockC_metrics)

flockC_metrics$species <- "Columba livia (Sankey (C))"
write.csv(flockC_metrics, '../Data/flockC_metrics.csv', row.names = F)
################################################################################
#homing flights ----
#hf1----

install.packages('swaRmverse')

library(swaRmverse)

#Import Datasets via working directory.

hf1 <- rbind(hf1_A,hf1_C,hf1_D,hf1_F,hf1_G,hf1_H,hf1_I,hf1_J,hf1_K,hf1_L)
write.csv(hf1,'../Data/Datasets/Trajectories/pigeonflocks_trajectories/hf1_bind.csv')

hf1 <- read.csv('../Data/Datasets/Trajectories/pigeonflocks_trajectories/hf1_bind.csv')

head(hf1, 10)
hf1$sec <-  hf1$centisec/100
head(hf1)
data_hf1 <- set_data_format(
  raw_x = hf1$X,
  raw_y = hf1$Y,
  raw_t = hf1$sec,
  raw_id = hf1$id,
  flightid = hf1$flight,
  tz = "GMT"
)

summary(data_hf1)
head(data_hf1, 10)
unique(hf1$id)
plot(hf1$X,hf1$Y)

is_geo <- FALSE

data_dfs_hf1 <- add_velocities(data_hf1,
                                  geo = is_geo,
                                  verbose = TRUE,
                                  parallelize = FALSE
)


head(data_dfs_hf1[[1]])
hist(data_dfs_hf1[[3]]$speed)
plot(data_dfs_hf1[[1]]$y, data_dfs_hf1[[1]]$x)

print(paste("Velocity information added for", length(data_dfs_hf1), "sets."))

hist(data_dfs_flockC[[1]]$speed)

sampling_timestep <- 1
time_window <- 10 # seconds
smoothing_time_window <- time_window / sampling_timestep

g_metr_hf1 <- group_metrics_per_set(
  data_list = data_dfs_hf1,
  mov_av_time_window = smoothing_time_window,
  step2time = sampling_timestep,
  geo = is_geo,
  parallelize = FALSE
)

head(g_metr_hf1)
summary(g_metr_hf1)

library(dplyr)
g_metr_hf1 %>% group_by(set) %>% summarise(n_timesteps = n(), N = mean(N)) #N; mean of N, splits data into set, summaries sets. n() number of elements. maxtime - meantime
g_metr_hf1 <- g_metr_hf1[g_metr_hf1$N > 1, ]
g_metr_hf1 <- g_metr_hf1[g_metr_hf1$speed > 5, ]
g_metr_hf1 <- g_metr_hf1[g_metr_hf1$speed < 50, ]
hist(g_metr_hf1$speed)
hist(g_metr_hf1$pol)

head(g_metr_hf1)
tail(g_metr_hf1)
data_dfs_hf1 <- data_dfs_hf1[unique(g_metr_hf1$set)]
head(data_dfs_hf1[1])

data_hf1 <- pairwise_metrics(data_list = data_dfs_hf1,
                                geo = is_geo,
                                verbose = TRUE,
                                parallelize = FALSE,
                                add_coords = FALSE # could be set to FALSE if the relative positions of neighbors are not needed
)

hist(g_metr_hf1$speed)
hist(g_metr_hf1$pol)
data_hf1 <- data_hf1[data_hf1$set]
head(g_metr_hf1)
tail(g_metr_hf1)
summary(g_metr_hf1)

hf1_metrics <- col_motion_metrics(
  data_hf1,
  global_metrics = g_metr_hf1,
  step2time = sampling_timestep,
  verbose = TRUE,
  speed_lim = 15,
  pol_lim = 0.85, noise_thresh = 5)

head(g_metr_hf1)
summary(hf1_metrics$event_dur)
hf1_metrics <- hf1_metrics[hf1_metrics$event_dur > 10, ]
summary(hf1_metrics)
nrow(hf1_metrics)

hf1_metrics$species <- "Columba livia (Nagy (hf1))"
write.csv(hf1_metrics, '../Data/Metrics/hf1_metrics.csv', row.names = F)

head(hf1_metrics)

################################################################################
#hf2----

library(swaRmverse)

hf2 <- rbind(hf2_A,hf2_B,hf2_C,hf2_D,hf2_G,hf2_H,hf2_I,hf2_J,hf2_L)
write.csv(hf2,'../Data/Datasets/Trajectories/pigeonflocks_trajectories/hf2_bind.csv')
hf2 <- read.csv('../Data/Datasets/Trajectories/pigeonflocks_trajectories/hf2_bind.csv')

hf2$timestamp <- paste(hf2$date, hf2$time, sep = ' ')
hf2$timestamp <- as.POSIXct(hf2$timestamp, format = '%d/%m/%Y %H:%M:%OS', tz = "GMT")

head(hf2, 10)
hf2$sec <-  hf2$centisec/100
head(hf2)
data_hf2 <- set_data_format(
  raw_x = hf2$X,
  raw_y = hf2$Y,
  raw_t = hf2$sec,
  raw_id = hf2$id,
  flightid = hf2$flight,
  tz = "GMT"
)

summary(data_hf2)
head(data_hf2, 10)
unique(hf2$id)
plot(hf2$X,hf2$Y)

is_geo <- FALSE

data_dfs_hf2 <- add_velocities(data_hf2,
                               geo = is_geo,
                               verbose = TRUE,
                               parallelize = FALSE
)


head(data_dfs_hf2[[1]])
hist(data_dfs_hf2[[3]]$speed)
plot(data_dfs_hf2[[1]]$x, data_dfs_hf2[[1]]$y)

print(paste("Velocity information added for", length(data_dfs_hf2), "sets."))

hist(data_dfs_hf2[[1]]$speed)

sampling_timestep <- 1
time_window <- 10 # seconds
smoothing_time_window <- time_window / sampling_timestep

g_metr_hf2 <- group_metrics_per_set(
  data_list = data_dfs_hf2,
  mov_av_time_window = smoothing_time_window,
  step2time = sampling_timestep,
  geo = is_geo,
  parallelize = FALSE
)

head(g_metr_hf2)
summary(g_metr_hf2)

library(dplyr)
g_metr_hf2 %>% group_by(set) %>% summarise(n_timesteps = n(), N = mean(N)) #N; mean of N, splits data into set, summaries sets. n() number of elements. maxtime - meantime
g_metr_hf2 <- g_metr_hf2[g_metr_hf2$N > 1, ]
g_metr_hf2 <- g_metr_hf2[g_metr_hf2$speed > 5, ]
g_metr_hf2 <- g_metr_hf2[g_metr_hf2$speed < 50, ]
hist(g_metr_hf2$speed)
hist(g_metr_hf2$pol)

head(g_metr_hf2)
tail(g_metr_hf2)
data_dfs_hf2 <- data_dfs_hf2[unique(g_metr_hf2$set)]
head(data_dfs_hf2[1])

data_hf2 <- pairwise_metrics(data_list = data_dfs_hf2,
                             geo = is_geo,
                             verbose = TRUE,
                             parallelize = FALSE,
                             add_coords = FALSE # could be set to FALSE if the relative positions of neighbors are not needed
)

hist(g_metr_hf2$speed)
hist(g_metr_hf2$pol)
data_hf2 <- data_hf2[data_hf2$set]
head(g_metr_hf2)
tail(g_metr_hf2)
summary(g_metr_hf2)

hf2_metrics <- col_motion_metrics(
  data_hf2,
  global_metrics = g_metr_hf2,
  step2time = sampling_timestep,
  verbose = TRUE,
  speed_lim = 15,
  pol_lim = 0.97, noise_thresh = 5)

head(g_metr_hf2)
summary(hf2_metrics$event_dur)
hf2_metrics <- hf2_metrics[hf2_metrics$event_dur > 10, ]
summary(hf2_metrics)
nrow(hf2_metrics)

hf2_metrics$species <- "Columba livia (Nagy (hf2))"
write.csv(hf2_metrics, '../Data/Metrics/hf2_metrics.csv', row.names = F)

head(hf2_metrics)

################################################################################

#hf3----

library(swaRmverse)

hf3 <- rbind(hf3_A,hf3_B,hf3_C,hf3_D,hf3_G,hf3_H,hf3_I,hf3_J,hf3_L)
write.csv(hf3,'../Data/Datasets/Trajectories/pigeonflocks_trajectories/hf3_bind.csv')
hf3 <- read.csv('../Data/Datasets/Trajectories/pigeonflocks_trajectories/hf3_bind.csv')

head(hf3, 10)
hf3$sec <-  hf3$centisec/100
head(hf3)
data_hf3 <- set_data_format(
  raw_x = hf3$X,
  raw_y = hf3$Y,
  raw_t = hf3$sec,
  raw_id = hf3$id,
  flightid = hf3$flight,
  tz = "GMT"
)

summary(data_hf3)
head(data_hf3, 10)
unique(hf3$id)
plot(hf3$X,hf3$Y)

is_geo <- FALSE

data_dfs_hf3 <- add_velocities(data_hf3,
                               geo = is_geo,
                               verbose = TRUE,
                               parallelize = FALSE
)


head(data_dfs_hf3[[1]])
hist(data_dfs_hf3[[3]]$speed)
plot(data_dfs_hf3[[1]]$x, data_dfs_hf3[[1]]$y)

print(paste("Velocity information added for", length(data_dfs_hf3), "sets."))

hist(data_dfs_hf3[[1]]$speed)

sampling_timestep <- 1
time_window <- 10 # seconds
smoothing_time_window <- time_window / sampling_timestep

g_metr_hf3 <- group_metrics_per_set(
  data_list = data_dfs_hf3,
  mov_av_time_window = smoothing_time_window,
  step2time = sampling_timestep,
  geo = is_geo,
  parallelize = FALSE
)

head(g_metr_hf3)
summary(g_metr_hf3)

library(dplyr)
g_metr_hf3 %>% group_by(set) %>% summarise(n_timesteps = n(), N = mean(N)) #N; mean of N, splits data into set, summaries sets. n() number of elements. maxtime - meantime
g_metr_hf3 <- g_metr_hf3[g_metr_hf3$N > 1, ]
g_metr_hf3 <- g_metr_hf3[g_metr_hf3$speed > 5, ]
g_metr_hf3 <- g_metr_hf3[g_metr_hf3$speed < 50, ]
hist(g_metr_hf3$speed)
hist(g_metr_hf3$pol)

head(g_metr_hf3)
tail(g_metr_hf3)
data_dfs_hf3 <- data_dfs_hf3[unique(g_metr_hf3$set)]
head(data_dfs_hf3[1])

data_hf3 <- pairwise_metrics(data_list = data_dfs_hf3,
                             geo = is_geo,
                             verbose = TRUE,
                             parallelize = FALSE,
                             add_coords = FALSE # could be set to FALSE if the relative positions of neighbors are not needed
)

hist(g_metr_hf3$speed)
hist(g_metr_hf3$pol)
data_hf3 <- data_hf3[data_hf3$set]
head(g_metr_hf3)
tail(g_metr_hf3)
summary(g_metr_hf3)

hf3_metrics <- col_motion_metrics(
  data_hf3,
  global_metrics = g_metr_hf3,
  step2time = sampling_timestep,
  verbose = TRUE,
  speed_lim = 15,
  pol_lim = 0.95, noise_thresh = 5)

head(g_metr_hf3)
summary(hf3_metrics$event_dur)
hf3_metrics <- hf3_metrics[hf3_metrics$event_dur > 10, ]
summary(hf3_metrics)
nrow(hf3_metrics)

hf3_metrics$species <- "Columba livia (Nagy (hf3))"
write.csv(hf3_metrics, '../Data/Metrics/hf3_metrics.csv', row.names = F)

head(hf3_metrics)
################################################################################
#hf4----

hf4 <- rbind(hf4_A,hf4_B,hf4_C,hf4_D,hf4_G,hf4_H,hf4_I,hf4_J,hf4_L)
write.csv(hf4,'../Data/Datasets/Trajectories/pigeonflocks_trajectories/hf4_bind.csv')
hf4 <- read.csv('../Data/Datasets/Trajectories/pigeonflocks_trajectories/hf4_bind.csv')

head(hf4, 10)
hf4$sec <-  hf4$centisec/100
head(hf4)
data_hf4 <- set_data_format(
  raw_x = hf4$X,
  raw_y = hf4$Y,
  raw_t = hf4$sec,
  raw_id = hf4$id,
  flightid = hf4$flight,
  tz = "GMT"
)

summary(data_hf4)
head(data_hf4, 10)
unique(hf4$id)
plot(hf4$X,hf4$Y)

is_geo <- FALSE

data_dfs_hf4 <- add_velocities(data_hf4,
                               geo = is_geo,
                               verbose = TRUE,
                               parallelize = FALSE
)


head(data_dfs_hf4[[1]])
hist(data_dfs_hf4[[3]]$speed)
plot(data_dfs_hf4[[1]]$x, data_dfs_hf4[[1]]$y)

print(paste("Velocity information added for", length(data_dfs_hf4), "sets."))

hist(data_dfs_hf4[[1]]$speed)

sampling_timestep <- 1
time_window <- 10 # seconds
smoothing_time_window <- time_window / sampling_timestep

g_metr_hf4 <- group_metrics_per_set(
  data_list = data_dfs_hf4,
  mov_av_time_window = smoothing_time_window,
  step2time = sampling_timestep,
  geo = is_geo,
  parallelize = FALSE
)

head(g_metr_hf4)
summary(g_metr_hf4)

library(dplyr)
g_metr_hf4 %>% group_by(set) %>% summarise(n_timesteps = n(), N = mean(N)) #N; mean of N, splits data into set, summaries sets. n() number of elements. maxtime - meantime
g_metr_hf4 <- g_metr_hf4[g_metr_hf4$N > 1, ]
g_metr_hf4 <- g_metr_hf4[g_metr_hf4$speed > 5, ]
g_metr_hf4 <- g_metr_hf4[g_metr_hf4$speed < 50, ]
hist(g_metr_hf4$speed)
hist(g_metr_hf4$pol)

head(g_metr_hf4)
tail(g_metr_hf4)
data_dfs_hf4 <- data_dfs_hf4[unique(g_metr_hf4$set)]
head(data_dfs_hf4[1])

data_hf4 <- pairwise_metrics(data_list = data_dfs_hf4,
                             geo = is_geo,
                             verbose = TRUE,
                             parallelize = FALSE,
                             add_coords = FALSE # could be set to FALSE if the relative positions of neighbors are not needed
)

hist(g_metr_hf4$speed)
hist(g_metr_hf4$pol)
data_hf4 <- data_hf4[data_hf4$set]
head(g_metr_hf4)
tail(g_metr_hf4)
summary(g_metr_hf4)

hf4_metrics <- col_motion_metrics(
  data_hf4,
  global_metrics = g_metr_hf4,
  step2time = sampling_timestep,
  verbose = TRUE,
  speed_lim = 15,
  pol_lim = 0.95, noise_thresh = 5)

head(g_metr_hf4)
summary(hf4_metrics$event_dur)
hf4_metrics <- hf4_metrics[hf4_metrics$event_dur > 10, ]
summary(hf4_metrics)
nrow(hf4_metrics)

hf4_metrics$species <- "Columba livia (Nagy (hf4))"
write.csv(hf4_metrics, '../Data/Metrics/hf4_metrics.csv', row.names = F)

head(hf4_metrics)

#THE ENDING POINT OF EACH DATASET ANALYSIS FILE

#All dataframes done:
flockC_metrics <- read.csv('../Data/Metrics/flockC_metrics.csv')
flockP_metrics <- read.csv('../Data/Metrics/flockP_metrics.csv')
santos_metrics <- read.csv('../Data/Metrics/santos_metrics.csv')
hf1_metrics <- read.csv('../Data/Metrics/hf1_metrics.csv')
hf2_metrics <- read.csv('../Data/Metrics/hf2_metrics.csv')
hf3_metrics <- read.csv('../Data/Metrics/hf3_metrics.csv')
hf4_metrics <- read.csv('../Data/Metrics/hf4_metrics.csv')
#rbind----
hf_metrics <- rbind(hf1_metrics,hf2_metrics,hf3_metrics,hf4_metrics)
hf_metrics$species <- "Columbia livia (Nagy)"
all_data <- rbind(santos_metrics,flockC_metrics,flockP_metrics,hf_metrics)
write.csv(all_data, '../Data/Metrics/metrics_pigeons.csv', row.names = FALSE)
summary(all_data)
#PLOTS----
#PCA----
new_pca <- swaRmverse::swarm_space(metrics_data = all_data,
                       space_type = "pca"
)

ggplot2::ggplot(new_pca$swarm_space,
                ggplot2::aes(x = PC1, y = PC2, color = species)
) +
  ggplot2::geom_point(size = 3) +
  ggplot2::theme_bw()+
  scale_x_continuous(limits = c(-3,3))+
  scale_y_continuous(limits = c(-2,2))+
  theme(text = element_text(size = 20))

summary(new_pca$pca)
new_pca$pca$rotation
pcs_info <- new_pca$pca$rotation[, new_pca$pca$sdev > 1]

print(pcs_info)

summary(new_pca$pca)

#expanded PCA----
new_species_tobind <- all_data[,!colnames(all_data) %in% c('event_dur', 'N', 'set', 'start_time')]
all_data <- rbind(new_species_tobind,multi_species_metrics)

data("multi_species_pca_data")

new_pca_data <- expand_pca_swarm_space(metrics_data = new_species_tobind,
                                       pca_space = multi_species_pca)

expanded_pca <- rbind(multi_species_pca_data,
                      new_pca_data)

ggplot2::ggplot(expanded_pca,
                ggplot2::aes(x = PC1, y = PC2, color = species)) +
  ggplot2::geom_point(size = 3) +
  ggplot2::theme_bw()+
  scale_x_continuous(limits = c(-5,5))+
  scale_y_continuous(limits = c(-4,3))+
  theme(text = element_text(size = 20))




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

#References----

#1) Nagy, M., Ákos, Z., Biro, D., & Vicsek, T. (2010). Hierarchical Group Dynamics in Pigeon Flocks. Nature, 464(7290), 890–893. https://doi.org/10.1038/nature08891

#2) Papadopoulou, M., Garnier, S., King, A.J. (In prep.) swaRmverse: an R package for the comparative analysis of collective motion.

#3) Papadopoulou, M., Fürtbauer, I., O’Bryan, L. R., Garnier, S., Dimitra Georgopoulou, Bracken, A., Christensen, C., & King, A. J. (2023). Dynamics of Collective Motion Across Time and Species. Philosophical Transactions of the Royal Society B, 378(1874). https://doi.org/10.1098/rstb.2022.0068

#4) Papadopoulou, M., Hildenbrandt, H., Sankey, D. W. E., Portugal, S. J., & Hemelrijk, C. K. (2022). Self-organization of Collective Escape in Pigeon Flocks. PLOS Computational Biology, 18(1), e1009772. https://doi.org/10.1371/journal.pcbi.1009772

#5) Santos, CD., Neupert, S., Lipp, H., Wikelski, M., Dechmann, DKN. (2014). Data from: Temporal and contextual consistency of leadership in homing pigeon flocks. Movebank Data Repository. https://doi.org/10.5441/001/1.33159h1h


