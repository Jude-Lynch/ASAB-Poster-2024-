rm(list = ls())

#Ctrl + shift + B to build package
library(swaRmverse)

#Spear nosed bats----

spearBats <- read.csv("D:/SHOAL DATA/spear_bat_data.csv")

head(spearBats)

spearBats$time <- as.POSIXct(x = spearBats$time, format = '%d/%m/%Y %H:%M:%S', tz = 'America/Panama')

#formatting ----
data_spearBat <- swaRmverse::set_data_format(raw_x = spearBats$location.long,
                                       raw_y = spearBats$location.lat,
                                       raw_t = spearBats$time,
                                       raw_id = spearBats$individual.local.identifier,
                                       origin = min(spearBats$time),
                                       tz = 'America/Panama')

head(data_spearBat)

tail(data_spearBat)

library(swaRmverse)

is_geo <- TRUE

data_dfs <- add_velocities(data_spearBat,
                           geo = is_geo,
                           verbose = TRUE,
                           parallelize = FALSE
)
head(data_dfs[[1]])

print(paste("Velocity information added for", length(data_dfs), "sets."))

sampling_timestep <- 1

time_window <- 30 # seconds

smoothing_time_window <- time_window / sampling_timestep

library(swaRmverse)

g_metr <- group_metrics_per_set(data_list = data_dfs,
                                mov_av_time_window = smoothing_time_window,
                                step2time = sampling_timestep,
                                geo = is_geo,
                                parallelize = FALSE
)

head(g_metr)

#removal of NAs
g_metr <- g_metr[g_metr$N > 1, ]

data_spearBat <- data_spearBat[data_spearBat$t %in% g_metr$t, ]

head(g_metr)

library(swaRmverse)

data_spearBat <- pairwise_metrics(data_list = data_dfs,
                            geo = is_geo,
                            verbose = TRUE,
                            parallelize = FALSE,
                            add_coords = FALSE # could be set to FALSE if the relative positions of neighbors are not needed
)


head(data_spearBat)

tail(data_spearBat)

summary(data_spearBat)

summary(spearBat_metrics$event_dur)

#Bats----
library(swaRmverse)

#species metrics for spear bats.

spearBat_metrics <- col_motion_metrics_from_raw(data_spearBat,
                                             mov_av_time_window = 10,
                                             step2time = 1,
                                             geo = TRUE,
                                             verbose = FALSE,
                                             speed_lim = NA,
                                             pol_lim = NA,
                                             parallelize_all = FALSE
)
# Percentile: Speed = 40% (3.90), Pol = 40% (0.54). 1600 events, 457.683 mins.


head(spearBat_metrics)

summary(spearBat_metrics)

spearBat_metrics$species <- "Greater Spear Nosed Bat"

write.csv(spearBat_metrics, 'data/spearBat_metrics.csv', row.names = F)

#Fruit Bat----
library(swaRmverse)

fruitBat <- read.csv("D:/SHOAL DATA/Fruit Bats/ATLAS_RousettusAegyptiacus_HulaValley2015-2019.csv")

#From here, use 'Conversion' script to convert to lon-lat

fruitBat$time <- paste(fruitBat$Hour, fruitBat$Minute, fruitBat$Second, sep = ':')

fruitBat$date <- paste(fruitBat$Year, fruitBat$Month, fruitBat$Day, sep = '/')

fruitBat$timestamp <- paste(fruitBat$Date, fruitBat$Time, sep = ' ')

fruitBat$dateTime <- as.POSIXct(x = fruitBat$timestamp, format = '%d/%m/%Y %H:%M:%S', tz = 'Asia/Jerusalem')

library(swaRmverse)

fruitBat_data <- set_data_format(raw_x = fruitBat$GPS$lon,
                           raw_y = fruitBat$GPS$lat,
                           raw_t = fruitBat$dateTime,
                           raw_id = fruitBat$ID,
                           origin = min(fruitBat$dateTime),
                           tz = "Asia/Jerusalem"
)

head(fruitBat_data)

is_geo <- TRUE

data_dfs_fruit <- add_velocities(fruitBat_data,
                           geo = is_geo,
                           verbose = TRUE,
                           parallelize = FALSE
)
head(data_dfs_fruit[[1]])

print(paste("Velocity information added for", length(data_dfs), "sets."))

sampling_timestep <- 10

time_window <- 30 # seconds

smoothing_time_window <- time_window / sampling_timestep

g_metr_fruit <- group_metrics_per_set(data_list = data_dfs_fruit,
                                mov_av_time_window = smoothing_time_window,
                                step2time = sampling_timestep,
                                geo = is_geo,
                                parallelize = TRUE
)
head(g_metr_fruit)

hist(g_metr_fruit$pol)

g_metr_fruit <- g_metr_fruit[g_metr_fruit$N > 1, ]

head(g_metr_fruit)

library(swaRmverse)

data_fruitbat <- pairwise_metrics(data_list = data_dfs_fruit,
                                  geo = is_geo,
                                  verbose = TRUE,
                                  parallelize = TRUE,
                                  add_coords = TRUE # could be set to FALSE if the relative positions of neighbors are not needed
)

head(data_fruitbat)

tail(data_fruitbat)

hist(g_metr_fruit$pol)

hist(g_metr_fruit$speed)

#data_fruitbat <- data_fruitbat[data_fruitbat$nnd < , ]
library(swaRmverse)

fruitbat_metrics <- col_motion_metrics(data_fruitbat,
                                          global_metrics = g_metr_fruit,
                                          step2time = sampling_timestep,
                                          verbose = TRUE,
                                          speed_lim = NA,
                                          pol_lim = NA)

# Percentile: Speed = 70% (2.81), Pol = 75% (0.87). 5598 events over 1291.83 mins.


head(fruitbat_metrics)

fruitbat_metrics$species <- "Egyptian Fruit Bat"

write.csv(fruitbat_metrics, 'data/fruitbat_metrics.csv', row.names = F)


#Pigeons----

#santos----
library(swaRmverse)

library(dplyr)

santosPigeon <- read.csv("D:/SHOAL DATA/Pigeon Leadership (Santos)/Leadership in homing pigeon flocks (Columba livia) (data from Santos et al. 2014) (3).csv")

santosPigeon$dateTime <- as.POSIXct(santosPigeon$timestamp, format = '%d/%m/%Y %H:%M:%OS', tz = "Europe/Paris")

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

is_geo <- TRUE

data_dfs_santos <- add_velocities(data_santos,
                       geo = is_geo,
                       verbose = TRUE,
                       parallelize = FALSE
)
head(data_dfs_santos[[1]])

print(paste("Velocity information added for", length(data_dfs), "sets."))

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

tail(data_santos)

 santos_metrics <- col_motion_metrics(data_santos,
                                            global_metrics = g_metr_santos,
                                            step2time = sampling_timestep,
                                            verbose = TRUE,
                                            speed_lim = NA,
                                            pol_lim = NA)
# Percentile: Speed 20% (0.55), Pol 90% (0.97). 7179 events over 150.2 mins.

library(swaRmverse)

 santos_metrics$species <- "Columba livia (Santos)"

 ncol(santos_metrics)

 head(santos_metrics)

 write.csv(santos_metrics, 'data/santos_metrics.csv', row.names = F)
#gagliardo----

 gagPigeon <- read.csv("D:/SHOAL DATA/Pigeon (Gagliardo)/Use of visual familiar cues in anosmic pigeons.csv")

 gagPigeon$dateTime <- as.POSIXct(x = gagPigeon$timestamp, format = '%d/%m/%Y %H:%M:%S', tz = 'Europe/Rome')

 data_gag <- set_data_format(raw_x = gagPigeon$location.long,
                                raw_y = gagPigeon$location.lat,
                                raw_t = gagPigeon$dateTime,
                                raw_id = gagPigeon$individual.local.identifier,
                                origin = min(gagPigeon$dateTime),
                                tz = "Europe/Rome"
 )

 head(data_gag)

 is_geo <- TRUE

 data_dfs_gag <- add_velocities(data_gag,
                        geo = is_geo,
                        verbose = TRUE,
                        parallelize = FALSE
 )

 head(data_dfs_gag[[1]])

 print(paste("Velocity information added for", length(data_dfs), "sets."))

 sampling_timestep <- 1

 time_window <- 20 # seconds

 smoothing_time_window <- time_window / sampling_timestep

 g_metr_gag <- group_metrics_per_set(data_list = data_dfs_gag,
                                 mov_av_time_window = smoothing_time_window,
                                 step2time = sampling_timestep,
                                 geo = is_geo,
                                 parallelize = FALSE
 )

 head(g_metr_gag)

 g_metr_gag <- g_metr_gag[g_metr_gag$N > 1, ]

 data_gag <- data_gag[data_gag$t %in% g_metr_gag$t, ]

 hist(g_metr_gag$speed)

 hist(g_metr_gag$pol)

 length(unique(g_metr_gag$set))

 table(g_metr_gag$N)

install.packages('dplyr')

library(dplyr)

g_metr_gag %>% group_by(set) %>% summarise(n_timesteps = n(), N = mean(N)) #N; mean of N, splits data into set, summaries sets. n() number of elements. maxtime - meantime


 data_gag <- pairwise_metrics(data_list = data_dfs_gag,
                                 geo = is_geo,
                                 verbose = TRUE,
                                 parallelize = TRUE,
                                 add_coords = FALSE # could be set to FALSE if the relative positions of neighbors are not needed
 )

 head(data_gag)
 tail(data_gag)
 summary(data_gag)
 library(swaRmverse)

 gag_metrics <- col_motion_metrics(data_gag,
                                      global_metrics = g_metr_gag,
                                      step2time = sampling_timestep,
                                      verbose = TRUE,
                                      speed_lim = NA,
                                      pol_lim = NA)
# Percentile: Speed 55% (0.08), Pol 40% (0.97). 941 events over 190.65 mins.

ncol(gag_metrics)

summary(gag_metrics)

 nrow(gag_metrics)

 head(gag_metrics)

library(swaRmverse)

 gag_metrics$species <- "Columba livia (Gagliardo)"

 ncol(gag_metrics)

 head(gag_metrics)

 write.csv(gag_metrics, 'data/gag_metrics.csv', row.names = F)




#THE ENDING POINT OF EACH DATASET ANALYSIS FILE

spearBat_metrics$species <- "Greater Spear Nosed Bats"
santos_metrics$species <- "Pigeons (Santos)"
gag_metrics$species <- "Pigeons (Gagliardo)"
fruitbat_metrics <- "Egyptian Fruit Bats"

#All dataframes done:

  spearBat_metrics <- read.csv('data/spearBat_metrics.csv')
  gag_metrics <- read.csv('data/gag_metrics.csv')
  santos_metrics <- read.csv('data/santos_metrics.csv')
  fruitbat_metrics <- read.csv('data/fruitbat_metrics.csv')

#rbind----
ncol(fruitbat_metrics)
ncol(santos_metrics)
ncol(spearBat_metrics)
ncol(gag_metrics)
write.csv(all_data, 'data/metrics_batsvspigeons.csv', row.names = FALSE)

#PLOTS----

all_data <- rbind(gag_metrics, spearBat_metrics, santos_metrics, fruitbat_metrics)

#PCA----
new_pca <- swarm_space(metrics_data = all_data,
                       space_type = "pca"
)

ggplot2::ggplot(new_pca$swarm_space,
                ggplot2::aes(x = PC1, y = PC2, color = species)
) +
  ggplot2::geom_point() +
  ggplot2::theme_bw()

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







