#Tracks and Histograms

#spear bat----
ggplot(event1, aes(x = x, y = y, color = id)) +
  geom_point()+
  coord_equal()
start_time1 <- spearBat_metrics[spearBat_metrics$event == 90, 'start_time']
end_time1 <- start_time1 + spearBat_metrics[spearBat_metrics$event == 90, 'event_dur']
event1 <- data_spearBat[data_spearBat$t >= start_time1 & data_spearBat$t < end_time1,]
head(event1)

#fruitbat----
ggplot(event2, aes(x = x, y = y, color = id)) +
  geom_point()+
  coord_equal()
start_time2 <- fruitbat_metrics[fruitbat_metrics$event, 'start_time']
end_time2 <- start_time2 + fruitbat_metrics[fruitbat_metrics$event, 'event_dur']
event2 <- fruitBat_data[fruitBat_data$t >= start_time2 & fruitBat_data$t < end_time2,]
head(event2)

plot(y ~ x, data = fruitBat_data)

ggplot(fruitBat_data, aes(x = x, y = y, color = id)) +
  geom_point()+
  coord_equal()

#Gagliardo----

ggplot(event3, aes(x = x, y = y, color = id)) +
  geom_point()+
  coord_equal()
start_time3 <- gag_metrics[gag_metrics$event == 416, 'start_time']
end_time3 <- start_time3 + gag_metrics[gag_metrics$event == 416, 'event_dur']
event3 <- data_gag[data_gag$t >= start_time3 & data_gag$t < end_time3,]
head(event3)

#Santos----

ggplot(event4, aes(x = x, y = y, color = id)) +
  geom_point()+
  coord_equal()
start_time4 <- santos_metrics[santos_metrics$event == 3, 'start_time']
end_time4 <- start_time4 + santos_metrics[santos_metrics$event == 3, 'event_dur']
event4 <- data_santos[data_santos$t >= start_time4 & data_santos$t < end_time4,]
head(event4)


