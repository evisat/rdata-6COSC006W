install.packages(ggthemes)


Commutelength = 331.65

averagemark.per.course <- studentData.clean %>%
  group_by(NEWCOURSETITLE, COMMUTELENGTH) %>%
  filter(AVERAGEMODULEMARK <= 70 & PERCENTAGEATTENDANCE <= 70)

p <- ggplot(
  studentData.clean, 
  aes(x = PERCENTAGEATTENDANCE, y=AVERAGEMODULEMARK, colour = NEWCOURSETITLE)) +
  geom_point(size = 1, pch = 21, show.legend = FALSE, alpha = 0.7) +
  facet_wrap( ~NEWCOURSETITLE, nrow = 8) +
  geom_smooth(method=lm) +
  xlim (-1, 70) +
  ylim (-5, 70) +
  scale_color_viridis_d() +
  labs(x = "Percentage Attendance", y = "Average Module Mark")
p

results.complete <- d %>%
  left_join(averagemark.per.course)


averagemark.per.course.plot <-ggplot(averagemark.per.course,
          aes(ordering, group = NEWCOURSETITLE,color=NEWCOURSETITLE,fill=NEWCOURSETITLE)) +
  geom_tile(aes(y = AVERAGEMODULEMARK/2, 
                height = AVERAGEMODULEMARK,
                width = 0.9), alpha = 0.9) +
  # text on top of bars
  geom_text(aes(y = AVERAGEMODULEMARK, label = NEWCOURSETITLE), hjust = -0.4) +
  # text in x-axis (requires clip = "off" in coord_cartesian)
  geom_text(aes(y = 0, label = NEWCOURSETITLE), hjust = 1.4) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_color_viridis_d(name="")+
  scale_fill_viridis_d(name="")+
  guides(color=F,fill=F)+
  labs(title='{frame_time}', x = "",y="Average Mark") +
  theme(plot.title = element_text(hjust = 1, size = 22),
        axis.ticks.y = element_blank(),
        axis.text.y  = element_blank()) + 
  transition_time(COMMUTELENGTH)+
  ease_aes('cubic-in-out')

animate(averagemark.per.course.plot, nframes = 100, fps = 5, end_pause = 20) #again, use anim_save(filename) to save

animate(averagemark.per.course.plot, height = 800, width = 640)
anim_save("animation.gif", path = "~/Desktop/Final Year Project/rdata-6COSC006W/Output/")
