# Source: https://github.com/dgrtwo/gganimate
install.packages("cowplot")  # a gganimate dependency

# install.packages('devtools')
devtools::install_github("thomasp85/gganimate")
install.packages("gapminder")

library(gganimate)
library(gapminder)

theme_set(theme_bw())  # pre-set the bw theme.

average.commutelengths.per.course <- studentData.clean %>%
  group_by(NEWCOURSETITLE) %>%
  summarise(avg_moduleMark = mean(AVERAGEMODULEMARK)) %>%
  arrange(desc(avg_moduleMark))

attendance.per.course <- studentData.clean %>%
  group_by(NEWCOURSETITLE) %>%
  summarise(avg_attendance = mean(PERCENTAGEATTENDANCE)) %>% 
  arrange(desc(avg_attendance))

#Using the top value from the results above to determine limits for graph
attendance.by.commutelength.per.course <-
  studentData.clean %>%
  filter(PERCENTAGEATTENDANCE <= 70 & AVERAGEMODULEMARK <= 70)

attendance.by.commutelength.per.course.plot <- 
ggplot(attendance.by.commutelength.per.course, aes(x = PERCENTAGEATTENDANCE, 
                                                   y = AVERAGEMODULEMARK, colour=NEWCOURSETITLE)) +
  geom_point(size = 3, alpha = 0.7, show.legend = FALSE) +
  # shadow_mark(alpha = 0.2, size = 3) +
  scale_color_viridis_d() +
  labs(title = "A scatterplot showing the relationship between average \n module mark by average attendance per commute length", 
       subtitle = 'Commute Length (Geodesic distance in miles): {frame_time}', 
       x = 'Average module attendance (%)', y = 'Average module mark (%)') +
  theme(plot.title = element_text(face = "bold", size = 20),
        plot.subtitle = element_text(size = 12),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)),
        plot.margin=unit(c(1,1,1.5,1.2),"cm")) +
  xlim (0, 70) +
  ylim (0, 70) +
  transition_time(COMMUTELENGTH) +
  ease_aes('linear')

animate(attendance.by.commutelength.per.course.plot, height = 800, width = 640)
anim_save("attendance_by_commutelength_per_course_plot_2.gif", path = "~/Desktop/Final Year Project/rdata-6COSC006W/Charts/")
