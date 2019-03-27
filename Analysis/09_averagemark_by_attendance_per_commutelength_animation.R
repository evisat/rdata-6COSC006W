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
ggplot(attendance.by.commutelength.per.course, aes(x = PERCENTAGEATTENDANCE, y = AVERAGEMODULEMARK, colour=NEWCOURSETITLE)) +
  geom_point(size = 3, alpha = 0.7, show.legend = FALSE) +
  # shadow_mark(alpha = 0.2, size = 3) +
  scale_color_viridis_d() +
  xlim (0, 70) +
  ylim (0, 70) +
  labs(title = "Scatterplot showing average module mark by average attendance per commute length", subtitle = 'Commute Length (Geodesic miles): {frame_time}', x = 'Average Module Attendance (%)', y = 'Average Module Mark (%)') +
  transition_time(COMMUTELENGTH) +
  ease_aes('linear')

animate(attendance.by.commutelength.per.course.plot, height = 800, width = 640)
anim_save("attendance_by_commutelength_per_course_plot_2.gif", path = "~/Desktop/Final Year Project/rdata-6COSC006W/Output/")
