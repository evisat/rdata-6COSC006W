# Source: https://github.com/dgrtwo/gganimate
install.packages("cowplot")  # a gganimate dependency

install.packages('devtools')
devtools::install_github("https://github.com/thomasp85/gganimate/releases/tag/v0.1.1")
devtools::install_github("thomasp85/gganimate")
install.packages("gapminder")

library(ggplot2)
library(stringr)
library(gganimate)
library(gapminder)

theme_set(theme_bw())  # pre-set the bw theme.

average.commutelengths.per.course <- studentData.clean %>%
  group_by(NEWCOURSETITLE) %>%
  summarise(avg_commutelength = mean(AVERAGEMODULEMARK)) %>%
  arrange(desc(avg_commutelength))

attendance.per.course <- studentData.clean %>%
  group_by(NEWCOURSETITLE) %>%
  summarise(avg_attendance = mean(PERCENTAGEATTENDANCE)) %>% 
  arrange(desc(avg_attendance))

#Using the top value from the results above to determine limits for graph
attendance.by.commutelength.per.course <-
  studentData.clean %>%
  filter(PERCENTAGEATTENDANCE <= 70 & AVERAGEMODULEMARK <= 70)

attendance.by.commutelength.per.course.plot <- 
ggplot(attendance.by.commutelength.per.course, aes(x = PERCENTAGEATTENDANCE, y = AVERAGEMODULEMARK, size="pop", colour="red")) +
  geom_point(alpha = 0.7, show.legend = FALSE) +
  scale_x_log10() +
  # Here comes the gganimate specific bits
  labs(title = 'COMMUTELENGTH: {frame_time}', x = 'Average Module Attendance (%)', y = 'Average Module Mark ()') +
  transition_time(COMMUTELENGTH) +
  ease_aes('linear')

animate(attendance.by.commutelength.per.course.plot, height = 800, width = 640)
anim_save("animation.gif", path = "~/Desktop/Final Year Project/rdata-6COSC006W/Output/")
