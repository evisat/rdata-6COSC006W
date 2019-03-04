# Source: https://github.com/dgrtwo/gganimate
install.packages("cowplot")  # a gganimate dependency

install.packages('devtools')
devtools::install_github("https://github.com/thomasp85/gganimate/releases/tag/v0.1.1")


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

ggplot(attendance.by.commutelength.per.course, aes(x = PERCENTAGEATTENDANCE, y = AVERAGEMODULEMARK, size="pop", colour="red")) +
  geom_point(alpha = 0.7, show.legend = FALSE) +
  scale_x_log10() +
  # Here comes the gganimate specific bits
  labs(title = 'COMMUTELENGTH: {frame_time}', x = 'Average Module Attendance (%)', y = 'Average Module Mark ()') +
  transition_time(COMMUTELENGTH) +
  ease_aes('linear')

averagemark.by.attendance.per.course.plot <- 
  ggplot(attendance.by.commutelength.per.course, aes(x = PERCENTAGEATTENDANCE, y = AVERAGEMODULEMARK)) +
  geom_point(size = 1, pch = 21, colour = "black") +
  facet_wrap( ~ NEWCOURSETITLE, nrow = 8) +
  geom_smooth(method=lm) +
  xlim (-1, 70) +
  ylim (-5, 70) +
  labs(title = "A scatterplot showing the relationship between attendance and average module mark",
       subtitle = "")



ggplot(attendance.by.commutelength.per.course, aes(COMMUTELENGTH, PERCENTAGEATTENDANCE)) +
  geom_point() +
  geom_smooth(method = "lm", 
              show.legend = FALSE) +
  scale_x_log10()  # convert to log scale

gganimate(g, interval=0.2)

averagemark.by.commutelength.per.course

#Make plot
averagemark.by.commutelength.per.course.plot <- 
  ggplot(averagemark.by.commutelength.per.course, aes(x = COMMUTELENGTH, y = AVERAGEMODULEMARK)) +
  geom_point(size = 1, pch = 21, colour = "black") +
  facet_wrap( ~ NEWCOURSETITLE, nrow = 8) +
  geom_smooth(method=lm) +
  xlim (-1, 20) +
  ylim (-5, 70) +
  labs(title = "A scatterplot showing the relationship between commute length and average module mark",
       subtitle = "")

averagemark.by.commutelength.per.course.plot

#save plot as png
save_plot(averagemark.by.commutelength.per.course.plot, 1000, 640, "averagemark_by_commutelength_per_course.png")
