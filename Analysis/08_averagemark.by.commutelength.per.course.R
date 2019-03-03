glimpse(studentData.clean)

average.commutelengths.per.course <- studentData.clean %>%
  group_by(NEWCOURSETITLE) %>%
  summarise(avg_commutelength = mean(COMMUTELENGTH)) %>%
  arrange(desc(avg_commutelength))

averagemark.per.course <- studentData.clean %>%
  group_by(NEWCOURSETITLE) %>%
  summarise(avg_mark = mean(AVERAGEMODULEMARK)) %>% 
  arrange(desc(avg_mark))

#Using the top value from the results above to determine limits for graph
averagemark.by.commutelength.per.course <-
  studentData.clean %>%
  filter(AVERAGEMODULEMARK <= 70 & COMMUTELENGTH <= 20)

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
