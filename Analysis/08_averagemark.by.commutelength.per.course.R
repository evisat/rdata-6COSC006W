glimpse(studentData.clean)

#Make plot
averagemark.by.commutelength.per.course.plot <- 
  ggplot(studentData.clean, aes(x = COMMUTELENGTH, y = AVERAGEMODULEMARK)) +
  geom_point(size = 1, pch = 21, colour = "black") +
  facet_wrap( ~ NEWCOURSETITLE, nrow = 8) +
  geom_smooth(method=lm) +
  xlim (-1, 101) +
  ylim (-5, 101) +
  labs(title = "A scatterplot showing the relationship between commute length and average module mark",
       subtitle = "")

averagemark.by.commutelength.per.course.plot

#save plot as png
save_plot(averagemark.by.commutelength.per.course.plot, 800, 640, "averagemark_by_commutelength_per_course.png")
