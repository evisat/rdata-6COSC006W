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
  ggplot(averagemark.by.commutelength.per.course, aes(x = COMMUTELENGTH, y = AVERAGEMODULEMARK, colour = NEWCOURSETITLE)) +
  geom_point(size = 1, pch = 21, show.legend = FALSE, alpha = 0.7) +
  facet_wrap( ~ NEWCOURSETITLE, nrow = 10, labeller = labeller(NEWCOURSETITLE = label_wrap_gen(width = 20))) +
  geom_smooth(method=lm, colour="black", size=1) +
  theme(legend.position = 'none') +
  xlim (-1, 20) +
  ylim (-5, 70) +
  scale_color_viridis_d() +
  labs(title = "A scatterplot showing the relationship between commute length and average module mark",
       subtitle = "")

averagemark.by.commutelength.per.course.plot

#save plot as png
save_plot(averagemark.by.commutelength.per.course.plot, 1000, 940, "averagemark_by_commutelength_per_course.png")
