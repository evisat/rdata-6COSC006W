glimpse(studentData.clean)

average.commutelengths.per.course <- studentData.clean %>%
  group_by(NEWCOURSETITLE) %>%
  summarise(avg_commutelength = mean(COMMUTELENGTH)) %>%
  arrange(desc(avg_commutelength))

averageattendance.per.course <- studentData.clean %>%
  group_by(NEWCOURSETITLE) %>%
  summarise(avg_attendance = mean(PERCENTAGEATTENDANCE)) %>% 
  arrange(desc(avg_attendance))

#Using the top value from the results above to determine limits for graph
averageattendance.by.commutelength.per.course <-
  studentData.clean %>%
  filter(PERCENTAGEATTENDANCE <= 70 & COMMUTELENGTH <= 20)

averageattendance.by.commutelength.per.course

#Make plot
averageattendance.by.commutelength.per.course.plot <- 
  ggplot(averageattendance.by.commutelength.per.course, aes(x = COMMUTELENGTH, y = PERCENTAGEATTENDANCE, colour = NEWCOURSETITLE)) +
  geom_point(size = 1, pch = 21, show.legend = FALSE, alpha = 0.7) +
  facet_wrap( ~ NEWCOURSETITLE, nrow = 10, labeller = labeller(NEWCOURSETITLE = label_wrap_gen(width = 20))) +
  geom_smooth(method=lm, colour="black", size=1) +
  theme(legend.position = 'none') +
  xlim (0, 20) +
  ylim (0, 70) +
  scale_color_viridis_d() +
  labs(title = "A scatterplot showing the relationship between commute length and average percentage attendance",
       subtitle = "")

averageattendance.by.commutelength.per.course.plot

#save plot as png
save_plot(averageattendance.by.commutelength.per.course.plot, 1000, 940, "averageattendance_by_commutelength_per_course.png")
