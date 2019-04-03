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
  labs(title = "A scatterplot showing the relationship between commute length and average module attendance",
       x="Average commute length (Geodesic distance in miles)", y="Average module attendance (%)") +
  theme(plot.title = element_text(face = "bold", size = 20),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)),
        plot.margin=unit(c(1,1,1.5,1.2),"cm"))

#save plot as png
save_plot(averageattendance.by.commutelength.per.course.plot, 1200, 1300, "averageattendance_by_commutelength_per_course.png",
          "Charts/")
