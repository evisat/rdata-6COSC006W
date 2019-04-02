glimpse(studentData.clean)

average.attendance.per.course <- studentData.clean %>%
  group_by(NEWCOURSETITLE) %>%
  summarise(avg_attendance = mean(PERCENTAGEATTENDANCE)) %>%
  arrange(desc(avg_attendance))

averagemark.per.course <- studentData.clean %>%
  group_by(NEWCOURSETITLE) %>%
  summarise(avg_mark = mean(AVERAGEMODULEMARK)) %>% 
  arrange(desc(avg_mark))

#Using the top value from the results above to determine limits for graph
averagemark.by.attendance.per.course <-
  studentData.clean %>%
  filter(AVERAGEMODULEMARK <= 70 & PERCENTAGEATTENDANCE <= 70)

#Make plot
averagemark.by.attendance.per.course.plot <- 
  ggplot(averagemark.by.attendance.per.course, aes(x = PERCENTAGEATTENDANCE, y = AVERAGEMODULEMARK, colour = NEWCOURSETITLE)) +
  geom_point(size = 1, pch = 21, show.legend = FALSE, alpha = 0.7) +
  facet_wrap( ~ NEWCOURSETITLE, nrow = 10, labeller = labeller(NEWCOURSETITLE = label_wrap_gen(width = 20))) +
  geom_smooth(method=lm, colour="black", size=1) +
  theme(legend.position = 'none') +
  xlim (0, 70) +
  ylim (0, 70) +
  scale_color_viridis_d() +
  labs(title = "A scatterplot showing the relationship between average module attendance and average module mark",
       x="Average module attendance (%)", y="Average module mark (%)") +
  theme(plot.title = element_text(face = "bold", size = 20),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)),
        plot.margin=unit(c(1,1,1.5,1.2),"cm"))

#save plot as png
save_plot(averagemark.by.attendance.per.course.plot, 1200, 1300, "averagemark_by_attendance_per_course.png",
          "~/Desktop/Final Year Project/rdata-6COSC006W/Charts/")
