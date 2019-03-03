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

averagemark.by.attendance.per.course

#Make plot
averagemark.by.attendance.per.course.plot <- 
  ggplot(averagemark.by.attendance.per.course, aes(x = PERCENTAGEATTENDANCE, y = AVERAGEMODULEMARK)) +
  geom_point(size = 1, pch = 21, colour = "black") +
  facet_wrap( ~ NEWCOURSETITLE, nrow = 8) +
  geom_smooth(method=lm) +
  xlim (-1, 70) +
  ylim (-5, 70) +
  labs(title = "A scatterplot showing the relationship between attendance and average module mark",
       subtitle = "")

averagemark.by.attendance.per.course.plot

#save plot as png
save_plot(averagemark.by.attendance.per.course.plot, 1000, 640, "averagemark_by_attendance_per_course.png")
