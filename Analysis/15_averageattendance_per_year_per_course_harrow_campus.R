glimpse(studentData.clean)

averageattendance.per.year.by.course <-
  studentData.clean %>%
  filter(CAMPUSPOSTCODE == 'HA1 3TP') %>%
  group_by(NEWCOURSETITLE, YEAR) %>%
  summarise(average_attendance = mean(PERCENTAGEATTENDANCE)) %>%
  arrange(YEAR)

#Make plot
averageattendance.per.year.by.course.plot <-
  ggplot(averageattendance.per.year.by.course,
         aes(x = YEAR, y = average_attendance)) +
  geom_bar(stat="identity",
           position="identity",
           fill="#f28cab") +
  facet_wrap( ~ NEWCOURSETITLE, nrow = 8) +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  labs(title="Average attendance per year for courses at Harrow Campus",
       subtitle = "Average attendance of timetabled event recorded by Univeristy of Westminster by Course", x = "Year of study", y = "Average Attendance (%)")

averageattendance.per.year.by.course.plot

#save plot as png
save_plot(averageattendance.per.year.by.course.plot, 700, 840, "averageattendance_per_year_per_course_Harrow.png")

