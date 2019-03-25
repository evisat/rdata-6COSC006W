glimpse(studentData.clean)

#get top 10 on average attendance for each course
averageattendance.per.course <- studentData.clean %>%
  group_by(NEWCOURSETITLE) %>%
  summarise(avg_attendance = mean(PERCENTAGEATTENDANCE)) %>% 
  arrange(desc(avg_attendance)) %>%
  top_n(10, avg_attendance)

#Make a plot
averageattendance.per.course.plot <-
  ggplot(averageattendance.per.course,
         aes(x = reorder(NEWCOURSETITLE, avg_attendance), y = avg_attendance)) +
  geom_bar(stat="identity",
           position="identity",
           fill="#1380A1") +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  labs(title="Top 10 Average Attendance Per Course", x="Course Name", y="Average Attendance (%)")

averageattendance.per.course.plot + coord_flip()

#save plot as png
save_plot(averageattendance.per.course.plot + coord_flip(), 800, 440, "top10_averageattendance_per_course.png")


#get bottom 10 on average attendance for each course
averageattendance.per.course <- studentData.clean %>%
  group_by(NEWCOURSETITLE) %>%
  summarise(avg_attendance = mean(PERCENTAGEATTENDANCE)) %>% 
  arrange(desc(avg_attendance)) %>%
  top_n(-10, avg_attendance)

#Make a plot
averageattendance.per.course.plot <-
  ggplot(averageattendance.per.course,
         aes(x = reorder(NEWCOURSETITLE, avg_attendance), y = avg_attendance)) +
  geom_bar(stat="identity",
           position="identity",
           fill="#a2134a") +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  labs(title="Bottom 10 Average Attendance Per Course", x="Course Name", y="Average Attendance (%)")

averageattendance.per.course.plot + coord_flip()

#save plot as png
save_plot(averageattendance.per.course.plot + coord_flip(), 800, 440, "bottom10_averageattendance_per_course.png")

