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
           fill="#247BA0") +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  labs(title="Top 10 average attendance per course", x="Course name", y="Average module attendance (%)") +
  theme(plot.title = element_text(face = "bold", size = 16),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)),
        plot.margin=unit(c(1,1,1.5,1.2),"cm"))

#save plot as png
save_plot(averageattendance.per.course.plot + coord_flip(), 800, 440, "top10_averageattendance_per_course.png",
          "~/Desktop/Final Year Project/rdata-6COSC006W/Charts/")


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
           fill="#247BA0") +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  labs(title="Bottom 10 Average attendance per course", x="Course name", y="Average module attendance (%)") +
  theme(plot.title = element_text(face = "bold", size = 16),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)),
        plot.margin=unit(c(1,1,1.5,1.2),"cm"))

#save plot as png
save_plot(averageattendance.per.course.plot + coord_flip(), 800, 440, "bottom10_averageattendance_per_course.png",
          "~/Desktop/Final Year Project/rdata-6COSC006W/Charts/")

