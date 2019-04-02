glimpse(studentData.clean)

averageattendance.per.year.by.course <-
  studentData.clean %>%
  filter(CAMPUSPOSTCODE == 'NW1 5LS') %>%
  group_by(NEWCOURSETITLE, YEAR) %>%
  summarise(average_attendance = mean(PERCENTAGEATTENDANCE)) %>%
  arrange(YEAR)

#Make plot
averageattendance.per.year.by.course.plot <-
  ggplot(averageattendance.per.year.by.course,
         aes(x = YEAR, y = average_attendance)) +
  geom_bar(stat="identity",
           position="identity",
           fill="#88965A") +
  facet_wrap( ~ NEWCOURSETITLE, nrow = 8) +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  labs(title="Average attendance per year for courses at Marylebone campus",
       x = "Year of study", y = "Average module attendance (%)") +
  theme(plot.title = element_text(face = "bold", size = 18),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)),
        plot.margin=unit(c(1,1,1.5,1.2),"cm"))

#save plot as png
save_plot(averageattendance.per.year.by.course.plot, 700, 840, "averageattendance_per_year_per_course_Marylebone.png",
          "~/Desktop/Final Year Project/rdata-6COSC006W/Charts/")

