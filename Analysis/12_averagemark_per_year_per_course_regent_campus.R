glimpse(studentData.clean)

averagemark.per.year.by.course <-
  studentData.clean %>%
  filter(CAMPUSPOSTCODE == 'W1B 2HW') %>%
  group_by(NEWCOURSETITLE, YEAR) %>%
  summarise(average_mark = mean(AVERAGEMODULEMARK)) %>%
  arrange(YEAR)

#Make plot
averagemark.per.year.by.course.plot <-
  ggplot(averagemark.per.year.by.course,
         aes(x = YEAR, y = average_mark)) +
  geom_bar(stat="identity",
           position="identity",
           fill="#f7b483") +
  facet_wrap( ~ NEWCOURSETITLE, nrow = 8) +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  labs(title="Average mark per year for courses at Regent campus",
       x = "Year of study", y = "Average module mark (%)") +
  theme(plot.title = element_text(face = "bold", size = 18),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)),
        plot.margin=unit(c(1,1,1.5,1.2),"cm"))

#save plot as png
save_plot(averagemark.per.year.by.course.plot, 700, 840, "averagemark_per_year_per_course_Regent.png",
          "~/Desktop/Final Year Project/rdata-6COSC006W/Charts/")

