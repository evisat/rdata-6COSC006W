glimpse(studentData.clean)

averagemark.per.year.by.course <-
  studentData.clean %>%
  filter(CAMPUSPOSTCODE == 'NW1 5LS') %>%
  group_by(NEWCOURSETITLE, YEAR) %>%
  summarise(average_mark = mean(AVERAGEMODULEMARK)) %>%
  arrange(YEAR)

#Make plot
averagemark.per.year.by.course.plot <-
  ggplot(averagemark.per.year.by.course,
         aes(x = YEAR, y = average_mark)) +
  geom_bar(stat="identity",
           position="identity",
           fill="#88965A") +
  facet_wrap( ~ NEWCOURSETITLE, nrow = 8) +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  labs(title="Average mark per year for courses at Marylebone Campus",
       subtitle = "Average number of marks reported by Univeristy of Westminster by Course", x = "Year of study", y = "Average Module Mark (%)")

averagemark.per.year.by.course.plot

#save plot as png
save_plot(averagemark.per.year.by.course.plot, 700, 840, "averagemark_per_year_per_course_Marylebone.png")

