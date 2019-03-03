glimpse(studentData.clean)

averagemark.per.year.by.course <-
  studentData.clean %>%
  group_by(NEWCOURSETITLE, YEAR) %>%
  summarise(average_mark = mean(AVERAGEMODULEMARK)) %>%
  arrange(YEAR)

averagemark.per.year.by.course <- averagemark.per.year.by.course[1:176,]

#Make plot
averagemark.per.year.by.course <-
  ggplot(averagemark.per.year.by.course,
         aes(x = YEAR, y = average_mark)) +
  geom_bar(stat="identity",
           position="identity",
           fill="#1380A1") +
  facet_wrap( ~ NEWCOURSETITLE, nrow = 8) +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  labs(title="Average mark per year",
       subtitle = "Average number of marks reported by Univeristy of Westmisnter by Course")

averagemark.per.year.by.course

#save plot as png
save_plot(averagemark.per.year.by.course, 800, 440, "averagemark_per_year_per_course.png")

