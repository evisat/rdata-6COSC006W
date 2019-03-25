glimpse(studentData.clean)

#get top 10 on average mark for each course
averagemark.per.course <- studentData.clean %>%
  group_by(NEWCOURSETITLE) %>%
  summarise(avg_mark = mean(AVERAGEMODULEMARK)) %>% 
  arrange(desc(avg_mark)) %>%
  top_n(10, avg_mark)

#Make a plot
averagemark.per.course.plot <-
  ggplot(averagemark.per.course,
       aes(x = reorder(NEWCOURSETITLE, avg_mark), y = avg_mark)) +
  geom_bar(stat="identity",
           position="identity",
           fill="#1380A1") +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  labs(title="Top 10 Average Module Mark Per Course", x="Course Name", y="Average Mark")

averagemark.per.course.plot + coord_flip()

#save plot as png
save_plot(averagemark.per.course.plot + coord_flip(), 800, 440, "top10_averagemark_per_course.png")


#get bottom 10 on average mark for each course
averagemark.per.course <- studentData.clean %>%
  group_by(NEWCOURSETITLE) %>%
  summarise(avg_mark = mean(AVERAGEMODULEMARK)) %>% 
  arrange(desc(avg_mark)) %>%
  top_n(-10, avg_mark)

#Make a plot
averagemark.per.course.plot <-
  ggplot(averagemark.per.course,
         aes(x = reorder(NEWCOURSETITLE, avg_mark), y = avg_mark)) +
  geom_bar(stat="identity",
           position="identity",
           fill="#a2134a") +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  labs(title="Bottom 10 Average Module Mark Per Course", x="Course Name", y="Average Mark")

averagemark.per.course.plot + coord_flip()

#save plot as png
save_plot(averagemark.per.course.plot + coord_flip(), 800, 440, "bottom10_averagemark_per_course.png")
