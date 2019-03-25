glimpse(studentData.clean)

#get top 10 on average commute length for each course
averagecommute.per.course <- studentData.clean %>%
  group_by(NEWCOURSETITLE) %>%
  summarise(avg_commute = mean(COMMUTELENGTH)) %>% 
  arrange(desc(avg_commute)) %>%
  top_n(10, avg_commute)

#Make a plot
averagecommute.per.course.plot <-
  ggplot(averagecommute.per.course,
         aes(x = reorder(NEWCOURSETITLE, avg_commute), y = avg_commute)) +
  geom_bar(stat="identity",
           position="identity",
           fill="#1380A1") +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  labs(title="Top 10 Average Commute Length Per Course", x="Course Name", y="Average Commute Length (geodesic miles)")

averagecommute.per.course.plot + coord_flip()

#save plot as png
save_plot(averagecommute.per.course.plot + coord_flip(), 800, 440, "top10_averagecommute_per_course.png")


#get bottom 10 on average commute length for each course
averagecommute.per.course <- studentData.clean %>%
  group_by(NEWCOURSETITLE) %>%
  summarise(avg_commute = mean(COMMUTELENGTH)) %>% 
  arrange(desc(avg_commute)) %>%
  top_n(-10, avg_commute)

#Make a plot
averagecommute.per.course.plot <-
  ggplot(averagecommute.per.course,
         aes(x = reorder(NEWCOURSETITLE, avg_commute), y = avg_commute)) +
  geom_bar(stat="identity",
           position="identity",
           fill="#a2134a") +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  labs(title="Bottom 10 Average Commute Length Per Course", x="Course Name", y="Average Commute Length (geodesic miles)")

averagecommute.per.course.plot + coord_flip()

#save plot as png
save_plot(averagecommute.per.course.plot + coord_flip(), 800, 440, "bottom10_averagecommute_per_course.png")
