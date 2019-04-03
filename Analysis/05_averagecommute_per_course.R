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
           fill="#a2134a") +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  labs(title="Top 10 average commute length per course", x="Course name", y="Average commute length (Geodesic distance in miles)") +
  theme(plot.title = element_text(face = "bold", size = 16),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)),
        plot.margin=unit(c(1,1,1.5,1.2),"cm"))

#save plot as png
save_plot(averagecommute.per.course.plot + coord_flip(), 800, 440, "top10_averagecommute_per_course.png",
          "Charts/")


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
  labs(title="Bottom 10 average commute length per course", x="Course name", y="Average commute length (Geodesic distance in miles)") +
  theme(plot.title = element_text(face = "bold", size = 16),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)),
        plot.margin=unit(c(1,1,1.5,1.2),"cm"))

#save plot as png
save_plot(averagecommute.per.course.plot + coord_flip(), 800, 440, "bottom10_averagecommute_per_course.png",
          "Charts/")
