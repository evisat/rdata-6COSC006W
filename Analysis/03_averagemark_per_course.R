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
           fill="#717EC3") +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  labs(title="Top 10 average module mark per course", x="Course name", y="Average module mark (%)") +
  theme(plot.title = element_text(face = "bold", size = 16),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)),
        plot.margin=unit(c(1,1,1.5,1.2),"cm"))

#save plot as png
save_plot(averagemark.per.course.plot + coord_flip(), 800, 440, "top10_averagemark_per_course.png",
          "~/Desktop/Final Year Project/rdata-6COSC006W/Charts/")


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
           fill="#717EC3") +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  labs(title="Bottom 10 average module mark per course", x="Course name", y="Average module mark (%)") +
  theme(plot.title = element_text(face = "bold", size = 16),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)),
        plot.margin=unit(c(1,1,1.5,1.2),"cm"))

#save plot as png
save_plot(averagemark.per.course.plot + coord_flip(), 800, 440, "bottom10_averagemark_per_course.png",
          "~/Desktop/Final Year Project/rdata-6COSC006W/Charts/")
