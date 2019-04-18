glimpse(studentData.clean)

barcols <- c("#FAFF70", "#39A9DB", "#2FBF71", "#DB504A")

#get top 10 on average mark for each course
averagemark.per.accom <- studentData.clean %>%
  filter(DEGREETYPE == "BMus" & STUDYLEVEL == "Level 4" & AVERAGEMODULEMARK >= 40) %>%
  group_by(ACCOMMODATIONTYPE) %>%
  summarise(total = n() / 56 * 100)


#Make a plot
averagemark.per.accom.plot <-
  ggplot(averagemark.per.accom,
         aes(x = reorder(ACCOMMODATIONTYPE, avg_mark), y = avg_mark, fill = ACCOMMODATIONTYPE)) +
  geom_bar(stat="identity",
           position="identity") +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  scale_fill_manual(values = barcols, name="Accommodation type") +
  labs(title="Average module mark by accommodation type", x="Accommodation type", y="Average module mark (%)") +
  theme(plot.title = element_text(face = "bold", size = 16),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)),
        plot.margin=unit(c(1,1,1.5,1.2),"cm"))

#save plot as png
save_plot(averagemark.per.accom.plot, 600, 400, "averagemark_per_accomm.png",
          "Charts/")
