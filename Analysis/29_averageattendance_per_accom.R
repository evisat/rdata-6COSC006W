glimpse(studentData.clean)

# barcols <- c("#DB504A", "#39A9DB", "#2FBF71")

#get top 10 on average mark for each course
averageattendance.per.accom <- studentData.clean %>%
  filter(ACCOMMODATIONTYPE != "Unknown") %>%
  group_by(ACCOMMODATIONTYPE) %>%
  summarise(avg_attendance = mean(PERCENTAGEATTENDANCE))

#Make a plot
averageattendance.per.accom.plot <-
  ggplot(averageattendance.per.accom,
         aes(x = reorder(ACCOMMODATIONTYPE, avg_attendance), y = avg_attendance, fill = ACCOMMODATIONTYPE)) +
  geom_bar(stat="identity",
           position="identity") +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  scale_fill_manual(values = barcols, name="Accommodation type") +
  labs(title="Average module attendance by accommodation type", x="Accommodation type", y="Average module attendance (%)") +
  theme(plot.title = element_text(face = "bold", size = 16),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)),
        plot.margin=unit(c(1,1,1.5,1.2),"cm"))

#save plot as png
save_plot(averageattendance.per.accom.plot, 600, 400, "averageattendance_per_accomm.png",
          "Charts/")
