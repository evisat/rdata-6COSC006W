# library(scales)
# mycols <- c("#EDAE49", "#D1495B", "#00798C", "#424B54")

pie.plot.data <- studentData.clean %>%
  filter(DEGREETYPE == 'BA') %>%
  group_by(STUDYLEVEL, DEGREECLASS) %>%
  summarize(COUNT = sum(AVERAGEMODULEMARK >= 40)) %>%
  arrange(desc(DEGREECLASS)) 

pie.plot.data <- pie.plot.data %>%
  mutate(perc = round(COUNT/sum(pie.plot.data[which(pie.plot.data$STUDYLEVEL == STUDYLEVEL),"COUNT"]) * 100, digits = 2), lab.ypos = round(cumsum(perc) - 0.5*perc, digits = 2))

pie.plot <-
  ggplot(pie.plot.data, aes(x = 2, y = perc, fill = DEGREECLASS)) +
  geom_bar(stat = "identity", color = "white") +
  coord_polar(theta = "y", start = 0) +
  geom_text(aes(y = lab.ypos, label = paste(perc, "%", sep="")), color = "white") +
  scale_fill_manual(values = mycols, name="Degree classification") +
  ggtitle("Average classifications of BA students per level of study") +
  theme_void() +
  theme( plot.title = element_text(face = "bold", size = 27, margin = margin(t = 0, r = 0, b = 30, l = 0, unit = "pt")),
         legend.title = element_text(size = 20),
         legend.text = element_text(size = 16, margin = margin(t = 0, r = 0, b = 5, l = 0, unit = "pt")),
         strip.text = element_text(size = 20, margin = margin(b=5)),
         plot.margin=unit(c(1,1,1.5,1.2),"cm")) +
  facet_wrap(~ STUDYLEVEL) +
  xlim(0.5, 2.5)

#save plot as png
save_plot(pie.plot, 1500, 940, "BA_avg_class_per_level_pie_chart.png",
          "~/Desktop/Final Year Project/rdata-6COSC006W/Charts/")
