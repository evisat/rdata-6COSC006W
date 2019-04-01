library(scales)

#add column to represent the degree classification e.g. 1st, 2:i, 2:ii, 3rd - ignoring marks under 40 
studentData.clean.class <- studentData.clean %>%
  filter((AVERAGEMODULEMARK >= 40)) %>%
  mutate(DEGREECLASS = ifelse(AVERAGEMODULEMARK >= 70, '1st', 
                              ifelse(AVERAGEMODULEMARK >= 60 & AVERAGEMODULEMARK < 70, '2:i',
                                     ifelse(AVERAGEMODULEMARK >= 50 & AVERAGEMODULEMARK < 60, '2:ii', '3rd'))))

pie.plot.data <- studentData.clean.class %>%
  group_by(DEGREETYPE, DEGREECLASS) %>%
  summarize(COUNT = sum(AVERAGEMODULEMARK >= 40)) %>%
  arrange(desc(DEGREECLASS)) 

pie.plot.data <- pie.plot.data %>%
  mutate(perc = round(COUNT/sum(pie.plot.data[which(pie.plot.data$DEGREETYPE == DEGREETYPE),"COUNT"]) * 100, digits = 2), lab.ypos = round(cumsum(perc) - 0.5*perc, digits = 2))

mycols <- c("#EDAE49", "#D1495B", "#00798C", "#424B54")

pie.plot <-
  ggplot(pie.plot.data, aes(x = 2, y = perc, fill = DEGREECLASS)) +
  geom_bar(stat = "identity", color = "white") +
  coord_polar(theta = "y", start = 0) +
  geom_text(aes(y = lab.ypos, label = paste(perc, "%", sep="")), color = "white") +
  scale_fill_manual(values = mycols, name="Degree Classification") +
  ggtitle("Average Classifications of students at the University of Westminster per Degree Type") +
  theme_void() +
  theme( plot.title = element_text(size = 27, margin = margin(t = 0, r = 0, b = 30, l = 0, unit = "pt")),
         legend.title = element_text(size = 20),
         legend.text = element_text(size = 16, margin = margin(t = 0, r = 0, b = 5, l = 0, unit = "pt")),
         strip.text = element_text(size = 20, margin = margin(b=5)),
         plot.margin=unit(c(1,1,1.5,1.2),"cm")) +
  facet_wrap(~ DEGREETYPE) +
  xlim(0.5, 2.5)

#save plot as png
save_plot(pie.plot, 1500, 940, "avg_class_per_degreeType_pie_chart.png")
