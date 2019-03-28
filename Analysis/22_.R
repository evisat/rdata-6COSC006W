total.averagemarks.by.classification.per.degreetype <- studentData.clean %>%
  filter((STUDYLEVEL == 'Level 6') & (AVERAGEMODULEMARK >= 40)) %>%
  group_by(DEGREETYPE) %>%
  summarize('1st' = sum(AVERAGEMODULEMARK >= 70),
            '2:i' = sum(AVERAGEMODULEMARK >= 60 & AVERAGEMODULEMARK < 70),
            '2:ii' = sum(AVERAGEMODULEMARK >= 50 & AVERAGEMODULEMARK < 60),
            '3rd' = sum(AVERAGEMODULEMARK < 50))

total.averagemarks.by.classification.per.degreetype <- t(total.averagemarks.by.classification.per.degreetype)


##### BA
totalBA <- total_averagemarks_by_classification_per_degreetype %>%
  filter(DEGREETYPE == 'BA')

# reverse the order of the columns and rows
reverseBA <- data.frame(t(totalBA[-1]))
colnames(reverseBA) <- totalBA[, 1]

#Make plot
blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.title = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    plot.title=element_text(size=14, face="bold")
  )

library(scales)

pie <-
  ggplot(reverseBA,
         aes(x = "", y = BA, fill = rownames(reverseBA))) +
  coord_polar(theta = "y") +
  theme(axis.text.x=element_blank()) +
  blank_theme +
  geom_bar(stat="identity",
           width = 1)

pie

pie + scale_fill_brewer("Blues") + blank_theme +
  theme(axis.text.x=element_blank())+
  geom_text(aes(y = value/3 + c(0, cumsum(value)[-length(value)]), 
                label =  percent(value/100)), size=5)



#save plot as png
save_plot(averagemark.per.year.by.course.plot, 700, 840, "averagemark_per_year_per_course_Harrow.png")
