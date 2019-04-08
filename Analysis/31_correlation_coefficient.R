sd(studentData.clean$COMMUTELENGTH) / sqrt(length(studentData.clean$COMMUTELENGTH))

lmMark = lm(AVERAGEMODULEMARK ~ PERCENTAGEATTENDANCE, data = studentData.clean)
summary(lmMark)
plot(lmMark$residuals, pch = 16, col = "red")

cor.test(studentData.clean$AVERAGEMODULEMARK, studentData.clean$PERCENTAGEATTENDANCE, method = "pearson")
cor.test(studentData.clean$AVERAGEMODULEMARK,studentData.clean$PERCENTAGEATTENDANCE, method = "spearman", exact=F)

averagemark.by.attendance.plot <- 
  ggplot(studentData.clean, aes(x = PERCENTAGEATTENDANCE, y = AVERAGEMODULEMARK, colour = NEWCOURSETITLE)) +
  geom_point(size = 1, pch = 21, show.legend = FALSE, alpha = 0.7) +
  geom_smooth(method=lm, colour="black", size=1) +
  theme(legend.position = 'none') +
  geom_smooth(method=lm, colour="black", size=1) +
  scale_color_viridis_d() +
  labs(title = "A scatterplot showing the relationship between \n average module attendance and average module mark",
       x="Average module attendance (%)", y="Average module mark (%)") +
  theme(plot.title = element_text(face = "bold", size = 12),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)),
        plot.margin=unit(c(1,1,1.5,1.2),"cm"))

save_plot(averagemark.by.attendance.plot, 400, 400, "averagemark_by_attendance_lm2.png",
          "Charts/")

########################
lmAtten = lm(PERCENTAGEATTENDANCE ~ COMMUTELENGTH, data = studentData.clean)
summary(lmAtten)
plot(lmAtten$residuals, pch = 16, col = "red")


cor.test(studentData.clean$PERCENTAGEATTENDANCE, studentData.clean$COMMUTELENGTH, method = "pearson")
cor.test(studentData.clean$PERCENTAGEATTENDANCE,studentData.clean$COMMUTELENGTH, method = "spearman", exact=F)

percattend.by.commutelength.plot <- 
  ggplot(studentData.clean, aes(x = COMMUTELENGTH, y = PERCENTAGEATTENDANCE, colour = NEWCOURSETITLE)) +
  geom_point(size = 1, pch = 21, show.legend = FALSE, alpha = 0.7) +
  geom_smooth(method=lm, colour="black", size=1) +
  theme(legend.position = 'none') +
  scale_color_viridis_d() +
  labs(title = "A scatterplot showing the relationship between \n commute length and average module attendance",
       x="Average commute length (Geodesic distance in miles)", y="Average module attendance (%)") +
  theme(plot.title = element_text(face = "bold", size = 12),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)),
        plot.margin=unit(c(1,1,1.5,1.2),"cm"))

save_plot(percattend.by.commutelength.plot, 400, 400, "percattend_by_commutelength_lm2.png",
          "Charts/")

########################
lmMarkComm = lm(AVERAGEMODULEMARK ~ COMMUTELENGTH, data = studentData.clean)
summary(lmMarkComm)

plot(AVERAGEMODULEMARK ~ COMMUTELENGTH, data = studentData.clean)
abline(lmMarkComm)

cor.test(studentData.clean$AVERAGEMODULEMARK, studentData.clean$COMMUTELENGTH, method = "pearson")

avgmark.by.commutelength.plot <- 
  ggplot(studentData.clean, aes(x = COMMUTELENGTH, y = AVERAGEMODULEMARK, colour = NEWCOURSETITLE)) +
  geom_point(size = 1, pch = 21, show.legend = FALSE, alpha = 0.7) +
  geom_smooth(method=lm, colour="black", size=1) +
  theme(legend.position = 'none') +
  xlim (0, 20) +
  ylim (0, 70) +
  scale_color_viridis_d() +
  labs(title = "A scatterplot showing the relationship between \n commute length and average module mark",
       x="Average commute length (Geodesic distance in miles)", y="Average module mark (%)") +
  theme(plot.title = element_text(face = "bold", size = 12),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)),
        plot.margin=unit(c(1,1,1.5,1.2),"cm"))

save_plot(avgmark.by.commutelength.plot, 400, 400, "avgmark_by_commutelength_lm.png",
          "Charts/")

plot(lmHeight$residuals, pch = 16, col = "red")


# Average Module Mark = a + Average Module Percentage * b1 + (Commute length) * b2
# You are now looking at the average mark as a function of the average module percentage and the commute length of the student
#When comparing students of similar commute times, the average predicted marks increases in 0.22% for every
#attendance the student has. THe same way, when comparing students with the same module percentage, the average mark increases
#(because the coefficient is positive) in 0.01% for each increase in commute length.
library(plyr)

# Break up studentData.clean by state, then fit the specified model to each piece and
# return a list
models <- dlply(studentData.clean, "NEWCOURSECODE", function(df) 
  lm(AVERAGEMODULEMARK ~ PERCENTAGEATTENDANCE + COMMUTELENGTH, data = df))

# Apply coef to each model and return a data frame
ldply(models, coef)

# Print the summary of each model - summart allows us to see detailed information on the model's performance and coefficients
l_ply(models, summary, .print = TRUE)

#multiple linear regression
lmCorr <- lm(AVERAGEMODULEMARK ~ PERCENTAGEATTENDANCE + COMMUTELENGTH, data = studentData.clean)
summary(lmCorr)

cor.test(studentData.clean$AVERAGEMODULEMARK, studentData.clean$COMMUTELENGTH, method = "pearson")

plot(AVERAGEMODULEMARK ~ PERCENTAGEATTENDANCE + COMMUTELENGTH, data = studentData.clean)
abline(lmCorr)
