# install.packages("devtools")
# devtools::install_github("hrbrmstr/waffle")

# library(waffle)

total_averagemarks_by_classification_per_degreetype <- studentData.clean %>%
  filter((STUDYLEVEL == 'Level 5') & (AVERAGEMODULEMARK >= 40)) %>%
  group_by(DEGREETYPE) %>%
  summarize('1st' = sum(AVERAGEMODULEMARK >= 70),
            '2:i' = sum(AVERAGEMODULEMARK >= 60 & AVERAGEMODULEMARK < 70),
            '2:ii' = sum(AVERAGEMODULEMARK >= 50 & AVERAGEMODULEMARK < 60),
            '3rd' = sum(AVERAGEMODULEMARK < 50))

##### BA
totalBA <- total_averagemarks_by_classification_per_degreetype %>%
  filter(DEGREETYPE == 'BA')

# reverse the order of the columns and rows
reverseBA <- data.frame(t(totalBA[-1]))
colnames(reverseBA) <- totalBA[, 1]

waffle.data.BA <- data.frame(
  names = rownames(reverseBA),
  vals = reverseBA$BA
)

Ba.waffle.plot <- waffle(waffle.data.BA, rows = 55) +
  labs(title = "Average classifications of BA students in level 5", fill='Classification type') +
  theme(plot.title = element_text(face = "bold", size = 24),
        legend.text = element_text(size = 20, margin = margin(t = 0, r = 0, b = 5, l = 0, unit = "pt")),
        plot.margin=unit(c(1,1,1.5,1.2),"cm"))

#save plot as png
save_plot(Ba.waffle.plot, 1000, 940, "averagemark_classifications_BA_level5.png",
          "~/Desktop/Final Year Project/rdata-6COSC006W/Charts/BA_waffle_charts/")


##### BMus
totalBMus <- total_averagemarks_by_classification_per_degreetype %>%
  filter(DEGREETYPE == 'BMus')

reverseBMus <- data.frame(t(totalBMus[-1]))
colnames(reverseBMus) <- totalBMus[, 1]

waffle.data.BMus <- data.frame(
  names = rownames(reverseBMus),
  vals = reverseBMus$BMus
)

BMus.waffle.plot <- waffle(waffle.data.BMus, rows = 8) +
  labs(title = "Average classifications of BMus students in level 5", fill='Classification type') +
  theme(plot.title = element_text(face = "bold", size = 24),
        legend.text = element_text(size = 20, margin = margin(t = 0, r = 0, b = 5, l = 0, unit = "pt")),
        plot.margin=unit(c(1,1,1.5,1.2),"cm"))

#save plot as png
save_plot(BMus.waffle.plot, 1000, 940, "averagemark_classifications_BMus_level5.png",
          "~/Desktop/Final Year Project/rdata-6COSC006W/Charts/BMus_waffle_charts/")


##### BEng
totalBEng <- total_averagemarks_by_classification_per_degreetype %>%
  filter(DEGREETYPE == 'BEng')

reverseBEng <- data.frame(t(totalBEng[-1]))
colnames(reverseBEng) <- totalBEng[, 1]

waffle.data.BEng <- data.frame(
  names = rownames(reverseBEng),
  vals = reverseBEng$BEng
)

BEng.waffle.plot <- waffle(waffle.data.BEng, rows = 5) +
  labs(title = "Average classifications of BEng students in level 5", fill='Classification type') +
  theme(plot.title = element_text(face = "bold", size = 24),
        legend.text = element_text(size = 20, margin = margin(t = 0, r = 0, b = 5, l = 0, unit = "pt")),
        plot.margin=unit(c(1,1,1.5,1.2),"cm"))

#save plot as png
save_plot(BEng.waffle.plot, 1000, 940, "averagemark_classifications_BEng_level5.png",
          "~/Desktop/Final Year Project/rdata-6COSC006W/Charts/BEng_waffle_charts/")


##### LLB
totalLLB <- total_averagemarks_by_classification_per_degreetype %>%
  filter(DEGREETYPE == 'LLB')

reverseLLB <- data.frame(t(totalLLB[-1]))
colnames(reverseLLB) <- totalLLB[, 1]

waffle.data.LLB <- data.frame(
  names = rownames(reverseLLB),
  vals = reverseLLB$LLB
)

LLB.waffle.plot <- waffle(waffle.data.LLB, rows = 15) +
  labs(title = "Average classifications of LLB students in level 5", fill='Classification type') +
  theme(plot.title = element_text(face = "bold", size = 24),
        legend.text = element_text(size = 20, margin = margin(t = 0, r = 0, b = 5, l = 0, unit = "pt")),
        plot.margin=unit(c(1,1,1.5,1.2),"cm"))

#save plot as png
save_plot(LLB.waffle.plot, 1000, 940, "averagemark_classifications_LLB_level5.png",
          "~/Desktop/Final Year Project/rdata-6COSC006W/Charts/LLB_waffle_charts/")


##### BSc
totalBSc <- total_averagemarks_by_classification_per_degreetype %>%
  filter(DEGREETYPE == 'BSc')

reverseBSc <- data.frame(t(totalBSc[-1]))
colnames(reverseBSc) <- totalBSc[, 1]

waffle.data.BSc<- data.frame(
  names = rownames(reverseBSc),
  vals = reverseBSc$BSc
)

BSc.waffle.plot <- waffle(waffle.data.BSc, rows = 40) +
  labs(title = "Average classifications of BSc students in level 5", fill='Classification type') +
  theme(plot.title = element_text(face = "bold", size = 24),
        legend.text = element_text(size = 20, margin = margin(t = 0, r = 0, b = 5, l = 0, unit = "pt")),
        plot.margin=unit(c(1,1,1.5,1.2),"cm"))

#save plot as png
save_plot(BSc.waffle.plot, 1000, 940, "averagemark_classifications_BSc_level5.png", 
          "~/Desktop/Final Year Project/rdata-6COSC006W/Charts/BSc_waffle_charts/")
