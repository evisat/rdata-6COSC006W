install.packages("devtools")
devtools::install_github("hrbrmstr/waffle")

library(waffle)
library(extrafont)
packageVersion("waffle")

font_import()


devtools::install_github("rstudio/fontawesome")
# (fa("graduation-cap", fill = "purple")

year4 <- studentData.clean %>%
  filter((STUDYLEVEL == 'Level 4') & (AVERAGEMODULEMARK >= 40)) %>%
  group_by(DEGREETYPE) %>%
  summarize('1st' = sum(AVERAGEMODULEMARK >= 70),
            '2:i' = sum(AVERAGEMODULEMARK >= 60 & AVERAGEMODULEMARK < 70),
            '2:ii' = sum(AVERAGEMODULEMARK >= 50 & AVERAGEMODULEMARK < 60),
            '3rd' = sum(AVERAGEMODULEMARK < 50))

##### BA
dataIneed <- year4 %>%
  filter(DEGREETYPE == 'BA')

df2 <- data.frame(t(dataIneed[-1]))
colnames(df2) <- dataIneed[, 1]

parts <- data.frame(
  names = rownames(df2),
  vals = df2$BA
)

waffle(parts, rows = 55)

##### BMus

dataIneed <- year4 %>%
  filter(DEGREETYPE == 'BMus')

df2 <- data.frame(t(dataIneed[-1]))
colnames(df2) <- dataIneed[, 1]

parts <- data.frame(
  names = rownames(df2),
  vals = df2$BMus
)

waffle(parts, rows = 8)

##### BEng

dataIneed <- year4 %>%
  filter(DEGREETYPE == 'BEng')

df2 <- data.frame(t(dataIneed[-1]))
colnames(df2) <- dataIneed[, 1]

parts <- data.frame(
  names = rownames(df2),
  vals = df2$BEng
)

waffle(parts, rows = 5)

##### LLB

dataIneed <- year4 %>%
  filter(DEGREETYPE == 'LLB')

df2 <- data.frame(t(dataIneed[-1]))
colnames(df2) <- dataIneed[, 1]

parts <- data.frame(
  names = rownames(df2),
  vals = df2$LLB
)

waffle(parts, rows = 15)

##### BSc

dataIneed <- year4 %>%
  filter(DEGREETYPE == 'BSc')

df2 <- data.frame(t(dataIneed[-1]))
colnames(df2) <- dataIneed[, 1]

parts <- data.frame(
  names = rownames(df2),
  vals = df2$BSc
)

waffle(parts, rows = 40)
