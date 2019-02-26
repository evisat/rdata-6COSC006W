# view the first 6 rows
head(studentData)
# shows dimensions of data frame. first value shows number of columns and second value shows number of rows
dim(studentData)
#check out the column names
names(studentData)
#check out the structure of each column
str(studentData)
#quick summary of each column
summary(studentData)

#get number of rows that are in the Harrow Campus
harrow_campus <- subset(studentData, CAMPUSPOSTCODE == "HA1 3TP")

#get number of rows that are in the Cavendish Campus
cavendish_campus <- subset(studentData, CAMPUSPOSTCODE == "W1W 6XH")

#get number of rows that are in the Regent Campus
regent_campus <- subset(studentData, CAMPUSPOSTCODE == "W1B 2HW")

#get number of rows that are in the Marylebone Campus
marylebone_campus <- subset(studentData, CAMPUSPOSTCODE == "NW1 5LS")

#get number of rows that are in Level 4
level4 <- subset(studentData, STUDYLEVEL == "Level 4")

#get number of rows that are in Level 5
level5 <- subset(studentData, STUDYLEVEL == "Level 5")

#get number of rows that are in Level 6
level6 <- subset(studentData, STUDYLEVEL == "Level 6")

#get number of rows that are in 2016/2017
year1617 <- subset(studentData, YEAR == "2016/17")

#get number of rows that are in 2017/2018
year1718 <- subset(studentData, YEAR == "2017/18")

#install dplyr package for data transformation packages
library(dplyr)

#filters through the data ignoring false or NA values
#na.omit can be used to filter and remove NA values, however the row numbers will remain the same
#so it will essentially look like rows are missing when they're not actually.
studentData2 = filter(studentData)

#Exploring and summerising NA in data
is.na(studentData)
any(is.na(studentData))
colSums(is.na(studentData))
sum(is.na(studentData))

#create a new column to store the number of occurences of NA values in each row
studentData$na_count <- apply(is.na(studentData), 1, sum)
sum(studentData$na_count > 1)
#669 rows with NA values
#However only two rows that have more then 1 one NA value
#the command below returns all row indexes and columns that contain NA values
newdataset <- which(is.na(studentData), arr.ind=TRUE)

#Removing NAs
#22500 - 669 = 21831
studentData.clean <- na.omit(studentData) 
row.names(studentData.clean) <- NULL
complete.cases(studentData.clean)

with(studentData.clean, plot(COMMUTELENGTH, ACTUALATTENDANCEDAYS/EXPECTEDATTENDANCEDAYS * 100))
with(studentData.clean, plot(COMMUTELENGTH, AVERAGEMODULEMARK))

summary(studentData.clean$COMMUTELENGTH)

#ONLY Computer Science students 
compSci <- subset(studentData.clean, NEWCOURSETITLE == "BSc Computer Science FT")
row.names(compSci) <- NULL
summary(compSci)
str(compSci$NEWCOURSETITLE)
with(compSci, plot(COMMUTELENGTH * 1.6, ACTUALATTENDANCEDAYS/EXPECTEDATTENDANCEDAYS * 100))
with(compSci, plot(COMMUTELENGTH, AVERAGEMODULEMARK))

#ONLY Architecture students 
Architec <- subset(studentData.clean, NEWCOURSETITLE == "BA Architecture FT")
row.names(Architec) <- NULL
summary(Architec)
str(Architec$NEWCOURSETITLE)
with(Architec, plot(COMMUTELENGTH, ACTUALATTENDANCEDAYS/EXPECTEDATTENDANCEDAYS * 100))
with(Architec, plot(COMMUTELENGTH, AVERAGEMODULEMARK))


#ONLY law students 
Law <- subset(studentData.clean, NEWCOURSETITLE == "LLB Law FT" & STUDYLEVEL == "Level 4")
Law <- subset(studentData.clean, NEWCOURSETITLE == "LLB Law FT")
Law <- subset(studentData.clean, NEWCOURSETITLE == "LLB Law FT" & ACCOMMODATIONTYPE == "UoW Halls")
Law <- subset(studentData.clean, NEWCOURSETITLE == "LLB Law FT" & ACCOMMODATIONTYPE == "Commuter")
Law <- subset(studentData.clean, NEWCOURSETITLE == "LLB Law FT" & ACCOMMODATIONTYPE == "Other inc. Private Halls")
Law <- subset(studentData.clean, NEWCOURSETITLE == "LLB Law FT" & ACCOMMODATIONTYPE == "Unknown")

row.names(Law) <- NULL
summary(Law)
str(Law$NEWCOURSETITLE)
with(Law, plot(COMMUTELENGTH, ACTUALATTENDANCEDAYS/EXPECTEDATTENDANCEDAYS * 100))
with(Law, plot(ACTUALATTENDANCEDAYS/EXPECTEDATTENDANCEDAYS * 100, AVERAGEMODULEMARK))

library(ggplot2)

d <- ggplot(Law, aes(x=ACTUALATTENDANCEDAYS/EXPECTEDATTENDANCEDAYS * 100, y=AVERAGEMODULEMARK)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)   # Add linear regression line 
#  (by default includes 95% confidence region)

d + geom_point(aes(colour = factor(AVERAGEMODULEMARK)))
# Round xvar and yvar to the nearest 5
xaxis <- round(Law$COMMUTELENGTH/5)*5
yaxis <- round((Law$ACTUALATTENDANCEDAYS/Law$EXPECTEDATTENDANCEDAYS * 100)/5)*5

# Make each dot partially transparent, with 1/4 opacity
# For heavy overplotting, try using smaller values
ggplot(aes(x=xaxis, y=yaxis)) +
  geom_point(shape=19,      # Use solid circles
             alpha=1/4)     # 1/4 opacity


# Jitter the points
# Jitter range is 1 on the x-axis, .5 on the y-axis
ggplot(dat, aes(x=xrnd, y=yrnd)) +
  geom_point(shape=1,      # Use hollow circles
             position=position_jitter(width=1,height=.5))

#histogram num of students and commute time
ggplot(Law, aes(x=COMMUTELENGTH)) + geom_histogram(binwidth=3, color="black", fill="white")

#histogram num of students and commute time with density plot
ggplot(Law, aes(x=COMMUTELENGTH)) + geom_histogram(binwidth=3, aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666")

ggplot(Law, aes(COMMUTELENGTH)) +
  geom_freqpoly(binwidth = 3)

#number of students living at home for each course
g <- ggplot(Law, aes(ACCOMMODATIONTYPE))
g + geom_bar()

library(dplyr)
ed_exp5 <- select(filter(studentData.clean, NEWCOURSECODE),c(State,Minor.Population:Education.Expenditures))

for (row in 1:nrow(stock)) {
  price <- stock[row, "apple"]
  date  <- stock[row, "date"]
  
  if(price > 117) {
    print(paste("On", date, 
                "the stock price was", price))
  }
}

# select(-(NEWCOURSETITLE)) %>%

pie <- ggplot(cBAADS01F, aes(x = AVERAGEMODULEMARK, fill = factor(YEAR))) +
  geom_bar(width = 1)
pie + coord_polar(theta = "y")



ggplot(averageCL, aes(x=YEAR, y = averageMark)) + geom_bar(fill="mediumvioletred", color="midnightblue", stat = "identity") + ggtitle("Plot of length \n by dose")

ggsave("graph.png", plot = last_plot(), device = NULL, path = "~/Desktop/Final Year Project/Charts",
       scale = 1, width = 20, height = 20, units = "cm",
       dpi = 300, limitsize = TRUE)
