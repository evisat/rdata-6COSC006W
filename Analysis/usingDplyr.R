library(dplyr)
studentData <- read.csv("Source/uowdata.csv")
studentData.clean <- na.omit(studentData) 
row.names(studentData.clean) <- NULL

#create a new column that contains the percentage attendance
studentData.clean <- mutate(studentData.clean, 
       PERCENTAGEATTENDANCE = ACTUALATTENDANCEDAYS / EXPECTEDATTENDANCEDAYS * 100
       )

#Create a variable or file containing a subset of each rows for a particular course.
#There are around about 104 course therefore I will have 104 different subsets
#The reason I am doing this is to analyse the difference for each course.

#looking at just Computer Science
BSCSS02F <- filter(studentData.clean, NEWCOURSECODE == "BSCSS02F")


#get average commute time per Course
by_course <- group_by(studentData.clean, NEWCOURSECODE)

averageCL <- summarise(by_course,  averageCL = mean(COMMUTELENGTH, na.rm = TRUE))
averageCL <- arrange(averageCL, desc(averageCL))
#use the first row and last value under course code to match to student data set to get the name of the course
#this will be the course who on average has the largest commute length in geodesic miles.
#below returns the first and last row in the group 
#good way to pronounce (pipe) %>% when reading code is 'then'

averageCL %>%
  dplyr::arrange(desc(averageCL)) %>%
  dplyr::filter(row_number() %in% c(1))

first(averageCL %>%
  arrange(desc(averageCL)) %>%
  filter(row_number() %in% c(1))
)

#get course title from course code


#get average grade per Course
averageMM <- summarise(by_course,  averageMM = mean(AVERAGEMODULEMARK, na.rm = TRUE))
