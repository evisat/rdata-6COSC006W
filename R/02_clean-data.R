#load student data
studentData <- read.csv(data_path)

#cleans student data
glimpse(studentData)

#removes rows that contain one or more NA (empty) value
studentData.clean <- na.omit(studentData)

#reset the row names
row.names(studentData.clean) <- NULL

#create a new column that contains the percentage attendance
studentData.clean <- studentData.clean %>% 
  mutate(PERCENTAGEATTENDANCE = ACTUALATTENDANCEDAYS / EXPECTEDATTENDANCEDAYS * 100
)

#remove courses who have less than [[N]] students (check on this number)
#a lot of courses have
studentData.clean <-studentData.clean %>%
  group_by(NEWCOURSECODE) %>%
  filter(n() >= 20)

#remove courses where they don't have both years '2016/2017' and '2017/2018'
#create a vector  of all the unique course codes
unique_course_codes <- as.vector(unique(studentData.clean$NEWCOURSECODE))

#get all course who only have rows representing one year
course_one_year <- studentData.clean %>%
  group_by(NEWCOURSECODE) %>%
  summarise(total_years = n_distinct(YEAR)) %>%
  filter(total_years < 2)

#filter these course from the data
studentData.clean <- studentData.clean %>%
  filter(!(NEWCOURSECODE %in% course_one_year[[1]]))