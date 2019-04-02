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
  mutate(PERCENTAGEATTENDANCE = round(ACTUALATTENDANCEDAYS / EXPECTEDATTENDANCEDAYS * 100, 2)) %>%
  select(-STUDENTID)

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

#Remove subjects with masters degree type e.g. MEng or Integrated Masters
studentData.clean <- studentData.clean %>%
  filter(!grepl('Integrated|MEng', NEWCOURSETITLE))

#add column to represent degree type e.g. BA, BSc, LLB, MEng, Integrated masters
studentData.clean <- studentData.clean %>% 
  mutate(DEGREETYPE = word(NEWCOURSETITLE))

#add column to represent the degree classification e.g. 1st, 2:i, 2:ii, 3rd - ignoring marks under 40 
studentData.clean <- studentData.clean %>%
  mutate(DEGREECLASS = ifelse(AVERAGEMODULEMARK >= 70, '1st', 
                              ifelse(AVERAGEMODULEMARK >= 60 & AVERAGEMODULEMARK < 70, '2:i',
                                     ifelse(AVERAGEMODULEMARK >= 50 & AVERAGEMODULEMARK < 60, '2:ii', '3rd'))))