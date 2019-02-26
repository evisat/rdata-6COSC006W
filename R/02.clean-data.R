#load student data
studentData <- read.csv(data_path)

#cleans student data
glimpse(studentData)

#removes rows that contain one or more NA (empty) value
studentData.clean <- na.omit(studentData)

#reset the row names
row.names(studentData.clean) <- NULL

#create a new column that contains the percentage attendance
studentData.clean <- mutate(studentData.clean, 
                            PERCENTAGEATTENDANCE = ACTUALATTENDANCEDAYS / EXPECTEDATTENDANCEDAYS * 100
)
