studentData <- read.csv("Source/uowdata.csv")
# view the first handful of rows
head(studentData)
# shows dimensions of data frame. first value shows number of columns and second value shows number of rows
dim(studentData)
#check out the column names
names(studentData)
#check out the structure of each column
str(studentData)
#quick summary of each column
summary(studentData)
