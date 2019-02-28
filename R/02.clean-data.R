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

#remove courses who have less than [[N]] students (check on this number)
#a lot of courses have
studentData.clean <-studentData.clean %>%
  group_by(NEWCOURSECODE) %>%
  filter(n() >= 20)

#-------------------------------------------------------------------------------------------------------------------
#remove courses where they don't have both years '2016/2017' and '2017/2018'
#create a list of all the distinct course codes

distinct_ct = studentData.clean %>% distinct(NEWCOURSECODE)

g <- c(1,2,3)

course.list <- lapply(g, function(v) {
  hits <- agg[,"NEWCOURSECODE"] == val
  #store data set temporarily in a local value
  data.set <- agg[hits,]
  str(data.set)
  #assign levels to the cloumn, this adds levels to string data
  levels(data.set[,"NEWCOURSECODE"]) <- column.levels
  #return list item
  data.set
})

glimpse(studentData.clean)
#make unique codes vector
unique_course_codes <- as.vector(unique(studentData.clean$NEWCOURSECODE))

course_one_year <- studentData.clean %>%
  group_by(NEWCOURSECODE) %>%
  summarise(total_years = n_distinct(YEAR)) %>%
  filter(total_years < 2)

studentData.clean %>%
  group_by(NEWCOURSETITLE) %>%
  summarise(avg_mark = mean(AVERAGEMODULEMARK)) %>% 
  arrange(desc(avg_mark)) 

studentData.new <- subset(studentData.clean, !(NEWCOURSECODE %in% course_one_year[[1]]))

studentData.test <- studentData.clean %>%
  filter(!(NEWCOURSECODE %in% course_one_year[[1]]))


get_list_of_courses <- function() {
  distinct_ct = studentData.clean %>% distinct(NEWCOURSECODE)
  
  course.list <- list()
  
  for(i in distinct_ct) {
    for(j in i) {
      assign(paste0("c", j), studentData.clean %>%
               filter(NEWCOURSECODE == j))
      course.list[[paste0("c", j)]] <- studentData.clean %>%
        filter(NEWCOURSECODE == j)
    }
  }
}

get_list_of_courses()

courseIgnore.list = list()

for(v in names(course.list)) {
  # varName <- paste0("AM.", v)
  
  x <- get(v) %>%
    dplyr::group_by(YEAR) %>%
    dplyr::summarise(averageMark = mean(AVERAGEMODULEMARK, na.rm = TRUE)) %>%
    tally()
  
  if (x < 2) {
    courseIgnore.list[[substring(v, 2)]] <- substring(v, 2)
  }
}

for(c in names(courseIgnore.list)) {
  studentData.clean <- studentData.clean %>%
    dplyr::filter(!grepl(c, NEWCOURSECODE))
}
