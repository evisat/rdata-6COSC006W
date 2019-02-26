#Filter the student data to get subsets for each campus post code
harrow_campus <- studentData.clean %>% 
  dplyr::filter(CAMPUSPOSTCODE == "HA1 3TP")

cavendish_campus <- studentData.clean %>% 
  dplyr::filter(CAMPUSPOSTCODE == "W1W 6XH")

regent_campus <- studentData.clean %>% 
  dplyr::filter(CAMPUSPOSTCODE == "W1B 2HW")

marylebone_campus <- studentData.clean %>% 
  dplyr::filter(CAMPUSPOSTCODE == "NW1 5LS")

#make a list of all the campus post codes
campus.list <- list(harrow_campus, cavendish_campus, regent_campus, marylebone_campus)

#Create a variable or file containing a subset of each rows for a particular course.
#There are around about 104 course therefore I will have 104 different subsets
#The reason I am doing this is to analyse the difference for each course.

#create a list of all the distinct course codes
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

for(course in names(course.list)) {
  # print(course.list[[course]][["NEWCOURSETITLE"]][1])
  varName <- paste0("AM-", course)
  assign(varName, get(course) %>%
           group_by(YEAR) %>%
           summarise(averageMark = mean(AVERAGEMODULEMARK, na.rm = TRUE)))

  ggplot(get(varName), aes(x=YEAR, y = averageMark)) + geom_bar(fill="mediumvioletred", color="midnightblue", stat = "identity") + ggtitle(paste0("Graph Showing Average Mark for students studying the course \n", course.list[[course]][["NEWCOURSETITLE"]][1]))

  ggsave(paste0("AM-", course, ".png"), plot = last_plot(), device = NULL, path = "~/Desktop/Final Year Project/Charts",
          scale = 1, width = 20, height = 20, units = "cm",
          dpi = 300, limitsize = TRUE)
}

#get average commute time per Course
by_course <- group_by(studentData.clean, CAMPUSPOSTCODE)
averageCL <- summarise(by_course, mean(AVERAGEMODULEMARK, na.rm = TRUE))

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
