# install.packages('readr')
library(jsonlite)
library(readr)

oldColNames <- colnames(studentData.clean)

newColNames <- c("year", "course_code", "course_title", "study_level", "commute_length", "accomm_type", 
                 "campus_postcode", "average_modulemark", "perc_attendance", "degree_type", "degree_class", "campus_name")

studentData.export <- studentData.clean %>%
  rename_at(vars(oldColNames), ~newColNames)

studentData.export %>%
  toJSON(pretty = TRUE) %>%
  write_lines(paste0(project_path, "Output/uowdata_clean.json"))

#example of json to follow:
# [
#   {
#     "year":"2016/17",
#     "course_code":"BAJMC06F",
#     "course_title":"BA Television Production FT",
#     "study_level":"Level 4",
#     "commute_length":0.59,
#     "accomm_type":"UoW Halls",
#     "campus_postcode":"HA1 3TP",
#     "average_modulemark":67,
#     "perc_attendance":72.62,
#     "degree_type":"BA",
#     "degree_class":"2:i"
#   },
# ]