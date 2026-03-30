rm(list=ls())
<<<<<<< HEAD
library(readr)
student <- read_csv("student_performance_updated_1000.csv")
sum(is.na(student))
=======
data <- read.csv("IMDbMovies-Clean.csv", header = TRUE)
data[data==""] <- NA
sum(is.na(data))
>>>>>>> b5ae94f880c3d010ae623ff3093fdd57c495ce25

student=student[,-c(1,2)]
