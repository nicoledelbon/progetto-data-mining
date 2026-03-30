rm(list=ls())
<<<<<<< HEAD
library(readr)
student <- read_csv("student_performance_updated_1000.csv")
sum(is.na(student))

student=student[,-c(1,2)]
