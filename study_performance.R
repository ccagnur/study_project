install.packages("rio")
install.packages("dplyr")
install.packages("GGally")
install.packages("csvy")
install.packages("ggplot2")
install.packages("psych")
install.packages("tidyr")
library(rio)
library(GGally)
library(dplyr)
library(ggplot2)
library(psych)
library(tidyr)

student_perf <- import("/Users/cagnur/Desktop/StudentsPerf.csv")
sperf <- student_perf %>% drop_na() 

student_perf$avg_score <- rowMeans(student_perf [,6:8], na.rm=TRUE)
student_perf$total_score <- rowSums(student_perf [,6:8], na.rm=TRUE)       


counts <- table(student_perf$gender)
barplot(counts, main="Gender Distribution",
        xlab="Gender",
        ylab="Frequency") 

par(mfrow = c(3, 1))

hist(student_perf$math_score,
     main = "Distribution of Marks",
     xlab = "Math Scores",
     col = "red")

hist(student_perf$reading_score,
     main = "Distribution of Marks",
     xlab = "Reading Scores",
     col = "purple")

hist(student_perf$writing_score,
     main = "Distribution of Marks",
     xlab = "Writing Scores",
     col = "orange")

boxplot(student_perf$reading_score ~ student_perf$gender)
boxplot(student_perf$writing_score ~ student_perf$gender)

ggplot(data = student_perf, aes(x = reading_score, y = writing_score, color = gender)) +
  geom_point() +
  labs(title = "Reading Score by Writing Score",
       x = "Reading Score",
       y = "Writing Score")

ggplot(data = student_perf, aes(x = math_score, y = writing_score, color = gender)) +
  geom_point() +
  labs(title = "Math Score by Writing Score",
       x = "Math Score",
       y = "Writing Score")

ggplot(data = student_perf, aes(x = math_score, y = reading_score, color = gender)) +
  geom_point() +
  labs(title = "Math Score by Reading Score",
       x = "Math Score",
       y = "Reading Score")

ggplot(data = student_perf, aes(x = parental_education, y = avg_score, fill = parental_education)) +
  geom_boxplot() +
  ggtitle("Does the academic level of the parents influence the students' average score?") +
  xlab("Parental Level of Education") + ylab("Students' Average Scores")

ggplot(data = student_perf, aes(x = race, y = avg_score, fill = race)) +
  geom_boxplot() +
  ggtitle("Does the student's race / ethnicity influence his average score?") +
  xlab("Student's Race/Ethnicity") + ylab("Students' Average Scores")