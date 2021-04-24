install.packages("csvy")
install.packages("rmatio")
install.packages("psych")
install.packages("tidyr")
install.packages("GGally")
install.packages("ggpubr")
library(ggpubr)
library(datasets)
library(GGally)
library(rio)
library(psych)
library(tidyr)

student_study <- import("/Users/cagnur/Desktop/student_study.csv")
sshour <- student_study %>% drop_na() 

summary(student_study)

head(student_study)

plot(student_study$Hours, student_study$Scores,
     pch = 19,         
     cex = 1.5,        
     col = "#cc0000",  
     main = "Student Study Hours as a Function of Student Scores",
     xlab = "Scores",
     ylab = "Hours")

shapiro.test(student_study$Hours) 

shapiro.test(student_study$Scores) 

cor_res <- cor.test(student_study$Hours, student_study$Scores, 
                    method = "pearson")
cor_res

cor_res$estimate

ggplot(student_study, aes(x = Scores, y = Hours)) +
  geom_point() +
  stat_smooth()

model <- lm(Scores ~ Hours, data = student_study)
model
