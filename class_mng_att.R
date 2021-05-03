install.packages("rio")
install.packages("dplyr")
install.packages("GGally")
install.packages("csvy")
install.packages("ggplot2")
install.packages("psych")
install.packages("tidyr")
install.packages("corrplot")
install.packages("ggcorrplot")
library(rio)
library(GGally)
library(dplyr)
library(ggplot2)
library(psych)
library(tidyr)
library(corrplot)
library(ggcorrplot)
class_mng <- import("/Users/cagnur/Desktop/class_mng.csv")
class <- class_mng %>% drop_na() 


cols = c("B2", "B3", "B5", "B8", "B11", "B12", "B18", "B22")
class_mng [ ,cols] = 6 - class_mng[ ,cols]

colsi = c("B6", "B7", "B10", "B12", "B13", "B16", "B19", 
          "B20", "B23", "B25")
class_mng$inatt_mean <- rowMeans(class_mng [ ,colsi] , na.rm=TRUE)

colsh = c("B2", "B3", "B5", "B8", "B11", "B18", "B22")
class_mng$hum_mean <- rowMeans(class_mng [ ,colsh] , na.rm=TRUE)


colsd = c("B1", "B4", "B14", "B21")
class_mng$disc_mean <- rowMeans(class_mng [ ,colsd] , na.rm=TRUE)


colsr = c("B9", "B15", "B17", "B24")
class_mng$rel_mean <- rowMeans(class_mng [ ,colsr] , na.rm=TRUE)


shapiro.test(c(4:28))

cronbach.alpha(class_mng, standardized = FALSE, CI = FALSE, 
               probs = c(0.025, 0.975), B = 1000, na.rm = FALSE)


subd1 <- class_mng %>% select(colsi)
head(subd1)
alpha(subd1)

subd2 <- class_mng %>% select(colsh)
head(subd2)
alpha(subd2)

subd3 <- class_mng %>% select(colsd)
alpha(subd3)

subd4 <- class_mng %>% select(colsr)
alpha(subd4)

ggplot(data = class_mng, aes(x = inatt_mean, y = bully_exp)) +
  geom_point() +
  labs(title = "Scatterplot of Inattentive Manner vs Bullying Experience",
       x = "Inattentive Manner",
       y = "Bullying Experience")
cor.test(class_mng$inatt_mean, class_mng$bully_exp, method = 'pearson')


ggplot(data = class_mng, aes(x = hum_mean, y = bully_exp)) +
  geom_point() +
  labs(title = "Scatterplot of Humanist Manner vs Bullying Experience",
       x = "Humanist Manner",
       y = "Bullying Experience")
cor.test(class_mng$hum_mean, class_mng$bully_exp, method = 'pearson')


ggplot(data = class_mng, aes(x = disc_mean, y = bully_exp)) +
  geom_point() +
  labs(title = "Scatterplot of Disciplinary Manner vs Bullying Experience",
       x = "Disciplinary Manner",
       y = "Bullying Experience")
cor.test(class_mng$disc_mean, class_mng$bully_exp, method = 'pearson')


ggplot(data = class_mng, aes(x = rel_mean, y = bully_exp)) +
  geom_point() +
  labs(title = "Scatterplot of Relentless Manner vs Bullying Experience",
       x = "Relentless Manner",
       y = "Bullying Experience")
cor.test(class_mng$rel_mean, class_mng$bully_exp, method = 'pearson')

my_data <- class_mng[ ,c(3, 29, 30, 31, 32)]
head(my_data)
res <- cor(my_data)
round(res, 1)

ggcorrplot(res, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle", 
           colors = c("tomato2", "white", "springgreen3"), 
           title="Correlogram of the Data", 
           ggtheme=theme_bw)

corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

ggplot(class_mng, aes(x=prof_year, y=bully_exp)) + 
  geom_bar(stat="identity", width=.5, fill="tomato3") + 
  labs(title="Ordered Bar Chart", 
       subtitle="Proficiency Year Vs Bullying Experience", 
       caption="source: mpg") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

ggplot(class_mng, aes(x=gender, y=bully_exp)) + 
  geom_bar(stat="identity", width=.5, fill="tomato3") + 
  labs(title="Ordered Bar Chart", 
       subtitle="Gender Vs Bullying Experience", 
       caption="source: mpg") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))


t.test(bully_exp ~ gender, 
       data = class_mng)
