pacman::p_load(pacman, dplyr, GGally, ggplot2, ggthemes, ggvis, httr, 
               lubridate, plotly, rio, rmarkdown, shiny, stringr, tidyr)
library(datasets)

head(USJudgeRatings)
data <- USJudgeRatings
?USJudgeRatings

x <- as.matrix(data[-12])
y <- data[, 12]

reg1 <- lm(y ~ x)
reg1
summary(reg1)

anova(reg1)
coef(reg1)
resid(reg1)
hist(resid(reg1))
