#setwd('C:/Erika/Stevens/2019 Spring/BIA-654-B/Final Project')

library(dplyr)
library(tidyr)
library(ggplot2)
library(car)
library(readxl)

survey <- read_xlsx('Survey Results.xlsx', sheet='Survey Responses', skip=4)

survey_reshape <- survey %>% gather(key='Respondent_ID',value='Response',17:ncol(survey)) %>% select(-Average)

fit <- lm(Response~A*B*C*D*E*F*G+Respondent_ID,survey_reshape)
anova(fit)

final_fit <- lm(Response~A+B*C+D+F+G+Respondent_ID,survey_reshape)

#Plot residuals
ggplot() +
  geom_point(data=final_fit,aes(x=seq(1,1648),y=final_fit$residuals)) +
  labs(title='Residuals Plot for Final Regression Equation',x='Response ID',y='Residual') +
  theme_bw()

#Histogram of residuals
ggplot() + 
  geom_histogram(data=final_fit,aes(x=final_fit$residuals),fill='steelblue') +
  labs(title='Histogram of Residual Values for Final Regression Equation',x='Residual Value',y='Count') +
  theme_bw()
