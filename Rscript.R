install.packages("tidyverse")
install.packages("sjstats")
install.packages("performance")
install.packages("lme4")
install.packages('texreg')
install.packages('ICC')
install.packages('sjPlot')
install.packages('ordinal')
install.packages('lmerTest')
install.packages('emmeans')
install.packages('glmmTMB')

library(performance)
library(sjstats)
library(tidyverse)
library(lme4) 
library(texreg)
library(ICC)
library(sjPlot)
library(ordinal)
library(lmerTest)
library(dplyr)
library(survival)
library(glmmTMB)

options(max.print=1000000)

dataset <- read.csv("dataset.csv")
head(dataset)
attach(dataset)
y_names <- c('imsmetn', 'imdfetn')
for (y_name in y_names) {
  dataset$y <- dataset[[y_name]]
  dataset$y_f <- factor(dataset$y, ordered = TRUE)
  
  print(ICCbare(cntry, y, data=dataset))
  nullm <- lmer(y ~ (1 | cntry), data = dataset, REML = FALSE) 
  print(performance::icc(nullm))
  
  # Многоуровневая линейная
  reg <- lmer(y ~ agea + gndr + hinctnta + eisced + Roman_Catholic + Protestant + Eastern_Orthodox + Islam + Non_Confessional + (1 | cntry), data=dataset)
  print(summary(reg))
  
  saveRDS(reg, file=paste('reg_', y_name,'.rda', sep=''))
  
  # Многоуровневая порядковая
  fmm1 <- clmm(y_f ~ agea + gndr + hinctnta + eisced + Roman_Catholic + Protestant + Eastern_Orthodox + Islam + Non_Confessional + (1 | cntry), data=dataset, Hess=TRUE)
  print(summary(fmm1))
  print(AIC(reg, fmm1))
  print(anova(reg, fmm1))

  fmm2 <- clmm(y_f ~ agea + gndr + hinctnta + eisced + Roman_Catholic + Protestant + Eastern_Orthodox + Islam + Non_Confessional + (1 + hinctnta | cntry), data=dataset, Hess=TRUE)
  print(summary(fmm2))
  print(AIC(reg, fmm2))
  print(anova(fmm1, fmm2))
  
  fmm3 <- clmm(y_f ~ agea + gndr + hinctnta + eisced + Roman_Catholic + Protestant + Eastern_Orthodox + Islam + Non_Confessional + (1 + hinctnta + eisced | cntry), data=dataset, Hess=TRUE, link='logit')
  print(summary(fmm3))
  print(AIC(fmm2, fmm3))
  print(anova(fmm2, fmm3))
  
  saveRDS(fmm3, file=paste('fmm3_', y_name,'.rda', sep=''))
  fmm3 <- readRDS(paste('fmm3_', y_name,'.rda', sep=''))
  
  
  # Тест на гетероскедастичность и нормальность остатков линейной регрессии
  print(check_heteroscedasticity(reg))
  residuals <- resid(reg)
  print(length(residuals[duplicated(residuals)]))
  print(length(residuals))
  print(ks.test(residuals, 'pnorm'))
  residuals <- residuals+ runif(length(residuals),-.01,.01)
  print(ks.test(residuals, 'pnorm'))
}

y_name = 'imsmetn'
reg <- readRDS(paste('reg_', y_name,'.rda', sep=''))
hist(resid(reg), breaks=40, main = "Гистограмма остатков для переменной \n«Разрешить въезд многим/немногим иммигрантам той же \nрасы/этнической группы, что и большинство» (imsmetn)", xlab="Остатки регрессии", ylab="Частота")

fmm3 <- readRDS(paste('fmm3_', y_name,'.rda', sep=''))
exp(coef(fmm3))
plot_model(fmm3) + xlab('Переменная') + ylab('Отношение шансов') + ggtitle(paste('Отношение шансов, зависимая переменная', y_name)) + scale_x_discrete(breaks=c("Islam","Eastern_Orthodox", 'Protestant', 'Roman_Catholic', 'eisced', 'hinctnta', 'gndr', 'agea'), 
                                                                                                                                                       labels=c("Исламская религиозность","Православная религиозность", 'Протестантская религиозность', 'Католическая религиозность', 'Образование', 'Доход', 'Пол', 'Возраст'), 
                                                                                                                                                       limits=c("Islam", 'Roman_Catholic', 'eisced', 'hinctnta', 'gndr', 'agea')) + ylim(0.5, 1.5)
p <- plot_model(fmm3, type = "re", grid=F, sort.est = T,  axis.lim=c(0.1, 10))
p[[1]] + xlab('Страна (уровень)') + ylab('Случайный эффект') + ggtitle(paste('Случайные эффекты Intercept, зависимая переменная', y_name))
p[[2]] + ylim(0.5, 1.5) + xlab('Страна (уровень)') + ylab('Случайный эффект') + ggtitle(paste('Случайные эффекты ковариаты eisced, зависимая переменная', y_name))
p[[3]] + ylim(0.75, 1.25) + xlab('Страна (уровень)') + ylab('Случайный эффект') + ggtitle(paste('Случайные эффекты ковариаты hinctnta, зависимая переменная', y_name))


y_name = 'imdfetn'
reg <- readRDS(paste('reg_', y_name,'.rda', sep=''))
hist(resid(reg), breaks=40, main = "Гистограмма остатков для переменной \n«Разрешить въезд многим/немногим иммигрантам \nрасы/этнической группы, отличной от большинства» (imdfetn)", xlab="Остатки регрессии", ylab="Частота")

fmm3 <- readRDS(paste('fmm3_', y_name,'.rda', sep=''))
exp(coef(fmm3))
plot_model(fmm3) + xlab('Переменная') + ylab('Отношение шансов') + ggtitle(paste('Отношение шансов, зависимая переменная', y_name)) + scale_x_discrete(breaks=c("Islam","Eastern_Orthodox", 'Protestant', 'Roman_Catholic', 'eisced', 'hinctnta', 'gndr', 'agea'), 
                                                                                                                                                       labels=c("Исламская религиозность","Православная религиозность", 'Протестантская религиозность', 'Католическая религиозность', 'Образование', 'Доход', 'Пол', 'Возраст'), 
                                                                                                                                                       limits=c("Islam", "Eastern_Orthodox", 'Protestant', 'Roman_Catholic', 'eisced', 'hinctnta', 'gndr', 'agea')) + ylim(0.5, 1.5)
p <- plot_model(fmm3, type = "re", grid=F, sort.est = T,  axis.lim=c(0.1, 10))
p[[1]] + xlab('Страна (уровень)') + ylab('Случайный эффект') + ggtitle(paste('Случайные эффекты Intercept, зависимая переменная', y_name))
p[[2]] + ylim(0.5, 1.5) + xlab('Страна (уровень)') + ylab('Случайный эффект') + ggtitle(paste('Случайные эффекты ковариаты eisced, зависимая переменная', y_name))
p[[3]] + ylim(0.75, 1.25) + xlab('Страна (уровень)') + ylab('Случайный эффект') + ggtitle(paste('Случайные эффекты ковариаты hinctnta, зависимая переменная', y_name))
