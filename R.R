dataset <- read.csv("dataset.csv") 
head(dataset) 
attach(dataset) 

table(dataset$rlgdnm)

dataset1<-dataset[dataset$rlgdnm!="Islam",] 

dataset1$rlgdnm<-recode(dataset1$rlgdnm,"Roman_Catholic"="Католицизм","Protestant"="Протестантизм","Eastern_Orthodox"="Православие")
# 1 и 3 (частично) гипотезы 
model1<-lm(imdfetn~1+rlgdgr+agea+gndr+hinctnta+eisced,data=dataset1) 
tab_model(model1) 
check_heteroscedasticity(model1) 
model_parameters(model1,vcov="HC3") 

# 2 гипотеза 
denom<-relevel(factor(dataset1$rlgdnm),ref='Протестантизм') 
model2<-lm(imdfetn~denom*rlgdgr+agea+gndr+hinctnta+eisced,data=dataset1) 
interact_plot(model2,pred=rlgdgr,modx="denom",interval=TRUE,robust=TRUE,x.label="Уровень субъективной религиозности",y.label="Неготовность принимать иммигрантов расы/этнической группы, 
              отличной от большинства",legend.main ="Деноминация") 
tab_model(model2) 
check_heteroscedasticity(model2) 
model_parameters(model2,vcov="HC3") 

# 3 гипотеза 
model3<-lm(imsmetn~rlgdgr+agea+gndr+hinctnta+eisced,data=dataset1) 
tab_model(model3) 
check_heteroscedasticity(model3) 
model_parameters(model3,vcov="HC3") 

# 4 гипотеза 
denom<-relevel(factor(dataset1$rlgdnm),ref='Православие') 
model4<-lm(imsmetn~denom*rlgdgr+agea+gndr+hinctnta+eisced,data=dataset1) 
interact_plot(model4,pred=rlgdgr,modx=denom) 
tab_model(model4) 
check_heteroscedasticity(model4) 
model_parameters(model4,vcov="HC3") 

model5<-lm(imdfetn~denom*rlgdgr+agea+gndr+hinctnta+eisced,data=dataset1) 
interact_plot(model5,pred=rlgdgr,modx="denom",interval=TRUE,robust=TRUE,x.label="Уровень субъективной религиозности",y.label="Неготовность принимать иммигрантов расы/этнической группы, 
              отличной от большинства",legend.main="Деноминация",modxvals=c("Православие","Протестантизм")) 
tab_model(model5) 
check_heteroscedasticity(model5) 
model_parameters(model5,vcov="HC3") 

dataset2<-dataset[dataset$rlgdnm=="Islam",] 

# 6 гипотеза 
model6<-lm(imdfetn~rlgdgr+agea+gndr+hinctnta+eisced,data=dataset2) 
tab_model(model6) 
check_heteroscedasticity(model6) 
model_parameters(model6,vcov="HC3") 

model7<-lm(imsmetn~rlgdgr+agea+gndr+hinctnta+eisced,data=dataset2) 
tab_model(model7) 
check_heteroscedasticity(model7) 
model_parameters(model7,vcov="HC3") 


screenreg(list(model3,model4,model7))
htmlreg(list(model3,model4,model7),file='same.doc')
screenreg(list(model1,model2,model5,model6))
htmlreg(list(model1,model2,model5,model6),file='dif.doc')


