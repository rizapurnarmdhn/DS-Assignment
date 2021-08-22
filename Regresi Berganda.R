budidaya<- read.csv(file.choose(), header = T)

head(budidaya,10)

library(tidyr)
library(dplyr)
budidaya1<- budidaya%>% drop_na(ABW)
budidaya1

budidaya2<- budidaya1%>%select(ABW,Size,Total.Weight,Survival.Rate)
budidaya2

budidaya3<- scale(budidaya2)
budidaya3
budidaya3<-as.data.frame(budidaya3)

model<- lm(budidaya3$Total.Weight~budidaya3$ABW+budidaya3$Size+budidaya3$Survival.Rate)
model


summary(model)

model2<- lm(budidaya3$Total.Weight~budidaya3$Size+budidaya3$Survival.Rate)
model2

summary(model2)

##normalitas
library(tseries)
library(normtest)
library(nortest)
jarque.bera.test(model2$residuals)

ad.test(model2$residuals)

#heterodeksitas
library(lmtest)
bptest(model2)

#multiko
library(car)
vif(model2)

library(corrplot)
library(RColorBrewer)
M <-cor(budidaya3)
corrplot(M, type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"))
cor(budidaya3)


