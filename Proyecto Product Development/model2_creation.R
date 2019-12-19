library(dplyr)
library(readr)
library(rpartr)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

data<-read.csv("winequality-red.csv")

dt = sort(sample(nrow(data), nrow(data)*.75))
train<-data[dt,]
test<-data[-dt,]

fit <- rpart(quality ~ alcohol + density + total.sulfur.dioxide + chlorides + volatile.acidity + fixed.acidity,
             data=train,
             method="class")

fancyRpartPlot(fit)

Prediction <- predict(fit, test, type="class")
saveRDS(fit, "winequalitymodel_2.rds")
