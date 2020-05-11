cd <- read.csv(file.choose())
cd1 <- cd
View(cd)
cd1$ShelveLoc <- as.numeric(cd$ShelveLoc)
cd1$Urban <- as.numeric(cd$Urban)
cd1$US <- as.numeric(cd$US)
View(cd1)
summary(cd1$Sales)
library(tidyverse)
ggplot(cd1,aes(cd1$Sales))+geom_boxplot(fill="red")+coord_flip()
hist(cd1$Sales)
cd1$Sales <- ifelse(cd1$Sales>=10,"high",ifelse(cd1$Sales>=5,"medium","low"))
#Converting the Sales variable in factors 
cd1$Sales <- as.factor(cd1$Sales)
str(cd1)
#Conidering 0-5 as low sales,5-10 as medium sales,10-17 as high sales
library(tree)
mod <- tree(cd1$Sales~.,data = cd1)
mod
plot(mod)
text(mod)
prd <- as.data.frame(predict(mod,cd1))
prd["Final"] <- NULL
for (i in 1:nrow(prd)) {
prd[i,"FINAL"] <- ifelse(prd[i,"high"]>0.5,"high",ifelse(prd[i,"low"]>0.5,"low","medium"))
}
str(prd)
prd$FINAL <- as.factor(prd$FINAL)
View(prd)
library(gmodels)
library(arules)
crossTable(prd$FINAL,cd1$Sales)
mean(prd$FINAL==cd1$Sales)
#we are gettting an accuracy of 80% in our model

