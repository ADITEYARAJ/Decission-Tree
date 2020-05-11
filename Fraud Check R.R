fraud <- read.csv(file.choose())
View(fraud)
fraud1 <- fraud
summary(fraud)
str(fraud)
#Undergrad,Maratial.Status,Urban are categorical variable
#Convert them into numeric or factors
fraud1$Undergrad <- as.numeric(fraud$Undergrad)
fraud1$Marital.Status <- as.numeric(fraud$Marital.Status)
fraud1$Urban <- as.numeric(fraud$Urban)
View(fraud1)
fraud1$Taxable.Income <- ifelse(fraud1$Taxable.Income<=30000,"Risky","Good")
fraud1$Taxable.Income <- as.factor(fraud1$Taxable.Income)
str(fraud1)
table(fraud1$Taxable.Income)
library(tree)
fraud1[,-3] <- scale(fraud1[,-3])
mod1 <- tree(fraud1$Taxable.Income~fraud1$Undergrad+fraud1$Marital.Status+fraud1$Work.Experience,data = fraud1)
mod1
plot(mod1)
text(mod1,pretty = 0)
p <-as.data.frame(predict(mod1,fraud1))
p
table(p)
p[,"Final"] <- NULL
xyz <- function(x){
  
return(ifelse(x <= 30000,"Risky","Good"))
}
fraud1[,3] <- xyz(fraud1[,3])
table(fraud1$Taxable.Income)
p[,"Final"] <- xyz(p)
p
library(gmodels)
crossTable(fraud1$Taxable.Income,p)
fraud1[,-3] <- scale(fraud1[,-3])

