projectdf = read.csv("production.csv")
str(projectdf)
avgcost = tapply(projectdf$COST, projectdf$PRODUCTCODE, mean)
avgcost
avg_MT = tapply(projectdf$MACHINETIME, projectdf$PRODUCTCODE, mean)
avg_MT
avg_AT = tapply(projectdf$ASSEMBLYTIME, projectdf$PRODUCTCODE, mean)
avg_AT
avg_FT = tapply(projectdf$FINISHTIME, projectdf$PRODUCTCODE, mean)
avg_FT
demanddf = read.csv("demand.csv")
str(demanddf)
s1demand = subset(demanddf, PRODUCTCODE == "s1")
s1demand
plot(s1demand$PERIOD, s1demand$DEMAND)

lms1 = lm(DEMAND ~ PERIOD, s1demand)
summary(lms1)
s1demand53 = (4.595 * 100) + (53 * 3.403)
s1demand53

s2demand = subset(demanddf, PRODUCTCODE == 's2')
s2demand
plot(s2demand$PERIOD, s2demand$DEMAND)

lms2 = lm(DEMAND ~ PERIOD, s2demand)
summary(lms2)
s2demand53 = (5.025 * 100) + (53 * 4.001)
s2demand53

s3demand = subset(demanddf, PRODUCTCODE == 's3')
s3demand
plot(s3demand$PERIOD, s3demand$DEMAND)
lms3 = lm(DEMAND ~ PERIOD, s3demand)
summary(lms3)
s3demand53 = (420.02) + (53 * 3.999)
s3demand53

qualitydf = read.csv("quality.csv")
seed = 1000
set.seed(seed)
testRows = sample(1:nrow(qualitydf),round(0.2*nrow(qualitydf)))
test = qualitydf[testRows, ]
train = qualitydf[-testRows, ]

library(rpart)
model = rpart(quality ~ ., data = train, method = "class")
library(rpart.plot)
prp(model)

predict_train = predict(model, train[ , -1], type="class")
predict_test = predict(model, test[ , -1], type="class")
actual_train  = train$quality
actual_test = test$quality

