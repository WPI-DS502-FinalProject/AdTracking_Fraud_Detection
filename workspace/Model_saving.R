

######### Saving the model ###############
saveRDS(tree_, "./final_model.rds")

######## Preparing the test data #########
df <- read.csv(file="sub_0.csv", header=TRUE, sep=",")
names(df) <- c('ip', 'app', 'device', 'os', 'channel', 'click_time', 'attributed_time', 'is_attributed')
df$day = as.POSIXlt(df$click_time)$wday
df$h = as.POSIXlt(df$click_time)$hour
######## Normalization ############
df$app = (df$app - mean(df$app))/sd(df$app)
df$ip = (df$ip - mean(df$ip))/sd(df$ip)
df$channel = (df$channel - mean(df$channel))/sd(df$channel)
df$device = (df$device - mean(df$device))/sd(df$device)
df$os = (df$os - mean(df$os))/sd(df$os)
df$day = (df$day - mean(df$day))/sd(df$day)
df$h = (df$h - mean(df$h))/sd(df$h)


######## Loading the model ############
model <- readRDS("./final_model.rds")
print(model)


######## Make a predictions on new test data using the final model ############
pred <- predict(model, df)


######## Confusion matrix ##############
results = seq(1, dim(pred)[1])
for (i in 1:dim(pred)[1]) {
  results[i] = c(0, 1)[which.max(pred[i,])]
}
pred = results

mean((pred - df$is_attributed)^2)  
u = union(pred, df$is_attributed)
t = table(factor(pred, u), factor(df$is_attributed, u))
confusionMatrix(t)
