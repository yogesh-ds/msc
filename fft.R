library(FFTrees)

train <- cd[indices, ]
test <- cd[!indices, ]

train$flag <- ifelse(train$flag==1,T,F)
test$flag <- ifelse(test$flag==1,T,F)



heart.fft <- FFTrees(formula = flag ~.,
                     data = train,
                     data.test = test,
                     main = "Heart Disease",
                     decision.labels = c("Healthy", "Diseased"))

# Visualize the best training tree applied to the test data
plot(heart.fft, data = "test")
heart.fft

pred <-predict(heart.fft, newdata = test, type = "prob")
