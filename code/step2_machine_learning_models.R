sample_size = dim(data_naomit)[1]
index = sample(sample_size,sample_size,replace = FALSE)
train_size = floor(0.6*sample_size)
validation_size = floor(0.2*sample_size)
test_size = sample_size - train_size - validation_size

train = data_naomit[index[1:train_size],]
validate = data_naomit[index[(train_size+1):(train_size+validation_size)],]
test = data_naomit[index[(train_size+validation_size+1):sample_size],]

# Linear Discriminant Analysis
ldamodel=lda(screening~Age + Alcohol + Height + Weight + Deps + Full + Part + Team2 + Team3
             + Team4 + Team5 + Team6 + Team7 + Team8 + Team9 + Team10 + Team11 + Full:Deps + Part:Deps + BMI
             ,data=train)
ldapred = predict(ldamodel,validate)
ldaclass = ldapred$class
table(ldaclass,validate$screening)
mean(ldaclass==validate$screening)

# Confusion Matrix
ldapred = predict(ldamodel,test)
predictions = ifelse(ldapred$posterior[,"Y"]>0.5,"Y","N")
predictions = as.factor(predictions)
screening = test$screening
screening = as.factor(screening)
out = capture.output(confusionMatrix(predictions, screening , positive = "Y", dnn = c("Prediction", "True")))
write.table(out,"output/lda_confusion_matrix.txt")

# Boosting

boostmodel1 = gbm(is_screen~Age + Alcohol + Height + Weight + Deps + Full + Part + Team2 + Team3
                 + Team4 + Team5 + Team6 + Team7 + Team8 + Team9 + Team10 + Team11 + BMI + Full:Deps + Part:Deps
                 ,data = train,distribution = "bernoulli", n.trees = 5000, interaction.depth = 1,
                 shrinkage=0.01,verbose=F)
boostmodel2 = gbm(is_screen~Age + Alcohol + Height + Weight + Deps + Full + Part + Team2 + Team3
                  + Team4 + Team5 + Team6 + Team7 + Team8 + Team9 + Team10 + Team11 + BMI+Full:Deps + Part:Deps
                  ,data = train,distribution = "bernoulli", n.trees = 5000, interaction.depth = 2,
                  shrinkage=0.01,verbose=F)
boostmodel3 = gbm(is_screen~Age + Alcohol + Height + Weight + Deps + Full + Part + Team2 + Team3
                  + Team4 + Team5 + Team6 + Team7 + Team8 + Team9 + Team10 + Team11 + BMI+Full:Deps + Part:Deps
                  ,data = train,distribution = "bernoulli", n.trees = 5000, interaction.depth = 3,
                  shrinkage=0.01,verbose=F)
boostmodel4 = gbm(is_screen~Age + Alcohol + Height + Weight + Deps + Full + Part + Team2 + Team3
                  + Team4 + Team5 + Team6 + Team7 + Team8 + Team9 + Team10 + Team11 + BMI+Full:Deps + Part:Deps
                  ,data = train,distribution = "bernoulli", n.trees = 5000, interaction.depth = 4,
                  shrinkage=0.01,verbose=F)

yhat_boost1=ifelse(predict(boostmodel1,newdata=validate,n.trees=5000,type = "response")>0.5,1,0)
yhat_boost2=ifelse(predict(boostmodel2,newdata=validate,n.trees=5000,type = "response")>0.5,1,0)
yhat_boost3=ifelse(predict(boostmodel3,newdata=validate,n.trees=5000,type = "response")>0.5,1,0)
yhat_boost4=ifelse(predict(boostmodel4,newdata=validate,n.trees=5000,type = "response")>0.5,1,0)

mean(yhat_boost1 == validate$is_screen)
mean(yhat_boost2 == validate$is_screen)
mean(yhat_boost3 == validate$is_screen)
mean(yhat_boost4 == validate$is_screen)

summary(boostmodel4)

# Confusion Matrix
predictions = ifelse(predict(boostmodel4,newdata=test,n.trees=5000,type = "response")>0.5,"Y","N")
predictions = as.factor(predictions)
screening = test$screening
screening = as.factor(screening)
out = capture.output(confusionMatrix(predictions, screening, positive = "Y", dnn = c("Prediction", "True")))
write.table(out,"output/boosting_confusion_matrix.txt")


# Logistic Regression
glmmodel=glm(formula(bothways),data=train,family = binomial)
summary(glmmodel)
glmprobs=predict(glmmodel,test,type="response")
glmpred=rep("N",test_size)
glmpred[glmprobs>.5]="Y"

mean(glmpred == test$screening)
table(glmpred,test$screening)

# Confusion Matrix
predictions = glmpred
predictions = as.factor(predictions)
screening = test$screening
screening = as.factor(screening)
out = capture.output(confusionMatrix(predictions, screening , positive = "Y", dnn = c("Prediction", "True")))
write.table(out,"output/glm_confusion_matrix.txt")


# Plot ROC
glmroc = rocit(score=glmprobs,class=test$is_screen)
glmroc$AUC

pdf("plot/roc.pdf",height = 1.9685*2,width = 1.9685*2,pointsize = 12)
plot(glmroc$FPR,glmroc$TPR,xlab = "1-Specificity(FPR)",ylab = "Sensitivity(TPR)",las = 1,type = "l",
     main = "Performance Comparison on Test Dataset",cex.main = 0.8, cex.lab= 0.8, cex.axis = 0.8)

yhat_boost4=ifelse(predict(boostmodel4,newdata=test,n.trees=5000,type = "response")>0.5,1,0)
boostroc = rocit(score=yhat_boost4,class=test$is_screen)
boostroc$AUC
lines(boostroc$FPR,boostroc$TPR,col = "red")

ldapred = ifelse(predict(ldamodel,test)$posterior[,2]>0.5,1,0)
ldaroc = rocit(score=ldapred,class=test$is_screen)
ldaroc$AUC
lines(ldaroc$FPR,ldaroc$TPR,col = "blue")
abline(a = 0,b = 1,lty = 2)
legend("bottomright", legend=c("Logistic Regression (AUC = 0.86)", "Boosting (AUC = 0.94)","LDA (AUC = 0.94)"),
       col=c("black","red","blue"), lty=rep(1,3), cex=0.6)
dev.off()

# Marginal Effects and Influence
pdf("plot/boost.pdf",pointsize = 3)
summary(boostmodel4)
dev.off()


# Combined Method
check = function(a,b,c)
{
  return(a==1 ||b==1 ||c==1 )
}
for(i in 1:97){predictions[i] = ifelse(check(glmpred[i],ldapred[i],yhat_boost4[i]),"Y","N")}
predictions = as.factor(predictions)
out = capture.output(confusionMatrix(predictions, screening , positive = "Y", dnn = c("Prediction", "True")))
write.table(out,"output/combined_confusion_matrix.txt")

