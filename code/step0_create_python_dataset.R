# Regularized logistic regression, decide lambda
Full_Deps = data_naomit$Full*data_naomit$Deps
Part_Deps = data_naomit$Part*data_naomit$Deps
datapython = data_naomit
datapython = as.data.frame(datapython)
datapython = cbind.data.frame(datapython,Full_Deps,Part_Deps)
head(datapython)
datapython = datapython[,-c(1,6,8)]
write.csv(datapython,"datapython.csv",row.names=FALSE)