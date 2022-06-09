

xy_ohencoding = as.data.frame(cbind(X_mufeat, y_ohencoding))

Xtrain_ohencoding = xy_ohencoding[sel,]
Xtest_ohencoding = xy_ohencoding[-sel,]







gammalist <- c(0.005,0.01,0.015,0.02,0.025,0.03,0.035,0.04,0.045,0.05)

#blues
svm_blues = tune.svm(as.factor(blues) ~., data = Xtrain_ohencoding[,c(1:28,29)], kernel = 'radial', cost = 2^(-1:5), gamma = gammalist)

#classical
svm_classical = tune.svm(as.factor(classical) ~., data = Xtrain_ohencoding[,c(1:28,30)], kernel = 'radial', cost = 2^(-1:5), gamma = gammalist)

#country
svm_country = tune.svm(as.factor(country) ~., data = Xtrain_ohencoding[,c(1:28,31)], kernel = 'radial', cost = 2^(-1:5), gamma = gammalist)

#disco
svm_disco = tune.svm(as.factor(disco) ~., data = Xtrain_ohencoding[,c(1:28,32)], kernel = 'radial', cost = 2^(-1:5), gamma = gammalist)

#hiphop
svm_hiphop = tune.svm(as.factor(hiphop) ~., data = Xtrain_ohencoding[,c(1:28,33)], kernel = 'radial', cost = 2^(-1:5), gamma = gammalist)

#jazz
svm_jazz = tune.svm(as.factor(jazz) ~., data = Xtrain_ohencoding[,c(1:28,34)], kernel = 'radial', cost = 2^(-1:5), gamma = gammalist)

#metal
svm_metal = tune.svm(as.factor(metal) ~., data = Xtrain_ohencoding[,c(1:28,35)], kernel = 'radial', cost = 2^(-1:5), gamma = gammalist)

#pop
svm_pop = tune.svm(as.factor(pop) ~., data = Xtrain_ohencoding[,c(1:28,36)], kernel = 'radial', cost = 2^(-1:5), gamma = gammalist)

#reggae
svm_reggae = tune.svm(as.factor(reggae) ~., data = Xtrain_ohencoding[,c(1:28,37)], kernel = 'radial', cost = 2^(-1:5), gamma = gammalist)

#rock
svm_rock = tune.svm(as.factor(rock) ~., data = Xtrain_ohencoding[,c(1:28,38)], kernel = 'radial', cost = 2^(-1:5), gamma = gammalist)


multiSVM = list(svm_blues = svm_blues, svm_classical = svm_classical, svm_country = svm_country, svm_disco = svm_disco, svm_hiphop = svm_hiphop, svm_jazz = svm_jazz, svm_metal = svm_metal, svm_pop = svm_pop, svm_reggae = svm_reggae, svm_rock = svm_rock)



predict_matrix = c()
Y_prediction = list()
ConMatrix = list()

for(i in 1:10){
  y_pre = predict(multiSVM[[i]]$best.model, Xtest_ohencoding[,1:28])
  Y_prediction = lappend(Y_prediction, y_pre)
  Con_Matr = confusionMatrix(data = y_pre, reference = as.factor(Xtest_ohencoding[,28+1]))
  ConMatrix = lappend(ConMatrix, Con_Matr)
  acc = Con_Matr$overall[1]
  print(acc)
  predict_value = sapply(as.vector(y_pre), as.numeric)*acc
  predict_matrix = cbind(predict_matrix, predict_value)
}

predict_matrix = as.data.frame(predict_matrix)
colnames(predict_matrix) = genre
rownames(predict_matrix) = rownames(Xtest)


predict_cat = as.factor(colnames(predict_matrix)[apply(predict_matrix,1,which.max)])

ConMatrix_ohencoding = confusionMatrix(data = predict_cat, reference = as.factor(Xtest$label))

ConMatrix_ohencoding



y_pre = predict(multiSVM[[1]]$best.model, Xtest_ohencoding[,1:28])


Con_Matr = confusionMatrix(data = y_pre, reference = as.factor(Xtest_ohencoding[,28+1]))
acc = Con_Matr$overall[1]
predict_value = sapply(as.vector(y_pre), as.numeric)*acc

vec = cbind(predict_value,c(1:200))

Con_Matr




lappend <- function (lst, ...){
  lst <- c(lst, list(...))
  return(lst)
}

lst = list(a=1,b=2,c=3)


lst = lappend(lst, df)



df = as.data.frame(matrix(c(1,2,3,4,
                            1,2,4,3,
                            1,4,3,2,
                            4,3,2,1), ncol=4,nrow=4,byrow = T))

max_v = as.factor(colnames(df)[apply(df,1,which.max)])
