

matrix_std = function(df,p){
  pn = ncol(df)
  df_std = df[,1:p]
  df_std = scale(df_std)
  df_std = as.data.frame(df_std)
  df_std = cbind(df_std, df[,(p+1):pn])
  df_std = as.data.frame(df_std)
  return(df_std)
}

xy_ohencoding = as.data.frame(cbind(X_mufeat, y_ohencoding))

Xtrain_ohencoding_std = matrix_std(xy_ohencoding[sel,],28)
Xtest_ohencoding_std = matrix_std(xy_ohencoding[-sel,],28)







gammalist <- c(0.005,0.01,0.015,0.02,0.025,0.03,0.035,0.04,0.045,0.05)

#blues
svm_blues_std = tune.svm(as.factor(blues) ~., data = Xtrain_ohencoding_std[,c(1:28,29)], kernel = 'radial', cost = 2^(-1:5), gamma = gammalist)

#classical
svm_classical_std = tune.svm(as.factor(classical) ~., data = Xtrain_ohencoding_std[,c(1:28,30)], kernel = 'radial', cost = 2^(-1:5), gamma = gammalist)

#country
svm_country_std = tune.svm(as.factor(country) ~., data = Xtrain_ohencoding_std[,c(1:28,31)], kernel = 'radial', cost = 2^(-1:5), gamma = gammalist)

#disco
svm_disco_std = tune.svm(as.factor(disco) ~., data = Xtrain_ohencoding_std[,c(1:28,32)], kernel = 'radial', cost = 2^(-1:5), gamma = gammalist)

#hiphop
svm_hiphop_std = tune.svm(as.factor(hiphop) ~., data = Xtrain_ohencoding_std[,c(1:28,33)], kernel = 'radial', cost = 2^(-1:5), gamma = gammalist)

#jazz
svm_jazz_std = tune.svm(as.factor(jazz) ~., data = Xtrain_ohencoding_std[,c(1:28,34)], kernel = 'radial', cost = 2^(-1:5), gamma = gammalist)

#metal
svm_metal_std = tune.svm(as.factor(metal) ~., data = Xtrain_ohencoding_std[,c(1:28,35)], kernel = 'radial', cost = 2^(-1:5), gamma = gammalist)

#pop
svm_pop_std = tune.svm(as.factor(pop) ~., data = Xtrain_ohencoding_std[,c(1:28,36)], kernel = 'radial', cost = 2^(-1:5), gamma = gammalist)

#reggae
svm_reggae_std = tune.svm(as.factor(reggae) ~., data = Xtrain_ohencoding_std[,c(1:28,37)], kernel = 'radial', cost = 2^(-1:5), gamma = gammalist)

#rock
svm_rock_std = tune.svm(as.factor(rock) ~., data = Xtrain_ohencoding_std[,c(1:28,38)], kernel = 'radial', cost = 2^(-1:5), gamma = gammalist)


multiSVM_std = list(svm_blues = svm_blues_std, svm_classical = svm_classical_std, svm_country = svm_country_std, svm_disco = svm_disco_std, svm_hiphop = svm_hiphop_std, svm_jazz = svm_jazz_std, svm_metal = svm_metal_std, svm_pop = svm_pop_std, svm_reggae = svm_reggae_std, svm_rock = svm_rock_std)



predict_matrix_std = c()
Y_prediction_std = list()
ConMatrix_std = list()

for(i in 1:10){
  y_pre = predict(multiSVM_std[[i]]$best.model, Xtest_ohencoding_std[,1:28])
  Y_prediction_std = lappend(Y_prediction_std, y_pre)
  Con_Matr = confusionMatrix(data = y_pre, reference = as.factor(Xtest_ohencoding_std[,28+1]))
  ConMatrix_std = lappend(ConMatrix_std, Con_Matr)
  acc = Con_Matr$overall[1]
  print(acc)
  predict_value = sapply(as.vector(y_pre), as.numeric)*acc
  predict_matrix_std = cbind(predict_matrix_std, predict_value)
}

predict_matrix_std = as.data.frame(predict_matrix_std)
colnames(predict_matrix_std) = genre
rownames(predict_matrix_std) = rownames(Xtest)


predict_cat_std = as.factor(colnames(predict_matrix_std)[apply(predict_matrix_std,1,which.max)])

ConMatrix_ohencoding_std = confusionMatrix(data = predict_cat_std, reference = as.factor(Xtest$label))

ConMatrix_ohencoding_std



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
