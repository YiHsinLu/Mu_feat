# Naive regression



multiSample = function(n,g,k){
  # totally n members
  # for g groups
  # take k members from each group
  if(n%%g==0){
    ms = c()
    ng = n/g
    for(i in 1:g){
      init = (i-1)*ng+1
      ms = cbind(ms, sample(init:(i*ng), k, replace = FALSE))
    }
    return(sort(ms))
  }else{
    print('Error: n could not divided by g')
  }
}







Y_nu = as.data.frame(sapply(factor(Y_mufeat$label), unclass))
colnames(Y_nu) = 'ladel'
rownames(Y_nu) = rownames(Y_mufeat) = rownames(X_mufeat)




set.seed(610911007)
sel = multiSample(1000,10,75)




mu_feat_nu = mu_feat
mu_feat_nu$label = Y_nu
colnames(mu_feat_nu[,29]) = 'label'

Xtrain_nu = mu_feat_nu[sel,]
Xtest_nu = mu_feat_nu[-(sel),]

colnames(Xtrain_nu) = colnames(Xtest_nu) = colnames(mu_feat)




Xtrain_scale = scale(Xtrain_nu[,1:28])
Xtest_scale = scale(Xtest_nu[,1:28])




classifier = naiveBayes(label ~ ., data = Xtrain_nu)




# Predicting on test data'
Y_pred <- predict(classifier, newdata = Xtest_nu)





# Confusion Matrix
cm <- table(Xtest_nu$label, Y_pred)
cm




# Model Evaluation
confusionMatrix(cm)

