# Create Training and Testing data

mu_feat = read.csv("D:/NDHU_Master/110-2/KML/Project/Data_sets/data.csv", row.names = 1)

X_mufeat = mu_feat[,-29]
Y_mufeat = as.data.frame(mu_feat[,'label'])
colnames(Y_mufeat) = 'label'
feat = colnames(X_mufeat)

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




set.seed(10)
sel = multiSample(1000,10,75)




mu_feat_nu = mu_feat
mu_feat_nu$label = Y_nu
colnames(mu_feat_nu[,29]) = 'label'

Xtrain_nu = mu_feat_nu[sel,]
Xtest_nu = mu_feat_nu[-(sel),]

colnames(Xtrain_nu) = colnames(Xtest_nu) = colnames(mu_feat)
