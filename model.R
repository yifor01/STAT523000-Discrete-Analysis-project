## 用UTF-8 開
library(GGally);library(tidyverse);library(beepr)
library(corrplot);library(dummies);library(xgboost)
library(glmnet);library(pROC);library(glmnet);library(e1071)
library(randomForest);library(plotROC)
library(ROCR);library(caret);library(parallel)
detectCores()
#cl <- makeCluster(3)

# LOL data
dat = read.csv("games.csv")
dat2 = read.csv("champs.csv")
dat1 = dat[,c(5,3,6:61)]


ggplot(dat1,aes(x=factor(dat1$t1_champ1id))) +geom_histogram(stat="count")+  coord_flip()


dat1 %>%  count(t1_champ1id)%>% ggplot(aes(x=fct_reorder(factor(t1_champ1id),n) ,y=n ) ) +
  geom_bar(stat="identity")+ labs(x="champion",y="count") + coord_flip()

# 英雄出場頻率
aq = 
  (dat1 %>%  filter(winner==1) %>% select(t1_champ1id) %>% count(t1_champ1id))+
  (dat1 %>%  filter(winner==1) %>%  select(t1_champ2id) %>% count(t1_champ2id))+
  (dat1 %>%  filter(winner==1) %>%  select(t1_champ3id) %>% count(t1_champ3id))+
  (dat1 %>%  filter(winner==1) %>%  select(t1_champ4id) %>% count(t1_champ4id))+
  (dat1 %>%  filter(winner==1) %>%  select(t1_champ5id) %>% count(t1_champ5id))+
  (dat1 %>%  filter(winner==2) %>%  select(t2_champ1id) %>% count(t2_champ1id))+
  (dat1 %>%  filter(winner==2) %>%  select(t2_champ2id) %>% count(t2_champ2id))+
  (dat1 %>%  filter(winner==2) %>%  select(t2_champ3id) %>% count(t2_champ3id))+
  (dat1 %>%  filter(winner==2) %>%  select(t2_champ4id) %>% count(t2_champ4id))+
  (dat1 %>%  filter(winner==2) %>%  select(t2_champ5id) %>% count(t2_champ5id))
aa = 
  (dat1 %>%  select(t1_champ1id) %>% count(t1_champ1id))+
  (dat1 %>%  select(t1_champ2id) %>% count(t1_champ2id))+
  (dat1 %>%  select(t1_champ3id) %>% count(t1_champ3id))+
  (dat1 %>%  select(t1_champ4id) %>% count(t1_champ4id))+
  (dat1 %>%  select(t1_champ5id) %>% count(t1_champ5id))+
  (dat1 %>%  select(t2_champ1id) %>% count(t2_champ1id))+
  (dat1 %>%  select(t2_champ2id) %>% count(t2_champ2id))+
  (dat1 %>%  select(t2_champ3id) %>% count(t2_champ3id))+
  (dat1 %>%  select(t2_champ4id) %>% count(t2_champ4id))+
  (dat1 %>%  select(t2_champ5id) %>% count(t2_champ5id))

aa[,1] = aq[,1]  = arrange(dat2,id)[,1]
# 英雄總出場次數
arrange(aa,desc(n))[1:10,]
aa %>% ggplot(aes(x=fct_reorder(factor(t1_champ1id),n) ,y=n ) ) +
  geom_bar(stat="identity")+ labs(x="champion",y="count") + coord_flip(xlim = c(130.4,138) )

# 英雄勝率
rate = arrange(data.frame("id"= arrange(dat2,id)[,1],"rate"= aq[,2]/aa[,2]),rate)
data.frame("id"= rev(rate$id),"rate" = rev(rate$rate) )[1:10,]

# EDA
corrplot(cor(dat1[,c(1,2,3,4,5,6,7,8,24,25,26,27,28,49,50,51,52,53)]))
eda = data.frame(sapply(c(1,3:8), function(i) as.factor(dat1[,i]) ),dat1$gameDuration,
                 dat1$t1_towerKills,dat1$t1_inhibitorKills,dat1$t1_baronKills,
                 dat1$t1_dragonKills,dat1$t1_riftHeraldKills,
                 dat1$t2_towerKills,dat1$t2_inhibitorKills,dat1$t2_baronKills,
                 dat1$t2_dragonKills,dat1$t2_riftHeraldKills)
colnames(eda) = colnames(dat1[,c(1,3:8,2,24:28,49:53)])
summary(eda)

#ggpairs(eda)

eda1 = data.frame("winner"=as.numeric( rep(dat1$winner,2)),
                  "towerKills"=c(dat1$t1_towerKills,dat1$t2_towerKills),
                  "team"= c(rep(1,length(dat1$t1_towerKills)),
                            rep(2,length(dat1$t2_towerKills) )))
eda2 = data.frame("winner"=as.numeric( rep(dat1$winner,2)),
                  "inhibitorKills"=c(dat1$t1_inhibitorKills,dat1$t2_inhibitorKills),
                  "team"= c(rep(1,length(dat1$t1_inhibitorKills)),
                            rep(2,length(dat1$t2_inhibitorKills) )))

ggplot(eda1,aes(x=factor(winner-1),y=towerKills,fill=factor(team)  )) +geom_boxplot()+
  scale_fill_manual(breaks = c("1", "2"), values=c("#0000FF", "#FF0000")) +
  labs(x="winner",y="towerKills",fill="team")+
  ggtitle("Winner vs. TowerKills")

ggplot(eda2,aes(x=factor(winner-1),y=inhibitorKills,fill=factor(team)  )) +geom_boxplot()+
  scale_fill_manual(breaks = c("1", "2"), values=c("#0000FF", "#FF0000")) +
  labs(x="winner",y="inhibitorKills",fill="team")+
  ggtitle("Winner vs. InhibitorKills")

###############################################
# 資料處理

tc1 = matrix(0,ncol=138*4,nrow = nrow(dat) )
for(i in 1:nrow(dat) ){
  tc1[i,which(dat2$id== dat1$t1_champ1id[i])]=1
  tc1[i,which(dat2$id== dat1$t1_champ2id[i])]=1
  tc1[i,which(dat2$id== dat1$t1_champ3id[i])]=1
  tc1[i,which(dat2$id== dat1$t1_champ4id[i])]=1
  tc1[i,which(dat2$id== dat1$t1_champ5id[i])]=1
  tc1[i,which(dat2$id== dat1$t2_champ1id[i])+138]=1
  tc1[i,which(dat2$id== dat1$t2_champ2id[i])+138]=1
  tc1[i,which(dat2$id== dat1$t2_champ3id[i])+138]=1
  tc1[i,which(dat2$id== dat1$t2_champ4id[i])+138]=1
  tc1[i,which(dat2$id== dat1$t2_champ5id[i])+138]=1
  tc1[i,which(dat2$id== dat1$t1_ban1[i])+138*2]=1
  tc1[i,which(dat2$id== dat1$t1_ban2[i])+138*2]=1
  tc1[i,which(dat2$id== dat1$t1_ban3[i])+138*2]=1
  tc1[i,which(dat2$id== dat1$t1_ban4[i])+138*2]=1
  tc1[i,which(dat2$id== dat1$t1_ban5[i])+138*2]=1
  tc1[i,which(dat2$id== dat1$t2_ban1[i])+138*3]=1
  tc1[i,which(dat2$id== dat1$t2_ban2[i])+138*3]=1
  tc1[i,which(dat2$id== dat1$t2_ban3[i])+138*3]=1
  tc1[i,which(dat2$id== dat1$t2_ban4[i])+138*3]=1
  tc1[i,which(dat2$id== dat1$t2_ban5[i])+138*3]=1
}
tc1 = data.frame(tc1)

colnames(tc1)=c(paste0("t1",dat2$name),paste0("t2",dat2$name),
                paste0("ban1",dat2$name),paste0("ban2",dat2$name))

tc =tc1[,-c(138,138*2,138*3,138*4)]

sc1 = 
  
  
  X = data.frame(scale(dat1$gameDuration),
                 dummy(dat1$firstBlood)[,-1],
                 dummy(dat1$firstTower)[,-1],
                 dummy(dat1$firstInhibitor)[,-1],
                 dummy(dat1$firstBaron)[,-1],
                 dummy(dat1$firstDragon)[,-1],
                 tc,
                 scale(dat1$t1_baronKills),
                 scale(dat1$t1_dragonKills),
                 scale(dat1$t1_riftHeraldKills),
                 scale(dat1$t2_baronKills),
                 scale(dat1$t2_dragonKills), 
                 scale(dat1$t2_riftHeraldKills) ) 


colnames(X)[1:11] = c("gameDuration","firstBlood0","firstBlood1",
                      "firstTower0","firstTower1","firstInhibitor0","firstInhibitor1",
                      "firstBaron0","firstBaron1","firstDragon0","firstDragon1")
colnames(X)[560:565] = c("t1_baronKills","t1_dragonKills","t1_riftHeraldKills",
                         "t2_baronKills","t2_dragonKills","t2_riftHeraldKills" )

y = dat1$winner-1

train = sample(1:nrow(X),0.8*nrow(X),replace = F)

##########################################################################################
# 資料分析
# model 1 (GLM)
#--------------------------
# tuning parameter(lambda)
ff1 = function(){
  cv = cv.glmnet(as.matrix(X[train,]),y[train],nfolds=5,family = "binomial",
                 type.measure="auc",alpha=1,standardize = FALSE)
  as.numeric( predict(cv,type="response",  newx = as.matrix(X[-train,]),s ='lambda.min'))
}
ac1 = t(sapply(1:100,function(i) ff1() ))

# 選變數
cv1 = cv.glmnet(as.matrix(X[train,]),y[train],nfolds=5,family = "binomial",
                type.measure="auc",alpha=1,standardize = FALSE)

plot(cv1)
abline(v=log(cv1$lambda.min),col=2,lty=2)

plot(cv1$glmnet.fit,xvar="lambda",label=T,main="Solution Path" )
abline(v=log(cv1$lambda.min),lty=2)


beta1 = cv1$glmnet.fit$beta[,which(cv1$glmnet.fit$lambda==cv1$lambda.min)]
colnames(X)[which(beta1!=0)]
cutvalue = 0.25

varplot =   data.frame("variable" = colnames(X)[ as.numeric( which(abs(beta1)>cutvalue)) ]  ,
                       "log odds ratio"= as.numeric( beta1[which(abs(beta1)>cutvalue)]) ) %>% 
  arrange(desc( abs( as.numeric( beta1[which(abs(beta1)>cutvalue)]) )  )) 

varplot = varplot %>% mutate(sign= factor( as.numeric( log.odds.ratio>0)*2-1 ) )
varplot  %>%  ggplot(aes(x=variable,y= log.odds.ratio,fill=sign)) +geom_col() +
  coord_flip( ) + geom_vline(xintercept = 7.5,lty=2)+ 
  geom_vline(xintercept = 16.5,lty=2) + ggtitle("GLM Importance Variables( >0.25)") + 
  scale_y_continuous("log odds ratio", breaks = seq(-2, 2, by = .1)) 

# AUC (table+ROC)
table(y[-train] , as.numeric( apply(ac1,2,mean)>0.5 ))
rc1 = data.frame( A=y[-train],B = apply(ac1,2,mean)   )

basicplot1 = ggplot(rc1, aes(d = A, m = B)) + geom_roc() 
basicplot1 + style_roc(theme = theme_grey) +
  theme(axis.text = element_text(colour = "blue")) +
  ggtitle("GLM (Lassp) ROC curve") +
  annotate("text", x = .75, y = .29, 
           label = paste("AUC =",round(calc_auc(basicplot1)$AUC, 4) )) +
  annotate("text", x = .785, y = .25, 
           label = paste("Predict error =",round(mean(as.numeric(rc1$B>0.5)==rc1$A), 4)  )) +
  scale_x_continuous("Specificity", breaks = seq(0, 1, by = .1)) +
  scale_y_continuous("Sensitivity", breaks = seq(0, 1, by = .1)) 
#=======================================================================
# model 2 (SVM)
#--------------------------
# tuning parameter(SVM)
data = data.frame(y,X)
#---------------------------
fit2 = svm(y~.,data=data[train,],type="C-classification",probability = T)

num2 = predict(fit2 ,data[-train,-1],decision.values=TRUE)

rc2 = data.frame( A=y[-train],B = -as.numeric( attr(num2,"decision.values"))   )

table(factor(num2),y[-train])

basicplot1 = ggplot(rc2, aes(d = A, m = B)) + geom_roc()
basicplot1 + 
  style_roc(theme = theme_grey) +
  theme(axis.text = element_text(colour = "blue")) +
  ggtitle("SVM ROC curve") + 
  annotate("text", x = .75, y = .29, 
           label = paste("AUC =",round(calc_auc(basicplot1)$AUC, 4) )) +
  annotate("text", x = .783, y = .25, 
           label = paste("Predict error =",
                         round(mean(y[-train]==as.numeric(as.numeric(attr(num2,"decision.values"))<0)), 4)  )) +
  scale_x_continuous("Specificity", breaks = seq(0, 1, by = .1))+
  scale_y_continuous("Sensitivity", breaks = seq(0, 1, by = .1)) 



#=======================================================================
# model 3 (XGboost)
#--------------------------
# tuning parameter(XG)
dtrain = xgb.DMatrix(data = as.matrix(X[train,]),label = y[train])
dtest = xgb.DMatrix(data = as.matrix(X[-train,]),label = y[-train])

xgb.params = list( colsample_bytree = 1, subsample = 1,booster = "gbtree",  max_depth = 2,
                   eta = 0.03,
                   eval_metric = "auc",objective = "reg:logistic",  gamma = 0)   

cv.model = xgb.cv(params = xgb.params, data = dtrain,
                  nfold = 5, nrounds=200,early_stopping_rounds = 30, print_every_n = 20 )

tmp = cv.model$evaluation_log
plot(x=1:nrow(tmp), y= tmp$train_auc_mean, col='red', xlab="nround", ylab="auc", 
     main="Avg.Performance in CV") 
points(x=1:nrow(tmp), y= tmp$test_auc_mean, col='blue') 
legend("bottomright", pch=1, col = c("red", "blue"), 
       legend = c("Train", "Validation") )

cv.model$best_iteration
#--------------------------
ff3 = function(){
  fit = xgboost(as.matrix(X[train,]),label = y[train],objective = "reg:logistic", 
                eval_metric = "auc",eta = 0.03,max_depth = 2,gamma = 0,
                nrounds = 200,verbose = 0)
  as.numeric( predict(fit,as.matrix(X[-train,]) ))
}
ac3 = t(sapply(1:100,function(i) ff3() ))

# 選變數
fit3 = xgboost(as.matrix(X[train,]),label = y[train],objective = "reg:logistic", 
               eval_metric = "auc",eta = 0.03,max_depth = 2,gamma = 0,
               nrounds = 200,verbose = 0)
xgb.plot.multi.trees(fit3,feature_names = names(X))
importance_matrix <- xgb.importance(names(X), model = fit3)
xgb.plot.importance(importance_matrix,xlab="Gain",main="XGboost Importance Variables")
importance_matrix

# 預測
table(y[-train] , as.numeric(apply(ac3,2,mean)>0.5) )
rc3 = data.frame( A=y[-train],B = apply(ac3,2,mean)   )

basicplot3 = ggplot(rc3, aes(d = A, m = B)) + geom_roc() 
basicplot3 + style_roc(theme = theme_grey) +
  theme(axis.text = element_text(colour = "blue")) +
  ggtitle("XGboost ROC curve") + 
  annotate("text", x = .75, y = .29, 
           label = paste("AUC =",round(calc_auc(basicplot3)$AUC, 4) )) +
  annotate("text", x = .783, y = .25, 
           label = paste("Predict error =",round(mean(as.numeric(rc3$B>0.5)==rc3$A), 4)  )) +
  scale_x_continuous("Specificity", breaks = seq(0, 1, by = .1))+
  scale_y_continuous("Sensitivity", breaks = seq(0, 1, by = .1)) 



#=======================================================================
#model 4 (RF)
#--------------------------
# tuning parameter(RF)
#data = data.frame(y,X)
#fit4.1 = randomForest(y~.,data=data[train,],ntree=1000)
#plot(fit4.1)
#tuneRF(data[train,-1], data[train,1])
#--------------------------
fit4 = randomForest(y~.,data=data[train,],ntree=300,mytry=93)
num4 = predict(fit4,data[-train,-1] )

varImpPlot(fit4)

rc4 = data.frame( A=y[-train],B = as.numeric( num4)   )

table(as.numeric(num4>0.5),y[-train])

basicplot4 = ggplot(rc4, aes(d = A, m = B)) + geom_roc()
basicplot4 + 
  style_roc(theme = theme_grey) +
  theme(axis.text = element_text(colour = "blue")) +
  ggtitle("RF ROC curve") + 
  annotate("text", x = .75, y = .29, 
           label = paste("AUC =",round(calc_auc(basicplot4)$AUC, 4) )) +
  annotate("text", x = .783, y = .25, 
           label = paste("Predict error =",
                         round(mean(y[-train]==as.numeric(num4>0.5)), 4)  )) +
  scale_x_continuous("Specificity", breaks = seq(0, 1, by = .1))+
  scale_y_continuous("Sensitivity", breaks = seq(0, 1, by = .1)) 
#=======================================================================
# model 5 (group lasso)
#-----------------------------------------------------------------------
library(grpreg)
index = c(0,1,1,2,2,3,3,4,4,5,5,
          6:(5+137),6:(5+137),143:(142+137),143:(142+137),
          280:282,280:282)

# tuning parameter (lambda)
cv5 = cv.grpreg(X = as.matrix(X[train,]),y = y[train],nfolds = 5,
                family = "binomial",group = index,trace = T)

#plot(cv5)

#abline(v = log(cv5$lambda.min),col = 2,lty = 2)

plot(cv5$fit,xvar = "lambda",label = F,main ="Solution Path",log.l = T)
abline(v = log(cv5$lambda.min),lty = 2)

beta2 = cv5$fit$beta[,which(cv5$fit$lambda==cv5$lambda.min)]
colnames(X)[which(beta2!=0)]

cutvalue = 0.25 
varplot = data.frame("variable" = colnames(X)[as.numeric( which(abs(beta2)>cutvalue))],
                     "log odds ratio" = as.numeric( beta2[which(abs(beta2)>cutvalue)])) %>%
  arrange(desc(abs(as.numeric(beta2[which(abs(beta2)>cutvalue)])))) 
varplot = varplot %>% mutate(sign= factor( as.numeric(log.odds.ratio>0)*2-1))
varplot  %>%  ggplot(aes(x = variable,y = log.odds.ratio,fill = sign)) + geom_col() +
  coord_flip( ) + geom_vline(xintercept = 7.5,lty = 2) + 
  geom_vline(xintercept = 11.5,lty = 2) + ggtitle("GLM Importance Variables( >0.25)") + 
  scale_y_continuous("log odds ratio", breaks = seq(-1,1, by = .1)) 


# AUC (table + ROC)
table(y[-train] , as.numeric(predict(cv5,type = "response",
                                     X = as.matrix(X[-train,]),s ='lambda.min')>0.5))
rc9 = data.frame(A = y[-train],
                 B = as.numeric(predict(cv5,type = "response",
                                        X= as.matrix(X[-train,]),s ='lambda.min')))

basicplot9 = ggplot(rc9, aes(d = A, m = B)) + geom_roc() 
basicplot9 + style_roc(theme = theme_grey) +
  theme(axis.text = element_text(colour = "blue")) +
  ggtitle("GLM (Glasso) ROC curve") +
  annotate("text", x = .75, y = .29, 
           label = paste("AUC =",round(calc_auc(basicplot9)$AUC, 4) )) +
  annotate("text", x = .785, y = .25, 
           label = paste("Predict error =",round(mean(as.numeric(rc9$B>0.5)==rc9$A), 4)  )) +
  scale_x_continuous("Specificity", breaks = seq(0, 1, by = .1)) +
  scale_y_continuous("Sensitivity", breaks = seq(0, 1, by = .1)) 


#-----------------------------------------------------------------------------
# 100 times average

ff5 = function(i){
  cv = cv.grpreg(X = as.matrix(X[train,]),y = y[train],nfolds = 5,
                 family = "binomial",group = index)
  cat(i,'% ')
  return(as.numeric( predict(cv,type="response",  X = as.matrix(X[-train,]),s ='lambda.min')))
}
ac5 = t(sapply(1:100,function(i) ff5(i) ))

table(y[-train] , as.numeric( apply(ac5,2,mean)>0.5 ))
rc5 = data.frame( A=y[-train],B = apply(ac5,2,mean)   )

basicplot5 = ggplot(rc5, aes(d = A, m = B)) + geom_roc() 
basicplot5 + style_roc(theme = theme_grey) +
  theme(axis.text = element_text(colour = "blue")) +
  ggtitle("GLM (Glasso) ROC curve") +
  annotate("text", x = .75, y = .29, 
           label = paste("AUC =",round(calc_auc(basicplot5)$AUC, 4) )) +
  annotate("text", x = .785, y = .25, 
           label = paste("Predict error =",round(mean(as.numeric(rc5$B>0.5)==rc5$A), 4)  )) +
  scale_x_continuous("Specificity", breaks = seq(0, 1, by = .1)) +
  scale_y_continuous("Sensitivity", breaks = seq(0, 1, by = .1)) 

