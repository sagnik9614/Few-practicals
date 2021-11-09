#Try regression approach on the raw popularity score 
#Select the features filtered out after EDA
#Try with glmnet

features_new<-features
features_new[11]<-"popularity"
df_reg<-select(df_reg, features_new)
df_reg$acousticness<-scale(df_reg$acousticness, center = T, scale =T)
df_reg$danceability<-scale(df_reg$danceability, center = T, scale =T)
df_reg$duration_ms<-scale(df_reg$duration_ms, center = T, scale =T)
df_reg$energy<-scale(df_reg$energy, center = T, scale =T)
df_reg$liveness<-scale(df_reg$liveness, center = T, scale =T)
df_reg$loudness<-scale(df_reg$loudness, center = T, scale =T)
df_reg$speechiness<-scale(df_reg$speechiness, center = T, scale =T)
df_reg$tempo<-scale(df_reg$tempo, center = T, scale =T)
df_reg$valence<-scale(df_reg$valence, center = T, scale =T)
df_reg$popularity<-scale(df_reg$popularity, center = T, scale =T)
head(df_reg)

train.reg<-df_reg[train.index,]
test.reg<-df_reg[-train.index,]

formula_all<-popularity~genre+acousticness+danceability+duration_ms+energy+liveness+loudness+speechiness+tempo+valence
formula_all<-as.formula(formula_all)

design_train<-model.matrix(formula_all, train.reg)[,-1]
design_test<-model.matrix(formula_all, test.reg)[,-1]

foldid=sample(1:10,size=length(train.index),replace=TRUE)

glmfit_0<-cv.glmnet(design_train, y=train.reg$popularity, family = "gaussian", alpha = 0, foldid = foldid)
glmfit_0.5<-cv.glmnet(design_train, y=train.reg$popularity, family = "gaussian", alpha = 0.5, foldid = foldid)
glmfit_1<-cv.glmnet(design_train, y=train.reg$popularity, family = "gaussian", alpha = 1, foldid = foldid)


png(filename = "glmnet_plots.png", height = 800, width = 1200)
par(mfrow=c(2,2))
plot(glmfit_0, xvar = "lambda", sub = "alpha=0.0")
plot(glmfit_0.5, xvar = "lambda",  sub = "alpha=0.5")
plot(glmfit_1, xvar = "lambda",  sub = "alpha=1.0")
plot(log(glmfit_1$lambda),glmfit_1$cvm,pch=19,col="red",xlab="log(Lambda)",ylab=glmfit_1$name)
points(log(glmfit_0.5$lambda),glmfit_0.5$cvm,pch=19,col="grey")
points(log(glmfit_0$lambda),glmfit_0$cvm,pch=19,col="blue")
legend("topleft",legend=c("alpha= 1","alpha= .5","alpha 0"),pch=19,col=c("red","grey","blue"))
dev.off()

glm_final<-glmnet(x = design_train, y = train.reg$popularity, family = "gaussian", aplha = 0, lambda = glmfit_0$lambda.1se)

glmpred<-predict(glm_final, newx=design_test)
mae<-mean(abs(glmpred-test.reg$popularity))
rmse<-sqrt(mean((glmpred-test.reg$popularity)^2))

regpreds<-cbind(glmpred, test.reg$popularity)
colnames(regpreds)<-c("predicted", "actual")
regpreds<-as.data.frame(regpreds)
regpreds<-melt(regpreds, measure.vars = c("predicted","actual"))
png(filename = "testPopularity.png", width=1200, height=800)
grid.arrange(regpreds%>%ggplot(aes(x=value, col = variable, fill=variable))+geom_density(alpha=0.9),
regpreds%>%ggplot(aes(x=value, col = variable, fill=variable))+geom_histogram(alpha=0.9), bottom = "Figure 6")
dev.off()

