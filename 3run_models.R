library(mgcv)
library(gbm)

Y <- data[,c("id" ,"h_index","i10_index" ,"nb_articles" ,"nb_journals" ,"gender" ,"age" ,
             "dist_erdos" ,"betw" ,"deg" ,"close" ,"eigen" ,"page" ,"n2", "bona", "max_d1", "max_d2", "c_louvain", "c_walk" ,"c_label", 
             "c_spin")]
Y <- na.omit(Y)
Y <- subset(Y, !(h_index == 0))

Y$dist_erdos <- as.factor(Y$dist_erdos)
Y$dist_erdos <- as.factor(Y$dist_erdos)
Y$c_louvain <- as.factor(Y$c_louvain)
Y$c_walk <- as.factor(Y$c_walk)
Y$c_label <- as.factor(Y$c_label)
Y$c_spin <- as.factor(Y$c_spin)
Y$gender <- as.factor(Y$gender)
Y$genderbin <- as.logical(Y$gender == "female")
Y$activity <- as.numeric(Y$nb_articles / (Y$age + 1))

err_lm <- c()
err_boost <- c()
err_gam <- c()
err_rel_lm <- c()
err_rel_boost <- c()
err_rel_gam <- c()

ntrain <- 100
N <- nrow(Y) - 1
perc <- 0.9
M <- floor(0.8*N)

for(i in 1:ntrain){
  if(i %% 10 == 1){print(paste("Beginning training",i,"out of",ntrain))}
  ns <- sample(2:(N+1))
  train <- Y[c(1,ns[1:M]),]
  test <- Y[ns[(M+1):N],]
  
  try({
    fit_lm <- lm(h_index ~ activity + nb_articles + nb_journals + gender + age 
                 + dist_erdos + betw + deg + close + eigen + page + n2 + bona + max_d1 + max_d2
                 + c_louvain #+ c_walk + c_label 
                 + c_spin,
                 data = train)
    frcst_lm <- predict(fit_lm, test)
    err_lm <- c(err_lm, as.numeric(abs(test$h_index - frcst_lm)))
    err_rel_lm <- c(err_rel_lm, as.numeric(abs(test$h_index - frcst_lm)/test$h_index))
  })
  
  try({
    fit_boost <- gbm(h_index ~ activity + nb_articles + nb_journals + gender + age 
                     + dist_erdos + betw + deg + close + eigen + page + n2 + bona + max_d1 + max_d2
                     + c_louvain #+ c_walk + c_label 
                     + c_spin,
                     data = train, distribution = "gaussian",
                     n.trees = 100, 
                     interaction.depth = 3, shrinkage = 0.01, keep.data = F, verbose = F, cv.folds = 0, bag.fraction = 0.9)#, train.fraction = 0.9)
    frcst_boost <- predict(fit_boost, test, 100)
    err_boost <- c(err_boost, as.numeric(abs(test$h_index - frcst_boost)))
    err_rel_boost <- c(err_rel_boost, as.numeric(abs(test$h_index - frcst_boost)/test$h_index))
  })
  
  try({
    fit_gam <- gam(as.numeric(h_index) ~ s(activity) + s(nb_articles) + s(nb_journals) + gender + s(age) 
                   + dist_erdos + s(betw) + s(deg) + s(close) + s(eigen) + s(page) + s(n2) + s(bona) + s(max_d1) + s(max_d2)
                   + c_louvain #+ c_walk + c_label 
                   + c_spin,
                   data = train, na.action = na.omit, select = F)
    frcst_gam <- predict(fit_gam, test)
    err_gam <- c(err_gam, as.numeric(abs(test$h_index - frcst_gam)))
    err_rel_gam <- c(err_rel_gam, as.numeric(abs(test$h_index - frcst_gam)/test$h_index))
  })
}

print(mean(err_lm[err_lm < as.numeric(quantile(err_lm, 0.95))]))
print(mean(err_boost[err_boost < as.numeric(quantile(err_boost, 0.95))]))
print(mean(err_gam[err_gam < as.numeric(quantile(err_gam, 0.95))]))

print(var(err_lm[err_lm < as.numeric(quantile(err_lm, 0.95))]))
print(var(err_boost[err_boost < as.numeric(quantile(err_boost, 0.95))]))
print(var(err_gam[err_gam < as.numeric(quantile(err_gam, 0.95))]))

print(mean(err_rel_lm[err_gam < as.numeric(quantile(err_gam, 0.95))]))
print(mean(err_rel_boost[err_rel_boost < as.numeric(quantile(err_rel_boost, 0.95))]))
print(mean(err_rel_gam[err_rel_gam < as.numeric(quantile(err_rel_gam, 0.95))]))

print(var(err_rel_lm[err_gam < as.numeric(quantile(err_gam, 0.95))]))
print(var(err_rel_boost[err_rel_boost < as.numeric(quantile(err_rel_boost, 0.95))]))
print(var(err_rel_gam[err_rel_gam < as.numeric(quantile(err_rel_gam, 0.95))]))


fit_lm <- lm(h_index ~ activity + nb_articles + nb_journals + gender + age 
             + dist_erdos + betw + deg + close + eigen + page + n2 + bona + max_d1 + max_d2
             + c_louvain #+ c_walk + c_label 
             + c_spin,
             data = Y)

fit_boost <- gbm(h_index ~ activity + nb_articles + nb_journals + gender + age 
                 + dist_erdos + betw + deg + close + eigen + page + n2 + bona + max_d1 + max_d2
                 + c_louvain #+ c_walk + c_label 
                 + c_spin,
                 data = Y, distribution = "gaussian",
                 n.trees = 100, 
                 interaction.depth = 3, shrinkage = 0.001, keep.data = F, verbose = F, cv.folds = 0, bag.fraction = 0.9)

fit_gam <- gam(as.numeric(h_index) ~ s(activity) + s(nb_articles) + s(nb_journals) + gender + s(age) 
               + dist_erdos + s(betw) + s(deg) + s(close) + s(eigen) + s(page) + s(n2) + s(bona) + s(max_d1) + s(max_d2)
               + c_louvain #+ c_walk + c_label 
               + c_spin,
               data = Y, na.action = na.omit, select = F)

# Without the obvious variables

fit_lm <- lm(h_index ~ gender + age 
             + dist_erdos + betw + deg + close + eigen + page + n2 + bona + max_d1 + max_d2
             + c_louvain #+ c_walk + c_label 
             + c_spin,
             data = Y)

fit_boost <- gbm(h_index ~ gender + age 
                 + dist_erdos + betw + deg + close + eigen + page + n2 + bona + max_d1 + max_d2
                 + c_louvain #+ c_walk + c_label 
                 + c_spin,
                 data = Y, distribution = "gaussian",
                 n.trees = 100, 
                 interaction.depth = 3, shrinkage = 0.01, keep.data = F, verbose = F, cv.folds = 0, bag.fraction = 0.9)

fit_gam <- gam(h_index ~ gender + s(age) 
               + dist_erdos + s(betw) + s(deg) + s(close) + s(eigen) + s(page) + s(n2) + s(bona) + s(max_d1) + s(max_d2)
               + c_louvain #+ c_walk + c_label 
               + c_spin,
               data = Y, na.action = na.omit, select = F)

fit_oax <- oaxaca(h_index ~ age + betw + deg + close + eigen
                  + page + n2 + bona + max_d1 + max_d2 | genderbin,
              data = Y, R = 30)
