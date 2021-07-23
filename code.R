# https://archive.ics.uci.edu/ml/datasets/Bank+Marketing#
# [Moro et al., 2014] S. Moro, P. Cortez and P. Rita. A Data-Driven Approach to 
# Predict the Success of Bank Telemarketing. Decision Support Systems, Elsevier, 62:22-31, June 2014

# The data is related with direct marketing campaigns of a Portuguese banking institution. The marketing campaigns 
# were based on phone calls. Often, more than one contact to the same client was required, in order to access 
# if the product (bank term deposit) would be ('yes') or not ('no') subscribed.

library(skimr)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(caret)
library(corrplot)
library(pROC)
#


bank_full <- read.csv('bank-additional-full.csv', sep = ';')
head(bank_full)


summary(bank_full)


skim(bank_full)

# Removing duplicates
sum(duplicated(bank_full))
idx <- which(duplicated(bank_full)==TRUE)
bank_full <- bank_full[-idx,]
rm(idx)


# Target variable : y ('yes','no') -> subscribe a term deposit 
bank_full %>% ggplot(aes(y)) + geom_bar()


# drop: duration -> this attribute highly affects the output target (e.g., if duration=0 then y='no'). 
# Yet, the duration is not known before a call is performed. Also, after the end of the call y is obviously known. 
# Thus, this input should only be included for benchmark purposes and should be discarded if the intention is to 
# have a realistic predictive model.
bank_full <- bank_full %>% select(-duration)

#numeric
library("Hmisc")
num_bank_full <- bank_full %>% select(age, campaign, pdays, previous, emp.var.rate, cons.price.idx, cons.conf.idx, 
                                      euribor3m,nr.employed)
res <- rcorr(as.matrix(num_bank_full))
res
corrplot(res$r, type = "lower",tl.col = "black", tl.srt = 45)
heatmap(res$r,symm = TRUE)
detach("package:Hmisc", unload = TRUE)
rm(res,num_bank_full)


# age
bank_full %>% ggplot(aes(age)) + facet_grid("y", scales='free') + geom_histogram(color='black', bins=35) +
  theme(axis.text.x = element_text(angle=45))


# job
bank_full %>% ggplot(aes(job)) + facet_grid("y", scales='free') + geom_bar() + 
  theme(axis.text.x = element_text(angle=45))


# day of the week
bank_full$day_of_week <- factor(bank_full$day_of_week, levels=c("mon", "tue", "wed", "thu", "fri"))
bank_full %>% ggplot(aes(day_of_week)) + facet_grid("y", scales='free') + geom_bar() + 
  theme(axis.text.x = element_text(angle=45))


# marital status
bank_full %>% ggplot(aes(marital)) + facet_grid("y", scales='free') + geom_bar() + 
  theme(axis.text.x = element_text(angle=45))
bank_full %>% filter(marital=="unknown") %>% group_by(y) %>% summarize(n())
# bank_full <- bank_full %>% filter(marital!="unknown")


# education: Put basic categories together and illiterate inside unknown?
bank_full %>% ggplot(aes(education)) + facet_grid("y", scales='free') + geom_bar() + 
  theme(axis.text.x = element_text(angle=45))
bank_full %>% filter(education=="illiterate") %>% group_by(y) %>% summarize(n())


#default
bank_full %>% ggplot(aes(default)) + geom_bar()
bank_full[which(bank_full$default=="yes"),] # should we drop it? lots of unknown and little yeses
bank_full %>% ggplot(aes(default)) + facet_grid("y", scales='free') + geom_bar() + 
  theme(axis.text.x = element_text(angle=45))

# housing
bank_full %>% ggplot(aes(housing)) + geom_bar()


# loan
bank_full %>% ggplot(aes(loan)) + geom_bar()


# contact
bank_full %>% ggplot(aes(contact)) + geom_bar()


# month
unique(bank_full$month)
bank_full$month <- factor(bank_full$month, levels=c("mar", "apr", "may", "jun", "jul",
                                                    "aug", "sep", "oct", "nov", "dec"))
bank_full %>% ggplot(aes(month)) + geom_bar()
bank_full %>% ggplot(aes(month)) + facet_grid("y", scales='free') + geom_bar() + 
  theme(axis.text.x = element_text(angle=45))


# campaign
summary(bank_full$campaign)
unique(bank_full$campaign)
bank_full %>% ggplot(aes(campaign)) + facet_grid("y", scales='free') + geom_histogram(color='black', bins=20) +
  theme(axis.text.x = element_text(angle=45)) + scale_x_log10()
bank_full %>% ggplot(aes(campaign, fill=y)) + geom_boxplot() + scale_x_log10()


# pdays: number of days that passed by after the client was last contacted from a previous campaign 
# (numeric; 999 means client was not previously contacted)
bank_full %>% ggplot(aes(pdays)) + geom_histogram(bins=25)
contacted_before <- bank_full %>% filter(pdays<999) %>% select(pdays)
contacted_before %>% ggplot(aes(pdays)) + geom_histogram(bins=15, color='black')
rm(contacted_before)


# pdays and previous
summary(bank_full$pdays) # 999 values
bank_full %>% filter(pdays==999) %>% dim() %>% .[1]
bank_full %>% filter(pdays==999) %>% dim() %>% .[1]/dim(bank_full)[1] #percentage
bank_full %>% filter(previous==0) %>% dim() %>% .[1]
bank_full %>% filter(previous==0) %>% dim() %>% .[1]/dim(bank_full)[1]
# drop 'pdays'


# poutcome
bank_full %>% ggplot(aes(poutcome)) + geom_bar()
bank_full %>% ggplot(aes(poutcome)) + facet_grid("y", scales='free') + geom_bar() + 
  theme(axis.text.x = element_text(angle=45))


# emp.var.rate
summary(bank_full$emp.var.rate)
bank_full %>% ggplot(aes(emp.var.rate)) + facet_grid("y", scales='free') + geom_histogram(color='black', bins=10) +
  theme(axis.text.x = element_text(angle=45))


# cons.price.idx
summary(bank_full$cons.price.idx)
bank_full %>% ggplot(aes(cons.price.idx)) + facet_grid("y", scales='free') + geom_histogram(color='black', bins=10) +
  theme(axis.text.x = element_text(angle=45))


# cons.conf.idx
summary(bank_full$cons.conf.idx)
bank_full %>% ggplot(aes(cons.conf.idx)) + facet_grid("y", scales='free') + geom_histogram(color='black', bins=10) +
  theme(axis.text.x = element_text(angle=45))


# euribor3m
summary(bank_full$euribor3m)
bank_full %>% ggplot(aes(euribor3m)) + facet_grid("y", scales='free') + geom_histogram(color='black', bins=10) +
  theme(axis.text.x = element_text(angle=45))


# nr.employed
summary(bank_full$nr.employed)
bank_full %>% ggplot(aes(nr.employed)) + facet_grid("y", scales='free') + geom_histogram(color='black', bins=10) +
  theme(axis.text.x = element_text(angle=45))



# chi-squared test


chisq.test(bank_full$y, bank_full$job, correct=FALSE)

chisq.test(bank_full$y, bank_full$marital, correct=FALSE)

chisq.test(bank_full$y, bank_full$education, correct=FALSE) #check
table(bank_full$y, bank_full$education) # illiterate -> unknown
bank_full$education[bank_full$education == "illiterate"] <- "unknown"
chisq.test(bank_full$y, bank_full$education, correct=FALSE) #check

chisq.test(bank_full$y, bank_full$default, correct=FALSE) #check
table(bank_full$y, bank_full$default) # drop? only 3 yeses

chisq.test(bank_full$y, bank_full$housing, correct=FALSE) #####

chisq.test(bank_full$y, bank_full$loan, correct=FALSE) #####

chisq.test(bank_full$y, bank_full$contact, correct=FALSE)

chisq.test(bank_full$y, bank_full$month, correct=FALSE)

chisq.test(bank_full$y, bank_full$day_of_week, correct=FALSE)

chisq.test(bank_full$y, bank_full$poutcome, correct=FALSE)

#integer

chisq.test(bank_full$y, bank_full$previous, correct=FALSE) # too small
table(bank_full$y, bank_full$previous) # put 3-7 together?




#### BARPLOTS, HISTOGRAMS, QQPLOT, BOXPLOT
# Modeling

# fct_lump


bank_full_2 <- bank_full %>% mutate(job = as.factor(job), marital = as.factor(marital), 
                                    education = as.factor(education), contact = as.factor(contact), 
                                    day_of_week = as.factor(day_of_week), poutcome = as.factor(poutcome)) %>%
  select(age, job, marital, education, contact, month, day_of_week, campaign, previous, poutcome, 
         emp.var.rate, cons.price.idx, cons.conf.idx, euribor3m, y)
rm(bank_full)

# Partition: train and test
set.seed(123, sample.kind = "Rounding")
index <- createDataPartition(bank_full_2$y, p=0.7, list=FALSE)
train <- bank_full_2[-index,]
test <- bank_full_2[index,]
rm(index,bank_full_2)

train <- train %>% mutate(y = as.factor(y))


# Logistic Regression
set.seed(123, sample.kind = "Rounding")
model <- glm(y ~ ., data = train, family = binomial)
summary(model)$coef

probabilities <- model %>% predict(test, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "yes", "no")

cm <- confusionMatrix(as.factor(predicted.classes), as.factor(test$y), positive = 'yes')
cm

roc_glm <- roc(response=as.ordered(predicted.classes), predictor=as.ordered(test$y), auc=TRUE)
roc_glm$auc #0.79

rm(model,probabilities,predicted.classes,roc_glm)


#knn
set.seed(123, sample.kind = "Rounding")
pred_F1 <- function(k){
  fit <- knn3(y ~ ., data = train, k = k)
  y_hat <- predict(fit, test, type = "class")
  F_meas(data = y_hat, reference = as.factor(test$y), relevant='yes')
  
}

k <- seq(2,10)

F1 <- sapply(k, pred_F1)
plot(k, F1)
max(F1)
k[which.max(F1)]


fit <- knn3(y ~ ., data = train, k = k[which.max(F1)])
y_hat <- predict(fit, test, type = "class")
F_meas(data = y_hat, reference = as.factor(test$y), relevant='yes')

cm <- confusionMatrix(as.factor(y_hat), as.factor(test$y), positive = 'yes')
cm

roc_knn <- roc(response=as.ordered(y_hat), predictor=as.ordered(test$y), auc=TRUE)
roc_knn$auc #0.6351

rm(F1,y_hat,F_meas,roc_knn,k,pred_F1,fit)


#rpart
ctrl <- trainControl(method = "cv",number=5, classProbs = TRUE, summaryFunction = twoClassSummary)
set.seed(123, sample.kind = "Rounding")
fit_tree <- train(y ~ ., data = train, method = 'rpart', tuneGrid = data.frame(cp = seq(0, 0.05, 0.002)),
                  metric="ROC", trControl = ctrl)

fit_tree$results$cp[which.max(fit_tree$results$Accuracy)]

tree_pred <- predict(fit_tree, test)
cm <- confusionMatrix(as.factor(tree_pred), as.factor(test$y), positive = 'yes')
cm

roc_rpart <- roc(response=as.ordered(tree_pred), predictor=as.ordered(test$y), auc=TRUE)
roc_rpart$auc #0.7092

rm(fit_tree,tree_pred,roc_rpart)


# Random Forest
set.seed(123, sample.kind = "Rounding")
fit_rf <- train(y ~ ., data = train, method = 'rf', ntree = 100, 
                tuneGrid = data.frame(mtry = seq(1:7)), trControl= ctrl, metric='ROC') #6

fit_rf$results$mtry[which.max(fit_rf$results$ROC)]

rf_pred <- predict(fit_rf, test)
cm <- confusionMatrix(as.factor(rf_pred), as.factor(test$y), positive = 'yes')
cm

roc_rf <- roc(response=as.ordered(rf_pred), predictor=as.ordered(test$y), auc=TRUE)
roc_rf$auc #0.7414

rm(rf_pred,roc_rf,fit_rf)

#gbm
set.seed(123, sample.kind = "Rounding")
fit_gbm <- train(y ~ ., data = train, 
                 method = "gbm", 
                 trControl = ctrl,
                 metric = "ROC",
                 verbose = FALSE)

gbm_pred <- predict(fit_gbm, test)
cm <- confusionMatrix(as.factor(gbm_pred), as.factor(test$y), positive = 'yes')
cm

roc_gbm <- roc(response=as.ordered(gbm_pred), predictor=as.ordered(test$y), auc=TRUE)
roc_gbm$auc #0.7846

rm(fit_gbm,gbm_pred,roc_gbm)



#Rborist

library(Rborist)
control <- trainControl(method="cv", number = 5, p = 0.8)
grid <- expand.grid(minNode = c(1,5) , predFixed = c(10, 15, 25, 35, 50))
set.seed(123, sample.kind = "Rounding")
train_rf <-  train(y ~ ., data= train,
                   method = "Rborist",
                   nTree = 50,
                   trControl = control,
                   tuneGrid = grid,
                   nSamp = 5000)
ggplot(train_rf)
train_rf$bestTune

set.seed(123, sample.kind = "Rounding")
fit_rf <- Rborist(x=train %>% select(-y), train$y,
                  nTree = 1000,
                  minNode = train_rf$bestTune$minNode,
                  predFixed = train_rf$bestTune$predFixed)

y_hat_rf <- predict(fit_rf, test%>%select(-y))
cm <- confusionMatrix(as.factor(y_hat_rf$yPred), as.factor(test$y))
cm

roc_rborist <- roc(response=as.ordered(y_hat_rf$yPred), predictor=as.ordered(test$y), auc=TRUE)
roc_rborist$auc #0.818

rm(control,grid,y_hat_rf,roc_rborist,fit_rf,train_rf)



library(purrr) # for functional programming (map)

ctrl <- trainControl(method = "repeatedcv",
                     number = 10,
                     repeats = 5,
                     summaryFunction = twoClassSummary,
                     classProbs = TRUE)

orig_fit <- train(y ~ .,
                  data = train,
                  method = "gbm",
                  verbose = FALSE,
                  metric = "ROC",
                  trControl = ctrl)

model_weights <- ifelse(train$y == "no",
                        (1/table(train$y)[1]) * 0.5,
                        (1/table(train$y)[2]) * 0.5)

# Use the same seed to ensure same cross-validation splits

ctrl$seeds <- orig_fit$control$seeds

# Build weighted model

weighted_fit <- train(y ~ .,
                      data = train,
                      method = "gbm",
                      verbose = FALSE,
                      weights = model_weights,
                      metric = "ROC",
                      trControl = ctrl)

ctrl$sampling <- "down"

down_fit <- train(y ~ .,
                  data = train,
                  method = "gbm",
                  verbose = FALSE,
                  metric = "ROC",
                  trControl = ctrl)

ctrl$sampling <- "up"

up_fit <- train(y ~ .,
                  data = train,
                  method = "gbm",
                  verbose = FALSE,
                  metric = "ROC",
                  trControl = ctrl)

model_list <- list(original = orig_fit,
                   weighted = weighted_fit,
                   down = down_fit,
                   up = up_fit)

test_roc <- function(model, data) {
  
  roc(data$y,
      predict(model, data, type = "prob")[, "yes"])
  
}

model_list_roc <- model_list %>%
  map(test_roc, data = test)

model_list_roc %>%
  map(auc)


results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    data_frame(tpr = the_roc$sensitivities,
               fpr = 1 - the_roc$specificities,
               model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve for all 4 models

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)

rm(model_list,model_list_roc,results_df_roc,results_list_roc,orig_fit,up_fit,down_fit,
   the_roc,weighted_fit,model_weights,custom_col,num_mod)

library(ROSE)

data.rose <- ROSE(y ~ ., data = train, seed = 1)$data

data_balanced_over <- ovun.sample(y ~ ., data = train, method = "over")$data
table(data_balanced_over$y)

data_balanced_under <- ovun.sample(y ~ ., data = train, method = "under", seed = 1)$data
table(data_balanced_under$y)

data_balanced_both <- ovun.sample(y ~ ., data = train, method = "both", seed = 1)$data
table(data_balanced_both$y)


library(rpart)

tree.rose <- rpart(y ~ ., data = data.rose)
tree.over <- rpart(y ~ ., data = data_balanced_over)
tree.under <- rpart(y ~ ., data = data_balanced_under)
tree.both <- rpart(y ~ ., data = data_balanced_both)

pred.tree.rose <- predict(tree.rose, newdata = test)
pred.tree.over <- predict(tree.over, newdata = test)
pred.tree.under <- predict(tree.under, newdata = test)
pred.tree.both <- predict(tree.both, newdata = test)


roc.curve(test$y, pred.tree.rose[,2])

roc.curve(test$y, pred.tree.over[,2])

#AUC Undersampling
roc.curve(test$y, pred.tree.under[,2])

#AUC Both
roc.curve(test$y, pred.tree.both[,2])


ROSE.holdout <- ROSE.eval(y ~ ., data = train, learner = rpart, method.assess = "holdout",
                          extr.pred = function(obj)obj[,2], seed = 1)
ROSE.holdout


rm(pred.tree.both,pred.tree.over,pred.tree.rose,pred.tree.under,data_balanced_both,data.rose,
   data_balanced_over,data_balanced_under,tree.both,tree.over,tree.rose,tree.under, ROSE.holdout)


