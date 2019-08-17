
### 20190817
### Nat
### analise de classificacao para a base heart.csv usando Naive Bayes

########################################################
########################################################
########################################################

# bibliotecas
library(e1071)
library(naiveBayes)
library(ggplot2)
library(caret)

### preproc



# carregar a base de dado
dado <- readRDS("data_heart_train_model.rds")
                 


# ver a estrutura do conjunto de dado
str(dado)

# resumo dos dados
summary(dado)

# colunas para virar fator
tofac <- c(
  "sex"
  ,"cp"
  # ,"trestbps"
  # ,"chol"
  ,"fbs"
  ,"restecg"
  # ,"thalach"
  ,"exang"
  # ,"oldpeak"
  ,"slope"
  ,"ca"
  ,"thal"
  ,"target"
)

# transformar para fator por referencia
for( i in tofac){
  dado[,i] <- as.factor(dado[,i])
}


# verificacao de dado faltante
table(is.na(dado))
 253*14


 #############################################
 
 ggplot(dado, aes(x = trestbps, y = chol)) + 
   geom_point(aes(colour = target))
 
 ################################################
 
 table(dado$target)
 prop.table(table(dado$target))
 
 ###############################################
 
 indice <- 1:253
 # ?sample
 set.seed(2019)
 totrain <- sample(indice, round(0.7*nrow(dado),0))
 totest <- indice[!(indice %in% totrain)]
 
 train <- dado[totrain, ]
 test <- dado[totest, ]
 
 ###############################################
 fit_naiveBayes <- naiveBayes(target ~ ., data = train)

 
 #predict
 
 adj_naiveBayes <- predict(fit_naiveBayes
                        , newdata = test
                        , type = "raw"
                          )
 
 
 
 test$class_naiveBayes_50 <- factor(ifelse(adj_naiveBayes[,2]<0.5,0,1))
 

 
 ########################################
 
 confusionMatrix(test$class_naiveBayes_50,test$target, positive =1)
 ?confusionMatrix
 
 ########################################
 
