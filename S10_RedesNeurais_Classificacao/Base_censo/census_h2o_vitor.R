setwd('/home/vitor/Desktop/DS/Udemy-R-MachineLearning/S10_RedesNeurais_Classificacao/Base_censo')

base = read.csv('census.csv')
base$X = NULL

base_b = base
# Label Encoder -----------------------------------------------------------
# install.packages('superml')
library(superml)
#Colunas que são strings, passamos para fatores para depois aplicar o labelencoder
lbllist = c(2,4,6,7,8,9,10,14,15)
lbl = LabelEncoder$new()
for (val in lbllist){
  base[,val] = as.factor(base[,val])
  base[,val] = lbl$fit_transform(base[,val])
}
# Escalonamento -----------------------------------------------------------
for (val in c(1:14)){
  base[,val] = as.numeric(scale(base[,val]))
}

# Sample Split ------------------------------------------------------------
library(caTools)
set.seed(1)
divisao = sample.split(base$income, SplitRatio = 0.85)
base_treinamento = subset(base, divisao == TRUE)
base_test = subset(base, divisao == FALSE)


# Puxar a biblioteca que roda a rede neural em um servidor misteri --------
library(h2o)
h2o.init(nthreads = -1)
classificador = h2o.deeplearning(y = 'income', 
                                 training_frame = as.h2o(base_treinamento),
                                 activation = 'rectifier',
                                 hidden = c(100),
                                 epochs = 500)

previsoes = h2o.predict(classificador, as.h2o(base_test[,-c(15)]))

previsoes = (previsoes > 0.5)
previsoes = as.vector(previsoes)
matriz_confusao = table(base_test[,15], previsoes)

library(caret)
confusionMatrix(matriz_confusao)
