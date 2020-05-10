setwd("C:\\Users\\vitor\\Google Drive\\DS\\Udemy-R-MachineLearning\\S4_NaiveBayes")

dados = read.csv('risco_credito.csv')

install.packages("e1071")

library('e1071')

#Naive Bayes aceita variáveis categóricas
classificador = naiveBayes(x = dados[-5],
                           y = dados$risco)

classificador

print(classificador)

#Classificar novo registro:

historia = c("boa")
divida = c("alta")
garantias = c("nenhuma")
renda = c("acima_35")
df = data.frame(historia, divida, garantias, renda)

previsao = predict(classificador, newdata = df, 'raw')
print(previsao)
