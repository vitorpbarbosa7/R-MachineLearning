setwd('C:\\Users\\vitor\\Google Drive\\DS\\Udemy-R-MachineLearning\\S24_PCA')

base = read.csv('dados.csv')


# Tratamento de dados -----------------------------------------------------
#Deletar coluna de clientid
base$clientid = NULL

#Valores inconsistentes na coluna de idade:
base$age = ifelse(base$age <0, 40.92, base$age)

#Retirar valores faltantes:
base$age = ifelse(is.na(base$age), mean(base$age, na.rm = T), base$age)

#Escalonar as coluninhas que precisam:
base[,1:4] = scale(base[,1:4])

#Encode da classe:
base$default = factor(base$default, levels = c(0,1))

#Divisão entre treinamento e teste:
# install.packages('caTools')
library(caTools)
set.seed(1)
divisao = sample.split(base$income, SplitRatio = 0.75)
base_treinamento  =  subset(base, divisao ==TRUE)
base_teste = subset(base, divisao == FALSE)

# Análise de PCA ----------------------------------------------------------
dadospca = base
pcacorr = prcomp(dadospca,scale = T)
names(pcacorr)

#Extrair valores desta análise pca
library(factoextra)
variancias_pca = get_eigenvalue(pcacorr)

#Dados nas novas coordenadas de PC1 e PC2
fviz_pca(pcacorr)

#Screeplot:
fviz_eig(pcacorr)
plot(variancias_pca$cumulative.variance.percent,type = 'b')

#Loadings para seleção das variáveis mais importantes:
pca_load = pcacorr$rotation

pca_load_1_order = pca_load[order(abs(pca_load[,1])),1]
dotchart(pca_load_1_order,
         cex = 0.7, xlab = "loadings", main = "loadings PC1")


# Aplicação de PCA após a análise  ----------------------------------------
library(MASS)
lda = lda(formula = default ~ ., data = base_treinamento)
base_treinamento = as.data.frame(predict(lda,base_treinamento))
base_teste = as.data.frame(predict(lda,base_teste))

#Só puxar os atributos necessários, não é preciso puxar posterior0 and posterior1
base_treinamento = base_treinamento[c(4,1)]
base_teste = base_teste[c(4,1)]

# Naive Bayes -------------------------------------------------------------
library(e1071)
classificador = naiveBayes(x = base_treinamento[-2],
                           y = base_treinamento$class)

previsoes = predict(classificador, newdata = base_teste[-2])
matriz_confusao = table(base_teste$class, previsoes)

# install.packages('caret')
library(caret)
confusionMatrix(matriz_confusao)
