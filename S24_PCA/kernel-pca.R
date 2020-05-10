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
base[,1:3] = scale(base[,1:3])

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
dadospca = base[-4]
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
install.packages('kernlab')
library(kernlab)
kpca = kpca(x = ~.,
            data = base_treinamento[-4],
            kernel = 'rbfdot',
            features = 2)
base_treinamento2 = as.data.frame(predict(kpca, base_treinamento))
#Voltar a classe:
base_treinamento2$default = base_treinamento$default

base_teste2 = as.data.frame(predict(kpca, base_teste))
base_teste2$default = base_teste$default

# Naive Bayes -------------------------------------------------------------
library(e1071)
classificador = naiveBayes(x = base_treinamento2[-3],
                           y = base_treinamento2$default)

previsoes = predict(classificador, newdata = base_teste2[-3])
matriz_confusao = table(base_teste2$default, previsoes)

# install.packages('caret')
library(caret)
confusionMatrix(matriz_confusao)