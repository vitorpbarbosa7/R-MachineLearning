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
library('caret')
pca = preProcess(x = base_treinamento[-4], 
                 method = 'pca',
                 pcaComp = 2)
#Transformação:
base_treinamento = predict(pca, base_treinamento)
base_teste = predict(pca, base_teste)




# Naive Bayes -------------------------------------------------------------
library(e1071)
classificador = naiveBayes(x = base_treinamento[-1],
                           y = base_treinamento$default)

previsoes = predict(classificador, newdata = base_teste[-1])
matriz_confusao = table(base_teste$default, previsoes)

# install.packages('caret')
confusionMatrix(matriz_confusao)

prev_df = data.frame(previsoes)

prev_df =data.frame(base_teste$default, prev_df$previsoes)

names(prev_df)[1] = 'teste'
names(prev_df)[2] = 'previsoes'
