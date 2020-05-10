setwd('C:\\Users\\Vitor Barbosa\\Google Drive\\DS\\Udemy-R\\S20_Agrupamento\\Cartao_Credito')

base = read.csv('original.csv')

#Função apply para somar as colunas linha por linha (MARGIN=1)
base$BILL_AMTT = apply(base[,13:18],1,sum)

#Agrupamento neste caso em uma base bidimensional apenas
X = data.frame(limite = base$LIMIT_BAL, 
               gasto = base$BILL_AMTT,
               genero = base$SEX,
               educacao = base$EDUCATION,
               civil = base$MARRIAGE, 
               idade = base$AGE)

X_scale = as.data.frame(scale(X))

#Elbow method para descrobrir quantos cluters serão necessários
#O Withins é a soma quadrática das distãncia de cada ponto em relação ao seu centróide
#Este valor é máximo para 1 centróide e mínimo para um número de centróides igual ao número de pontos, porque não haverá distância
#entre centróide e pontos. 

set.seed(1)
wcss = vector()
for (i in 1:10) {
  kmeans = kmeans(x = X_scale, centers = i)
  wcss[i] = sum(kmeans$withinss)
}
#Visualização do wcss
plot(wcss)

# KMeans ------------------------------------------------------------------
set.seed(1)
kmeans = kmeans(x = X_scale, 
                centers = 4)


previsoes = kmeans$cluster


pairs(X, col = c(1:4)[previsoes])


plot(X[,c(1,2)], 
     col = previsoes)


pcacorr = prcomp(X, scale = T)
coordenadas = as.data.frame(pcacorr$x)

plot(coordenadas[,c(1,2)], 
     col = previsoes)


#Clustplot não dá muito certo assim não 
library(cluster)
clusplot(coordenadas[,c(1,2)], previsoes, color = T)
