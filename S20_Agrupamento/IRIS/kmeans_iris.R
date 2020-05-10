base = iris

base2 = base[1:2]

set.seed(1)
kmeans = kmeans(x = base2, centers = 3)

previsoes = kmeans$cluster

library(cluster)
clusplot(base, previsoes,
         color = T)


# Verificar se fez certo --------------------------------------------------
tabela = table(base$Species, previsoes)


# Gráficos ----------------------------------------------------------------
library(tidyverse)
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) + 
  geom_point(aes(color = Species), size = 3) + 
  theme_bw
