idade = c(20,  27,  21,  37,  46, 53, 55,  47,  52,  32,  39,  41,  39,  48,  48)
salario = c(1000,1200,2900,1850,900,950,2000,2100,3000,5900,4100,5100,7000,5000,6500)
base = data.frame(idade, salario)

base = scale(base)

plot(idade, salario)

set.seed(1)
kmeans = kmeans(x = base, centers = 3)

centroides = kmeans$centers

previsoes = kmeans$cluster

library(cluster)
clusplot(base, previsoes, xlab = 'Salario', ylab = 'Idade', main = 'Agrupamento salarios',
         lines = 0, shade = TRUE, color = TRUE, labels = 2)