
getwd()

read.csv("plantas.csv",h=T)

dados <- read.csv("plantas.csv",h=T)

dados

attach(dados)

modelo <- lm(tamanho_cm~adubo_gr)

modelo

summary(dados)

anova(modelo)

summary(modelo)

plot(tamanho_cm ~ adubo_gr)

summary(modelo)

# FAzendo aparecer a reta
curve(-2.88133 + 0.72110 * x)


# refazendo o grafico e agora inserindo a reta JUNTO com o grafico
plot(tamanho_cm ~ adubo_gr)
curve(-2.88133 + 0.72110 * x, add=T)

# Mudando os nomes das variaveis
plot(tamanho_cm ~ adubo_gr, xlab="Quantidade de Adubo (gr)",
                            ylab="Crescimento da Planta (cm)",
                            pch=16)

curve(-2.88133 + 0.72110 * x, add=T,lty=2)

# =========================================================
#    ANOVA
# =========================================================

  dados2 <- read.csv("plantas2.csv",h=T)

  attach(dados2)

  dados2

  # Pergunta: 
  # H1 - A marca do Adubo altera o crescimento da planta ?
  # H0 - Não existe diferença entre as marcas
  
  modeloaov <- aov(tamanho_cm ~ adubo_marca)
  
  summary(modeloaov)

  summary(dados2)

  # Média dos tamanhos em relação a cada adubo:
  # comando para calcular variavel "x" CATEGORICA
  tapply(tamanho_cm, adubo_marca, mean)
  
  
  install.packages("gplots")
  
  library(gplots)
  
  medias <- tapply(tamanho_cm, adubo_marca, mean)
  
  barplot2(medias)  
  
  barplot2(medias, ylab="Crescimento da Planta (cm)",
                   xlab="Marca do Adubo")  
  
  
  barplot2(medias, ylab="Crescimento da Planta (cm)",
                   xlab="Marca do Adubo",
                   names.arg=c("Marca A", "Marca B"),
                   col=c("blue","red"))  
  

  erros <- tapply(tamanho_cm,adubo_marca,sd) /
           sqrt(tapply(tamanho_cm,adubo_marca,length))
  
  barplot2(medias, ylab="Crescimento da Planta (cm)",
           xlab="Marca do Adubo",
           names.arg=c("Marca A", "Marca B"),
           col=c("blue","red"),
           plot.ci=T, ci.u = medias + erros,
                      ci.l = medias - erros,
           ylim=c(0,10), main="Crescimento Médio das Plantas")
  
  