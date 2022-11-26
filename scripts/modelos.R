library(geoR)

#contagem------------------------------------------------------------------

geoDados = as.geodata(dados, data.col = "contagem")
bordadura = geoDados$coords[chull(geoDados$coords[,1:2]),1:2]
geoDados$borders = bordadura
plot(geoDados)

#ponderaRespostas----------------------------------------------------------

dados |> head()
#x, y
#0.01  1.2 

x1 = 0.01
y1 = 1.2


pondera_resp = function(x1, y1, df, resp){
  dist = sqrt((x1 - df$x)^2 + (y1 - df$y)^2)
  tempVar = which(dist == 0)
  respFin = sum(df[-tempVar, resp]/dist[-tempVar])
  return(respFin)
}


dados = dados |> rowwise() |> 
  mutate(newContagem = pondera_resp(x, y, dados, "contagem"),
         newDensidade = pondera_resp(x, y, dados, "densidade"),
         newSombra = pondera_resp(x, y, dados, "sombra"))

#modelo1-----------------------------------------------------------------
fit = lm(densidade ~ newDensidade, data = dados)
summary(fit)

par(mfrow = c(2,2))
plot(fit)

#verificar o ajuste sem o outlier
fit1 = lm(densidade ~ newDensidade, data = dados[-1,])
summary(fit1)

plot(fit1)

#modelo2-----------------------------------------------------------------
fit2 = glm(contagem ~ newContagem, data = dados,
           family = poisson())
summary(fit2)
glmTests::res_envelope(fit2)
glmTests::res_vs_fitted(fit2)
glmTests::res_vs_index(fit2)
glmTests::local_influence(fit2)
glmTests::influence(fit2, identify = 3)

#sem a observação 5
fit3 = glm(contagem ~ newContagem, data = dados[-5,],
           family = poisson())
summary(fit3)
par(mfrow = c(2,2))
glmTests::res_envelope(fit3)
glmTests::res_vs_fitted(fit3)
glmTests::res_vs_index(fit3)
glmTests::local_influence(fit3)
glmTests::influence(fit3, identify = 1)

#sem o 8 tbm
fit4 = glm(contagem ~ newDensidade, data = dados[c(-5, -8),],
           family = poisson())
summary(fit4)
par(mfrow = c(2,2))
glmTests::res_envelope(fit4)
glmTests::res_vs_fitted(fit4)
glmTests::res_vs_index(fit4)
glmTests::local_influence(fit4)
glmTests::influence(fit4, identify = 2)

#modelo3----------------------------------------------------------------
fit6 = glm(factor(sombra) ~ newSombra, data = dados,
           family = binomial())
summary(fit6)
glmTests::res_envelope(fit6)
glmTests::res_vs_fitted(fit6)
glmTests::res_vs_index(fit6)
glmTests::local_influence(fit6)
glmTests::influence(fit6, identify = 3)
glmTests::hl_test(fit6)
