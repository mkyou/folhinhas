require(tidyverse)
theme_set(theme_minimal())

dados = read_csv(file = ".\\data\\dados_tratados.csv")


#verificando densidade
dados |> ggplot(aes(x = x, y = y)) +
  geom_point(aes(col = factor(sombra), size = densidade)) +
  labs(subtitle = "Densidade de folhas por região",
       x = 'X',
       y = 'Y', 
       col = 'Sombra',
       size = 'Densidade',
       caption = 'Fonte: Autores (2022)')


#verificando apenas contagem
dados |> ggplot(aes(x = x, y = y)) +
  geom_point(aes(col = factor(sombra), size = contagem)) +
  labs(subtitle = "Contagem de folhas por região",
       x = 'X',
       y = 'Y', 
       col = 'Sombra',
       size = 'Contagem',
       caption = 'Fonte: Autores (2022)')


#considerando apenas correlação
dados |> ggplot(aes(x = x, y = densidade)) +
  geom_point() +
  geom_smooth(method = "loess", se = F) + 
  labs(subtitle = "Correlação entre eixo X e densidade de folhas",
       x = 'X',
       y = 'Densidade de folhas', 
       caption = 'Fonte: Autores (2022)')

dados |> ggplot(aes(x = y, y = densidade)) +
  geom_point() +
  geom_smooth(method = "loess", se = F) + 
  labs(subtitle = "Correlação entre eixo y e densidade de folhas",
       x = 'Y',
       y = 'Densidade de folhas', 
       caption = 'Fonte: Autores (2022)')

#contagem
dados |> ggplot(aes(x = x, y = contagem)) +
  geom_point() +
  geom_smooth(method = "loess", se = F) + 
  labs(subtitle = "Correlação entre eixo X e contagem de folhas",
       x = 'X',
       y = 'Contagem de folhas', 
       caption = 'Fonte: Autores (2022)')

dados |> ggplot(aes(x = y, y = contagem)) +
  geom_point() +
  geom_smooth(method = "loess", se = F) + 
  labs(subtitle = "Correlação entre eixo y e contagem de folhas",
       x = 'Y',
       y = 'Contagem de folhas', 
       caption = 'Fonte: Autores (2022)')

#contagem e densidade por sombra
#filtro pois há uma densidade que é um claro outlier
#tirando os claros outliers da sombra, parece que 
#os valores do grupo em que tinha sol são maiores
dados |> filter(densidade < .2) |> 
  ggplot(aes(x = factor(sombra), y = densidade)) + 
  geom_boxplot() + 
  labs(subtitle = "Densidade de folhas por indicadora de sombra",
       x = "Sombra",
       y = "Densidade",
       caption = 'Fonte: Autores (2022)')

#ok, parece que o número de folhas era um pouco maior onde tinha sol
dados |> ggplot(aes(x = factor(sombra), y = contagem)) + 
  geom_boxplot() + 
  labs(subtitle = "Contagem de folhas por indicadora de sombra",
       x = "Sombra",
       y = "Contagem",
       caption = 'Fonte: Autores (2022)')

#será que a área influenciava na contagem?
dados |> ggplot(aes(x = area, y = contagem)) +
  geom_point() +
  labs(subtitle = "Correlação entre área e contagem de folhas",
       x = 'Área',
       y = 'Contagem de folhas', 
       caption = 'Fonte: Autores (2022)')

#não parece ter tanta infleuncia na densidade também, exceto quando é
#muito pequena, o que faz sentido.
dados |> ggplot(aes(x = area, y = densidade)) +
  geom_point() +
  labs(subtitle = "Correlação entre área e densidade de folhas",
       x = 'Área',
       y = 'Densidade de folhas', 
       caption = 'Fonte: Autores (2022)')

#distribuições das variáveis resposta

dadosTrat = dados |> filter(densidade < 0.2) 

hist(dadosTrat$densidade, col = "chocolate")
hist(dados$contagem, col = "chocolate")
#hist(dados$sombra, col = "chocolate")
