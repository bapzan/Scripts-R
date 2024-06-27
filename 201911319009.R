# Docente: Luiz Andre R. Zardo
# Discente: João Baptista Zanin
# Avaliação III

# Descubra qual seu 'working directory'
getwd()
# Criando uma nova pasta para trabalhar
dir.create("C:/Users/joao2/OneDrive/Documents/FACULDADE/PACOTES ESTATÍSTICOS/Avaliação 3/Avaliacao_III")
# Mudando o lugar onde eu trabalho
setwd("C:/Users/joao2/OneDrive/Documents/FACULDADE/PACOTES ESTATÍSTICOS/Avaliação 3/Avaliacao_III")

#------------------------------------------------------------------------------#
# 1. Faça leitura do conteúdo referente a Produção de Gráficos e Análise Exploratória 
# de Dados utilizando o pacote ggplot2 contidos nas páginas web.
# https://r4ds.had.co.nz/
# https://rpubs.com/mnunes/ggplot2
# https://ggplot2.tidyverse.org/
# http://r-statistics.co/Complete-Ggplot2-Tutorial-Part1-With-R-Code.html
#------------------------------------------------------------------------------#
install.packages("dplyr")
library("dplyr")

install.packages("tidyverse")
library("tidyverse")

install.packages("ggplot2")
library("ggplot2")

#------------------------------------------------------------------------------#
# 2. Considere o dataset mpg disposto no pacote ggplot2:
# 2.1. Execute o código ggplot(data = mpg). O que é retornado? [0,75 ponto].
#------------------------------------------------------------------------------#
# Verificando o DF e plotando o gráfico
dfmpg = mpg
ggplot(data = mpg)

# Resosta: Me aparece um dataframe com o consumo de combustivel de carros populares
# entre 1999 e 2008.



#------------------------------------------------------------------------------#
# 2.2. O que a variável drv descreve? (vide help: ?mpg para suporte) [0,75 ponto].
#------------------------------------------------------------------------------#
# Verificando o DF e printando
df.drv = select(dfmpg, drv)
print(df.drv)

# Resposta: A coluna drv nos mostra o tipo de tração do veículo, sendo,
# f = tração nas rodas da frente, r = tração nas rodas de trás e 4 = tração nas 4 rodas



#------------------------------------------------------------------------------#
# 2.3. Obtenha um gráfico de dispersão para as variáveis hwy versus cyl. [0,75 ponto].
#------------------------------------------------------------------------------#
# Plotando o gráfico de dispersão
dispersao.hwy.cyl = ggplot(data = dfmpg, aes(x = hwy, y = cyl)) + geom_point() 
dispersao.hwy.cyl



#------------------------------------------------------------------------------#
# 2.4. O que acontece ao se criar facet (facetas) em uma variável contínua?
# Dê um exemplo com base em mpg. [0,75 ponto].
#------------------------------------------------------------------------------#

# Utilizando o exemplo do pacote
p = ggplot(mpg, aes(displ, cty)) + geom_point()
p
p + facet_grid(rows = vars(drv))
# Interpretação: Temos painéis comparando nossas variáveis contínuas



#------------------------------------------------------------------------------#
# 3. Obtenha um conjunto de dados (com base em dados reais). Indique a fonte
# e a base de dados utilizada.
# 3.1. Crie um objeto selecionando 5 variáveis contínuas. [1,00 ponto].
#------------------------------------------------------------------------------#
# Fonte: https://finance.yahoo.com/quote/VALE3.SA/history?p=VALE3.SA
# Base de dados baixada com os códigos a seguir:

install.packages("quantmod")
library("quantmod")

install.packages("BatchGetSymbols")
library("BatchGetSymbols")

# Pegando o dataframe com dados da ação VALE3 de 01 do 01 de 2021 até a data do seu SO
df1 = BatchGetSymbols(tickers = "VALE3.SA", first.date = "2021-01-01", last.date = Sys.Date())
# Armazenando os dados de 'tickers' do df1
df2 = df1$df.tickers
View(df2)

# Criando o gráfico do df2, com a data e o preço de fechamento e os nomes
grafico = ggplot(data = df2, aes(ref.date, price.close)) + geom_line() +
                 labs(x = "Data", y = "Preço de Fechamento", title="VALE3", subtitle = "VALE S.A.")
grafico
# Agora que provamos que a variável contínua existe, iremos armazenar em um objeto
# o preço de fechamento, preço de abertura, preço mínimo, preço máximo e o volume.
cinco.variaveis = select(df2, price.close, price.open, price.low, price.high, volume)
# Interpretação: O objeto 'cinco.variaveis' agora está com 5 variáveis do dataframe2 (df2)

#------------------------------------------------------------------------------#
# 3.2. Descreva as variáveis selecionadas [1,00 ponto].
#------------------------------------------------------------------------------#
# Resposta: No objeto 'cinco.variaveis' armazenamos as seguintes variáveis de ações
# da bolsa de valores brasileira, sendo a ação escolhida 'VALE3', negociada na 
# B&MF BOVESPA, os dados contínuos armazenados são os seguintes:
# price.close = Preço de fechamento do papel no dia
# price.open  = Preço de abertura do papel no dia
# price.low   = O preço mínimo que o papel atingiu no dia
# price.high  = O preço máximo que o papel atingiu no dia
# volume      = O volume financeiro negociado no dia
# Sendo as 5 variáveis acimas todas do Yahoo Finance



#------------------------------------------------------------------------------#
# 4. Com base no objeto criado no item 1 (a), escolha 2 variáveis:
# 4.1. Obtenha uma representação gráfica por meio de um gráfico de dispersão [1,00 ponto].
#------------------------------------------------------------------------------#
# Bom, não entendi muito bem qual é o item "1 (a)", vou supor que seja o objeto
# que eu criei?

# Escolhendo 2 variáveis do objeto 3.1, vamos criar um gráfico de dispersão simples
# Escolhemos a variável data
dispersao.MinimaMaxima = ggplot(data = cinco.variaveis, aes(x = price.low,
                                                               y = price.high)) + 
  geom_point() + labs(x = "Mínima do dia",
                      y = "Máxima do dia",
                      title = "VALE3",
                      subtitle = "Mínima x Máxima")
  
dispersao.MinimaMaxima
# Fizemos o preço minimo e maximo do dia na acao VALE DO RIO DOCE da bolsa de valores
# Notamos que existe uma correlação positiva entre a cotação mínima e a máxima dos dias


#------------------------------------------------------------------------------#
# 4.2. Obtenha a matriz de correlação das variáveis selecionadas, 
# interprete o resultado [1,00 ponto]
#------------------------------------------------------------------------------#
# Montando um objeto com 2 variáveis das 5 e fazendo a correlação
duas.variaveis = select(cinco.variaveis, price.low, price.high)
correlacao = cor(duas.variaveis)
correlacao

# INTERPRETAÇÃO
# Na nossa correlação entre 'price.low' e 'price.high', temos um r de
# price.low x price.high de 0.9974308, o que indica uma correlação positiva 
# quase perfeita



#------------------------------------------------------------------------------#
# 4.3. Assumindo que um modelo de regressão linear simples é adequado, determine
# os coeficientes da equação pelo método dos mínimos quadrados, e verifique a 
# significância estatística dos coeficientes estimados, interprete o resultado [1,50 pontos].
#------------------------------------------------------------------------------#
# Fazendo a regressão linear simples de mínimos quadrados e vendo o sumário
regressao.linear = lm(formula = price.low ~ price.high, data = cinco.variaveis)
summary(regressao.linear)

# Verificamos que a um nível de significancia de 5%, podemos afirmar que nosso
# modelo de regressão para a variável resposta 'price.high' é que 
# para cada unidade de 'price.high' há um aumento de 0,9855 em uma 
# unidade de 'price.low', com intercept de -1.1350. 



#------------------------------------------------------------------------------#
# 4.4. . Represente a reta de regressão no gráfico realizado no item 4.1 [1,00 ponto].
#------------------------------------------------------------------------------#
# Montando a reta do exemplo 4.1
reta.de.regressao = dispersao.MinimaMaxima + geom_smooth(method = "lm", se = F)
reta.de.regressao

# Acima temos a reta de regressão do exemplo 4.1, observamos que ele é ascendente



#------------------------------------------------------------------------------#
# 5. Salve o código do R (script) utilizado para realização da avaliação
# nomeando-o com o número de seu RGA, enviando para correção no local 
# apropriado no Ambiente Virtual de Aprendizagem (AVA) da disciplina [0,50]
#------------------------------------------------------------------------------#

# Salvando o script com o número do meu RGA


# IMPORTANTE:
# 2) O arquivo obtido para no item 3 deverá ser enviado junto com o script,
# o não envio do arquivo resultará em nota zero na avaliação.

# Vamos salvar o dataframe criando no item 3 em planilha para encaminha-lo
# junto ao trabalho final.

write.table(cinco.variaveis, file = "C:/Users/joao2/OneDrive/Documents/FACULDADE/PACOTES ESTATÍSTICOS/Avaliação 3/Avaliacao_III/ArquivoItem3.")



