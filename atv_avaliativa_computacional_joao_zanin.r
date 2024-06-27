# 1ª Atividade Avaliativa de Estatística Computacional
# Professor: Juliano Bortolini
# Aluno: João Baptista Zanin

# Prova de EC
# Exercícios da lista 2: 3,5,11,15 e 16
# Exercícios do livro da Maria Rizzo: 3.1, 3.3, 3.4, 3.5, 3.7, 3.8 e 3.9

############################## Lista 2 ##############################
# Lista 2 - Exercício 3
library("Rlab")
set.seed(123)
rbern(10, p = 0.75)

p <- 0.5
u <- 0.3
u <- c(0.2, 0.3, 0.67)

rber0 <- function(u, p) { # p == probabilidade de sucesso = P(X = 1) = p
    if (u < p) x <- 1 else x <- 0
    return(x)
}

rber <- function(n, p) {
    U <- runif(n)
    x <- NULL
    for (u in U) x <- c(x, rber0(u, p))
    return(x)
}

# função para obter realizações da Bin. - CONVOLUÇÃO
rbinomial <- function(n, m, p) {
    x <- matrix(rber(n * m, p), nrow = n, ncol = m)
    y <- apply(x, MARGIN = 1, sum)
    return(y)
}
rbinomial(10, 5, 0.75)

# Função para obter realizações da geométrica:
rgeo <- function(n, p) {
    u <- runif(n)
    x <- floor(log(1 - u) / log(1 - p)) + 1
    return(x)
}

rgeo(10, 0.3)
hist(rgeo(10, 0.3))

# Portanto, a resposta é:
# 1. Defina a função rnbinom_neg com parâmetros n, r e p.
# 2. Dentro da função, inicialize um vetor vazio x.
# 3. Crie um loop que executa n vezes.
# 4. Dentro do loop, gere r variáveis geométricas usando a função rgeo e some-as.
# 5. Adicione o resultado à lista x.
# 6. Após o loop, retorne x.

rnbinom_neg <- function(n, r, p) {
    x <- NULL
    for (i in 1:n) {
        sum_geo <- sum(rgeo(r, p))
        x <- c(x, sum_geo)
    }
    return(x)
}
rnbinom_neg(10, 5, 0.3)

# Lista 2 - Exercício 5
# Esta função gera n realizações da distribuição Burr XII com parâmetros c e k.
# Ela primeiro verifica se c e k são maiores que 0, e se não forem, ela para a execução e retorna uma mensagem de erro.
# Em seguida, ela gera n números aleatórios uniformemente distribuídos entre 0 e 1,
# e aplica a função de distribuição acumulada inversa de Burr XII a esses números para obter as realizações da distribuição Burr XII.

rburrxii <- function(n, c, k) {
    if (c <= 0) {
        stop("c deve ser maior que zero")
    }
    if (k <= 0) {
        stop("k deve ser maior que zero")
    }
    u <- runif(n)
    x <- ((1 - u)^(-1 / k) - 1)^(1 / c)
    return(x)
}
rburrxii(10, 3, 2)

# Lista 2 - Exercício 11
# 1. Gerar um número aleatório U de uma distribuição uniforme U(0,1).
# 2. Calcular a função inversa da função de distribuição acumulada (CDF) da distribuição de probabilidade desejada e aplicá-la a U.

rdagum <- function(n, a, b, p) {
    if (a <= 0 || b <= 0 || p <= 0) {
        stop("a, b, e p devem ser maior que zero")
    }
    U <- runif(n)
    x <- b * ((1 - U^(1 / p))^(-1 / a) - 1)
    return(x)
}
rdagum(10, 3, 2, 0.5)

# Lista 2 - Exercício 15
# 1. Definir a função rel que aceita três argumentos: n, beta e p.
# 2. Adicionar verificações para garantir que beta é maior que 0 e p está no intervalo (0,1).
# Se essas condições não forem atendidas, a função deve parar e exibir uma mensagem de erro.
# 3. Gerar n números aleatórios uniformemente distribuídos entre 0 e 1.
# 4. Aplicar a transformação inversa da função de distribuição acumulada para obter as realizações da distribuição EL(β,p).
# Retornar as realizações geradas.
rel <- function(n, beta, p) {
    if (beta <= 0) {
        stop("beta deve ser maior que zero")
    }
    if (p <= 0 || p >= 1) {
        stop("p deve estar no intervalo (0,1)")
    }
    U <- runif(n)
    x <- -1/beta * log(((1-p)^U)/1-p)
    return(x)
}
rel(10, 3, 0.5)

# Lista 2 - Exercício 16
rel <- function(n, beta, p) {
    if (beta <= 0) {
        stop("beta deve ser maior que zero")
    }
    if (p <= 0 || p >= 1) {
        stop("p deve estar no intervalo (0,1)")
    }
    U <- runif(n)
    x <- -1/beta * log(((1-p)^U)/1-p)
    return(x)
}

del <- function(x, beta, p) {
    if (beta <= 0) {
        stop("beta deve ser maior que zero")
    }
    if (p <= 0 || p >= 1) {
        stop("p deve estar no intervalo (0,1)")
    }
    f_x <- -1/beta * log(((1-p)^U)/1-p)
    return(f_x)
}

x <- rel(1000, 2, 0.9)
xs <- seq(0, 5, by = 0.01)
f_xs <- del(xs, 2, 0.9)
hist(x, freq = FALSE)
lines(xs, f_xs, type = "l", col = "blue", lwd = 3)



############################## Maria Rizzo ##############################
# Exercício 3.1
gerar_amostra <- function(n, lambda, eta) {
  # Gere uma amostra da distribuição exponencial de um parâmetro
  amostra <- rexp(n, rate = lambda)
  
  # Adicione o parâmetro de localização para obter uma amostra da distribuição de dois parâmetros
  amostra <- amostra + eta
  
  return(amostra)
}

# Gere uma grande amostra
n <- 1000
lambda <- 2
eta <- 0.9
amostra <- gerar_amostra(n, lambda, eta)

# Calcule os quantis teóricos
quantis_teoricos <- qexp(ppoints(n), rate = lambda) + eta

# Compare os quantis da amostra com os quantis teóricos
library(car)
qqPlot(amostra, quantis_teoricos)

#################################################################################
# Exercício 3.3
# A função de distribuição acumulada (CDF) da distribuição de Pareto é dada por:
# $F(x)=1-\left(\frac{b}{x}\right)^a, \quad x \geq b>0, a>0$
# Para obter a transformação inversa da probabilidade $F^{-1}(U)$, precisamos resolver a equação $U = F(x)$ para $x$. Isso nos dá:
# $U = 1 - \left(\frac{b}{x}\right)^a$
# Resolvendo para $x$, obtemos:
# $x = \frac{b}{(1-U)^{1/a}}$
# Agora, podemos usar o método de transformação inversa para simular uma amostra aleatória da distribuição de Pareto(2,2). Aqui está o código R para fazer isso:

# Definindo os parâmetros
a <- 2
b <- 2

# Gerando uma amostra aleatória usando o método de transformação inversa
n <- 1000
U <- runif(n)
x <- b / (1 - U)^(1 / a)

# Gerando a densidade teórica da distribuição de Pareto
x_teorico <- seq(min(x), max(x), length.out = 100)
densidade_teorico <- a * b^a / x_teorico^(a + 1)

# Plotando o histograma da amostra e a densidade teórica
hist(x, freq = FALSE, breaks = "FD", main = "Distribuição de Pareto(2,2)", xlab = "x", ylab = "Densidade")
lines(x_teorico, densidade_teorico, col = "red", lwd = 2)

#################################################################################
# Exercício 3.4

# A densidade de Rayleigh é dada por:
# $f(x)=\frac{x}{\sigma^2} e^{-x^2 /\left(2 \sigma^2\right)}, \quad x \geq 0, \sigma>0$
# A função de distribuição acumulada (CDF) da distribuição de Rayleigh é:
# $F(x) = 1 - e^{-x^2 / (2 \sigma^2)}$
# Para obter a transformação inversa da probabilidade $F^{-1}(U)$, precisamos resolver a equação $U = F(x)$ para $x$. Isso nos dá:
# $U = 1 - e^{-x^2 / (2 \sigma^2)}$
# Resolvendo para $x$, obtemos:
# $x = \sigma \sqrt{-2 \ln(1 - U)}$

#Agora, podemos usar o método de transformação inversa para simular uma amostra aleatória da distribuição de Rayleigh. Aqui está o código R para fazer isso:

# Definindo os parâmetros
sigma <- c(1, 2, 3)

# Gerando uma amostra aleatória usando o método de transformação inversa
n <- 1000
U <- runif(n)

# Gerando amostras para diferentes valores de sigma e verificando o modo
for (s in sigma) {
  x <- s * sqrt(-2 * log(1 - U))
  
  # Gerando o histograma da amostra
  hist(x, freq = FALSE, breaks = "FD", main = paste("Distribuição de Rayleigh(", s, ")", sep = ""), xlab = "x", ylab = "Densidade")
  
  # Adicionando uma linha vertical no modo teórico
  abline(v = s, col = "red", lwd = 2)
}

# Este código gera uma amostra aleatória de tamanho 1000 da distribuição de
#Rayleigh para diferentes valores de $\sigma$ e plota o histograma da amostra com uma linha vertical no modo teórico $\sigma$ para comparação.

#################################################################################
# Exercício 3.5
#Primeiro, vamos criar um plano para o código:

#1. Defina a variável aleatória discreta X e sua função de massa de probabilidade (pmf).
#2. Use o método de transformação inversa para gerar uma amostra aleatória de tamanho 1000 da distribuição de X.
#3. Construa uma tabela de frequência relativa.
#4. Compare as probabilidades empíricas com as probabilidades teóricas.
#5. Repita o processo usando a função de amostra R.

#Aqui está o código R que segue este plano:
# Passo 1: Defina a variável aleatória discreta X e sua pmf
x <- c(0, 1, 2, 3, 4)
px <- c(0.1, 0.2, 0.2, 0.2, 0.3)

# Passo 2: Use o método de transformação inversa para gerar uma amostra aleatória
cumulative_px <- cumsum(px)
U <- runif(1000)
inverse_transform_sample <- findInterval(U, c(0, cumulative_px))

# Passo 3: Construa uma tabela de frequência relativa
relative_freq_table_inverse_transform <- table(inverse_transform_sample) / length(inverse_transform_sample)

# Passo 4: Compare as probabilidades empíricas com as probabilidades teóricas
print("Probabilidades empíricas (Transformação Inversa):")
print(relative_freq_table_inverse_transform)
print("Probabilidades teóricas:")
print(px)

# Passo 5: Repita o processo usando a função de amostra R
sample_func_sample <- sample(x, size = 1000, replace = TRUE, prob = px)
relative_freq_table_sample_func <- table(sample_func_sample) / length(sample_func_sample)

# Compare as probabilidades empíricas com as probabilidades teóricas
print("Probabilidades empíricas (Função de Amostra):")
print(relative_freq_table_sample_func)
print("Probabilidades teóricas:")
print(px)

#Este código primeiro define a variável aleatória discreta X e sua função de massa de probabilidade.
#Em seguida, usa o método de transformação inversa para gerar uma amostra aleatória de tamanho 1000 da distribuição de X.
#Uma tabela de frequência relativa é construída e as probabilidades empíricas são comparadas com as probabilidades teóricas. 
#O processo é então repetido usando a função de amostra R.

#################################################################################
# Exercício 3.7
# O plano passo a passo para a função:

#1. Defina uma função aceitacao_rejeicao_beta que recebe os parâmetros n, a e b.
#2. Dentro da função, crie um loop que continua até termos n amostras aceitas.
#3. Em cada iteração do loop, gere dois números aleatórios uniformes u e v.
#4. Calcule x como u^(1/a).
#5. Calcule y como v^(1/b).
#6. Se x + y <= 1, então aceite x como uma amostra da distribuição Beta(a, b).
#7. Uma vez que temos n amostras, retorne a amostra.
#8. Fora da função, chame a função com os parâmetros n = 1000, a = 3 e b = 2 para gerar uma amostra.
#9. Plote um histograma da amostra.
#10. Sobreponha a densidade teórica Beta(3, 2) no histograma.
 
# Aqui está o código R que segue este plano:
# Defina a função
aceitacao_rejeicao_beta <- function(n, a, b) {
  amostras <- numeric(n)
  i <- 1
  while (i <= n) {
    u <- runif(1)
    v <- runif(1)
    x <- u^(1/a)
    y <- v^(1/b)
    if (x + y <= 1) {
      amostras[i] <- x / (x + y)
      i <- i + 1
    }
  }
  amostras
}

# Gere uma amostra
amostra <- aceitacao_rejeicao_beta(1000, 3, 2)

# Plote o histograma
hist(amostra, probability = TRUE, breaks = 30, main = "Histograma com densidade Beta(3,2)", xlab = "x")

# Sobreponha a densidade teórica Beta(3, 2)
curve(dbeta(x, 3, 2), add = TRUE, col = "blue", lwd = 2)

#################################################################################
# Exercício 3.8
# O plano
#1. Defina uma função que gere variáveis aleatórias de uma distribuição Lognormal(µ,σ) usando um método de transformação. O método de transformação envolve a geração de uma variável aleatória de uma distribuição Normal(µ,σ) e, em seguida, a exponenciação dela.
#2. Fora da função, chame a função com os parâmetros µ = 0 e σ = 1 para gerar uma amostra de tamanho 1000.
#3. Plote um histograma da amostra.
#4. Sobreponha a densidade teórica Lognormal(µ,σ) no histograma usando a função dlnorm em R.

# Aqui está o código R que segue este plano:

# Defina a função
gerar_lognormal <- function(n, mu, sigma) {
  # Gere variáveis aleatórias de uma distribuição Normal(mu, sigma)
  variaveis_normais <- rnorm(n, mu, sigma)
  
  # Transforme as variáveis normais em variáveis lognormais
  variaveis_lognormais <- exp(variaveis_normais)
  
  variaveis_lognormais
}

# Gere uma amostra
amostra <- gerar_lognormal(1000, 0, 1)

# Plote o histograma
hist(amostra, probability = TRUE, breaks = 30, main = "Histograma com densidade Lognormal(0,1)", xlab = "x")

# Sobreponha a densidade teórica Lognormal(0, 1)
curve(dlnorm(x, 0, 1), add = TRUE, col = "blue", lwd = 2)

# Este código primeiro define uma função gerar_lognormal que gera variáveis aleatórias de uma distribuição Lognormal(µ,σ).
#Isso é feito gerando variáveis aleatórias de uma distribuição Normal(µ,σ) e depois exponenciando-as. 
#A função é então chamada para gerar uma amostra de tamanho 1000 com µ = 0 e σ = 1. 
#Um histograma da amostra é plotado, e a densidade teórica Lognormal(0,1) é sobreposta no histograma.

#################################################################################
# Exercício 3.9
# Aqui está o plano:
#1. Defina uma função que gere três variáveis aleatórias independentes U1, U2 e U3 de uma distribuição Uniforme(-1,1).
#2. Na mesma função, verifique se o valor absoluto de U3 é maior ou igual aos valores absolutos de U1 e U2. Se for, retorne U2. Caso contrário, retorne U3.
#3. Fora da função, chame a função para gerar uma grande amostra (por exemplo, tamanho 10000).
#4. Plote um histograma da amostra para estimar a densidade.

# Aqui está o código R que segue este plano:
# Defina a função
gerar_epanechnikov <- function(n) {
  # Inicialize um vetor vazio para armazenar os resultados
  resultado <- numeric(n)
  
  # Gere n variáveis
  for (i in 1:n) {
    # Gere U1, U2, U3 de Uniforme(-1,1)
    U1 <- runif(1, -1, 1)
    U2 <- runif(1, -1, 1)
    U3 <- runif(1, -1, 1)
    
    # Verifique a condição e armazene o resultado
    if (abs(U3) >= abs(U2) && abs(U3) >= abs(U1)) {
      resultado[i] <- U2
    } else {
      resultado[i] <- U3
    }
  }
  
  resultado
}

# Gere uma grande amostra
amostra <- gerar_epanechnikov(10000)

# Plote o histograma
hist(amostra, probability = TRUE, breaks = 30, main = "Histograma com densidade Epanechnikov", xlab = "x")

# Este código irá gerar um histograma que aproxima a densidade do kernel Epanechnikov rescaled.