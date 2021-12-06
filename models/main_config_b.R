source("1st_stage.R")
source("2nd_stage.R")

print("rodando main")

set.seed(13)     # reproducibility

Nrep <- 1750

n.pac     <- c()
n.espera  <- c()
tm.espera <- c()
t.fim     <- c()
n.natendidos <- c()
line.max <- c()

n.pac2     <- c()
n.espera2  <- c()
tm.espera2 <- c()
t.fim2     <- c()
n.natendidos2 <- c()
line.max2 <- c()

lines <- list()
lines2 <- list()

t.total.servico <- c()

t.fechou <- 11

for( i in 1: Nrep) {
  res <- simula.funcionamento.primeiro.estagio.2(t.fecha = t.fechou, tol = 0)
  log <- res$log
  res <- res$ans
  lines[[i]] <- log

  res2 <- simula.funcionamento.segundo.estagio.2(res)
  log2 <- res2$log
  res2 <- res2$ans
  lines2[[i]] <- log2

  # max das filas
  line.max <- c(line.max, log %>% summarise(max = max(line_1, line_2)) %>% .$max)
  line.max2 <- c(line.max2, log2 %>% summarise(max = max(line_1, line_2)) %>% .$max)

  # Respostas:
  n.natendidos <- c(n.natendidos, log$line_1[length(log$line_1)] + log$line_2[length(log$line_2)])
  n.pac        <- c(n.pac, nrow(res))                         # 1
  n.espera     <- c(n.espera, sum(res$t.espera > 0))          # 2
  tm.espera    <- c(tm.espera, mean(res$t.espera))            # 3
  t.fim        <- c(t.fim, 8 * 3600 + res$t.saida[nrow(res)]) # 4

   # Respostas 2:
  n.natendidos2 <- c(n.natendidos2, log2$line_1[length(log2$line_1)] + log2$line_2[length(log2$line_2)])
  n.pac2        <- c(n.pac2, nrow(res2))                         # 1
  n.espera2     <- c(n.espera2, sum(res2$t.espera > 0))          # 2
  tm.espera2    <- c(tm.espera2, mean(res2$t.espera))            # 3
  t.fim2        <- c(t.fim2, 8 * 3600 + res2$t.saida[nrow(res2)]) # 4

  t.total.servico <- c(t.total.servico, mean(res2$t.total))
}
