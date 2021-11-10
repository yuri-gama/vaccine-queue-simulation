source("1st_stage.R")
source("2nd_stage.R")

print("rodando main")

set.seed(13)     # reproducibility

Nrep <- 100

n.pac     <- c()
n.espera  <- c()
tm.espera <- c()
t.fim     <- c()
n.natendidos <- c()

n.pac2     <- c()
n.espera2  <- c()
tm.espera2 <- c()
t.fim2     <- c()
n.natendidos2 <- c()

lines <- list()
lines2 <- list()

for( i in 1: Nrep) {
  res <- simula.funcionamento()
  log <- res$log
  res <- res$ans
  lines[[i]] <- log

  res2 <- simula.funcionamento.segundo.estagio(res)
  log2 <- res2$log
  res2 <- res2$ans
  lines2[[i]] <- log2

  # Respostas:
  n.natendidos <- c(n.natendidos, log$line_1[length(log$line_1)] + log$line_2[length(log$line_2)] + log$line_3[length(log$line_3)] )
  n.pac        <- c(n.pac, nrow(res))                         # 1
  n.espera     <- c(n.espera, sum(res$t.espera > 0))          # 2
  tm.espera    <- c(tm.espera, mean(res$t.espera))            # 3
  t.fim        <- c(t.fim, 9 * 3600 + res$t.saida[nrow(res)]) # 4

   # Respostas 2:
  n.natendidos2 <- c(n.natendidos2, log2$line_1[length(log2$line_1)] + log2$line_2[length(log2$line_2)] + log2$line_3[length(log2$line_3)] )
  n.pac2        <- c(n.pac2, nrow(res2))                         # 1
  n.espera2     <- c(n.espera2, sum(res2$t.espera > 0))          # 2
  tm.espera2    <- c(tm.espera2, mean(res2$t.espera))            # 3
  t.fim2        <- c(t.fim2, 9 * 3600 + res2$t.saida[nrow(res2)]) # 4

}

# print(tm.espera)
# # Conversão de unidades temporais
# tm.espera <- tm.espera / 60 # em minutos
# t.fim <- t.fim / 3600       # em horas

pdf(file="../simulation_3_3.pdf")

# Histogramas do primeiro estagio
plot((lines[[1]]$t), (lines[[1]]$line_1), type = "s",
  xlab = 't(s)', ylab = 'n', main = 'fila 1', col = rgb(0, 0, 0, 0.1))

for (i in 2:Nrep){
  lines((lines[[i]]$t), (lines[[i]]$line_1), col = rgb(0, 0, 0, 0.1))
}

abline(v = 8*3600, lw = 2, col = 'red')
abline(v = 8*3600 + 30*60, lw = 2, col = 'green')

plot((lines[[1]]$t), (lines[[1]]$line_2), type = "s",
  xlab = 't(s)', ylab = 'n', main = 'fila 2', col = rgb(0, 0, 0, 0.1))

for (i in 2:Nrep){
  lines((lines[[i]]$t), (lines[[i]]$line_2), col = rgb(0, 0, 0, 0.1))
}

abline(v = 8*3600, lw = 2, col = 'red')
abline(v = 8*3600 + 30*60, lw = 2, col = 'green')

plot((lines[[1]]$t), (lines[[1]]$line_3), type = "s",
  xlab = 't(s)', ylab = 'n', main = 'fila 3', col = rgb(0, 0, 0, 0.1))

for (i in 2:Nrep){
  lines((lines[[i]]$t), (lines[[i]]$line_3), col = rgb(0, 0, 0, 0.1))
}

abline(v = 8*3600, lw = 2, col = 'red')
abline(v = 8*3600 + 30*60, lw = 2, col = 'green')

par(mfrow = c(2,2))
hist(n.pac, 
     col = "gray", 
     main = "No. de pacientes atendidos",
     xlab ="")

abline(v   = quantile(n.pac, c(0.025, 0.5, 0.975)), 
       col = c(4, 2, 4), 
       lty = c(2, 1, 2), 
       lwd = 2)
       
hist(n.espera, 
     col = "gray", 
     main = "No. de pacientes que esperaram",
     xlab ="")
abline(v   = quantile(n.espera, c(0.025, 0.5, 0.975)), 
       col = c(4, 2, 4), 
       lty = c(2, 1, 2), 
       lwd = 2)

hist(tm.espera/60, 
     col = "gray",
     main = "Tempo médio de Espera (min)",
     xlab ="")
abline(v   = quantile(tm.espera/60, c(0.025, 0.5, 0.975)), 
       col = c(4, 2, 4), 
       lty = c(2, 1, 2), 
       lwd = 2)


       hist(t.fim/3600, 
     col = "gray",
     main = "Tempo de Fechamento da Clinica (h)",
     xlab ="")
abline(v   = quantile(t.fim/3600, c(0.025, 0.5, 0.975)), 
       col = c(4, 2, 4), 
       lty = c(2, 1, 2), 
       lwd = 2)

par(mfrow = c(1,1))

       hist(n.natendidos, 
     col = "gray",
     main = "N atendidos",
     xlab ="")
abline(v   = quantile(n.natendidos, c(0.025, 0.5, 0.975)), 
       col = c(4, 2, 4), 
       lty = c(2, 1, 2), 
       lwd = 2)


# Histogramas 2
plot((lines2[[1]]$t), (lines2[[1]]$line_1), type = "s",
  xlab = 't(s)', ylab = 'n', main = 'fila 1', col = rgb(0, 0, 0, 0.1))

for (i in 2:Nrep){
  lines((lines2[[i]]$t), (lines2[[i]]$line_1), col = rgb(0, 0, 0, 0.1))
}

abline(v = 8*3600, lw = 2, col = 'red')
abline(v = 8*3600 + 30*60, lw = 2, col = 'green')

plot((lines2[[1]]$t), (lines2[[1]]$line_2), type = "s",
  xlab = 't(s)', ylab = 'n', main = 'fila 2', col = rgb(0, 0, 0, 0.1))

for (i in 2:Nrep){
  lines((lines2[[i]]$t), (lines2[[i]]$line_2), col = rgb(0, 0, 0, 0.1))
}

abline(v = 8*3600, lw = 2, col = 'red')
abline(v = 8*3600 + 30*60, lw = 2, col = 'green')

plot((lines2[[1]]$t), (lines2[[1]]$line_3), type = "s",
  xlab = 't(s)', ylab = 'n', main = 'fila 3', col = rgb(0, 0, 0, 0.1))

for (i in 2:Nrep){
  lines((lines2[[i]]$t), (lines2[[i]]$line_3), col = rgb(0, 0, 0, 0.1))
}

abline(v = 8*3600, lw = 2, col = 'red')
abline(v = 8*3600 + 30*60, lw = 2, col = 'green')

par(mfrow = c(2,2))
hist(n.pac2, 
     col = "gray", 
     main = "No. de pacientes atendidos",
     xlab ="")

abline(v   = quantile(n.pac2, c(0.025, 0.5, 0.975)), 
       col = c(4, 2, 4), 
       lty = c(2, 1, 2), 
       lwd = 2)
       
hist(n.espera2, 
     col = "gray", 
     main = "No. de pacientes que esperaram",
     xlab ="")
abline(v   = quantile(n.espera2, c(0.025, 0.5, 0.975)), 
       col = c(4, 2, 4), 
       lty = c(2, 1, 2), 
       lwd = 2)

hist(tm.espera2/60, 
     col = "gray",
     main = "Tempo médio de Espera (min)",
     xlab ="")
abline(v   = quantile(tm.espera2/60, c(0.025, 0.5, 0.975)), 
       col = c(4, 2, 4), 
       lty = c(2, 1, 2), 
       lwd = 2)


       hist(t.fim2/3600, 
     col = "gray",
     main = "Tempo de Fechamento da Clinica (h)",
     xlab ="")
abline(v   = quantile(t.fim2/3600, c(0.025, 0.5, 0.975)), 
       col = c(4, 2, 4), 
       lty = c(2, 1, 2), 
       lwd = 2)

par(mfrow = c(1,1))

       hist(n.natendidos2, 
     col = "gray",
     main = "N atendidos",
     xlab ="")
abline(v   = quantile(n.natendidos2, c(0.025, 0.5, 0.975)), 
       col = c(4, 2, 4), 
       lty = c(2, 1, 2), 
       lwd = 2)
