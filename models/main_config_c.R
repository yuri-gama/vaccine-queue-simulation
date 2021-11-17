source("1st_stage.R")
source("2nd_stage.R")

print("rodando main")

set.seed(13)     # reproducibility

Nrep <- 350

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

for( i in 1: Nrep) {
  res <- simula.funcionamento.config.c(tol = 30)
  log <- res$log
  res <- res$ans
  # log <- log %>% mutate(t = hms::as_hms(t + 28800))
  # log <- log %>% mutate(t = t + 28800)
  lines[[i]] <- log


  # max das filas
  line.max <- c(line.max, log %>% summarise(max = max(line_1, line_2)) %>% .$max)

  # Respostas:
  n.natendidos <- c(n.natendidos, log$line_1[length(log$line_1)] + log$line_2[length(log$line_2)])
  n.pac        <- c(n.pac, nrow(res))                         # 1
  n.espera     <- c(n.espera, sum(res$t.espera > 0))          # 2
  tm.espera    <- c(tm.espera, mean(res$t.espera))            # 3
  t.fim        <- c(t.fim, 9 * 3600 + res$t.saida[nrow(res)]) # 4


  t.total.servico <- c(t.total.servico, mean(res$t.saida - res$t.chegada))
}

pdf(file="../simulation_config_c.pdf")

vector_times <- (c(0:((16-8)*2))*0.5 + 8)*3600
text_vector_times <- hms::as_hms(vector_times)
# Histogramas do primeiro estagio
plot((lines[[1]]$t), (lines[[1]]$line_1), type = "s",
  xlab = 't(s)', ylab = 'n', main = 'fila 1', col = rgb(0, 0, 0, 0.1), xaxt = "n")

axis(1,                                                   # Add dates to x-axis
     at = (vector_times - 28800), labels = text_vector_times, cex.axis = 0.75)

for (i in 2:Nrep){
  lines((lines[[i]]$t), (lines[[i]]$line_1), col = rgb(0, 0, 0, 0.1))
}

abline(v = 8*3600, lw = 2, col = 'red')
abline(v = 8*3600 + 30*60, lw = 2, col = 'green')

plot((lines[[1]]$t), (lines[[1]]$line_2), type = "s",
  xlab = 't(s)', ylab = 'n', main = 'fila 2', col = rgb(0, 0, 0, 0.1), xaxt = 'n')
axis(1,                                                   # Add dates to x-axis
     at = (vector_times - 28800), labels = text_vector_times, cex.axis = 0.75)


for (i in 2:Nrep){
  lines((lines[[i]]$t), (lines[[i]]$line_2), col = rgb(0, 0, 0, 0.1))
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

hist(t.total.servico/60, 
     col = "gray",
     main = "Tempo médio de Serviço (min)",
     xlab ="")
abline(v   = quantile(t.total.servico/60, c(0.025, 0.5, 0.975)), 
       col = c(4, 2, 4), 
       lty = c(2, 1, 2), 
       lwd = 2)