rm(list = ls())
library(dplyr)
set.seed(13)  # para tornar a simulação reprodutível

# Função que simula um dia de funcionamento da clínica

print("running")

resample <- function(x, ...) x[sample.int(length(x), ...)]

atendimento_medico <- function(t.clinica, medico, t.inicio, t.chegada, t.saida, t.espera, dur.consulta, i, target, medico_num, t_inicio_consulta, t.entra, tol, atendido){
    medico[i]     <- medico_num
    t.inicio[i]  <- t_inicio_consulta                   # inicio da consulta
    t.saida[i]   <- t.inicio [i] + dur.consulta [i] # fim da consulta
    t.espera[i]  <- t.inicio [i] - t.chegada [i]    # tempo de espera ate consulta
    t.clinica[i] <- t.saida [i]  - t.chegada [i]    # tempo que passou na clinica
    target <- t.saida [i]

    if(t.inicio[i] <= t.entra + tol)
    {
      atendido[i] <- TRUE
    }

    log <- data.frame(
          i = i,
          medico = medico[i],
          t_inicio = t.inicio[i],
          t_saida = t.saida[i],
          t_espera = t.espera[i],
          t_chegada = t.chegada[i],
          atendido = atendido[i]
    )

    ans <- list(clinica = t.clinica, medico = medico, inicio = t.inicio, 
    chegada = t.chegada, saida = t.saida, espera = t.espera, consulta = dur.consulta, target = target, log = log, atendido = atendido)


    return (ans)                    # fim da consulta de M2
}


simula.funcionamento <- function(t.abre = 8, 
                                 t.fecha = 16, 
                                 lambda.chegada = 1/2,
                                 dur.min =15,
                                 dur.max = 20){
  log <- data.frame()
  ans <- list()
  tol <- 30*60
  
  ## DEFINICOES DE VARIAVEIS:
  # clinica
  t.entra  <- ( t.fecha - t.abre ) * 3600  # duração do expediente na clínica (em segundos)
                                           # tempo máximo de admissao de pacientes
  
  # pacientes
  lambda.chegada <- lambda.chegada / 60  # taxa de chegadas por segundo 
  x <- t.entra * lambda.chegada * 5      # numero de pacientes que chegam ate a clinica
                                         # (valor superestimado => fator 5)
  
  # gera tempos de chegada dos pacientes
  t.chegada <- cumsum(rexp(x, lambda.chegada))  
  
  # consultas
  dur.min <- dur.min * 60  # duracao minima da consulta (em segundos)
  dur.max <- dur.max * 60  # duracao maxima da consulta (em segundos)
  
  # gera duracao de cada consulta dos pacientes que entraram na clinica
  dur.consulta <- runif(x, min = dur.min, max = dur.max)
  
  # seleciona pacientes que serao admitidos na clinica
  # serão atendidos apenas aqueles que chegarem antes de `t.entra`:
  pac.entra    <- which( t.chegada < t.entra ) # pacientes admitidos na clinica
  t.chegada    <- t.chegada[pac.entra]         # tempo de chegada dos pacientes admitidos
  dur.consulta <- dur.consulta [pac.entra]     # duracao da consulta dos pacientes admitidos
  n.entra      <- length(pac.entra)            # no. de pacientes admitidos na clinica
  
 
  ## INICIALIZACAO:
  ## ANTES DE INICIAR AS CONSULTAS:
  t.inicio  <- c() # tempo de inicio da consulta
  t.espera  <- c() # tempo de espera dos pacientes
  t.saida   <- c() # tempo de saida dos pacientes
  t.clinica <- c() # tempo que os pacientes passaram na clinica
  atendido <- rep(FALSE, length(t.chegada))
  medico    <- c() # medico que atendeu os pacientes
  tm1 <- tm2 <- tm3 <- 0  # instante em que M1 (médico 1) e M2 (médico 2) ficam livres
  
  ## PRIMEIRA CONSULTA: 
  # |-> medicos iniciam dia desocupados
  # |-> medico 1 atende
  # |-> paciente nao espera
  # |-> tamanho da fila = 0
  # t.inicio = tempo de chegada do 1o. paciente 
  # t.saida  = t.inicio + duracao da consulta

  # medico[1]     <- 1
  # t.espera [1]  <- 0
  # t.inicio [1]  <- t.chegada [1]
  # t.saida [1]   <- t.inicio [1] + dur.consulta [1]
  # t.clinica [1] <- t.saida [1]  - t.chegada [1]
  # tm1 <- t.saida [1]

  ## DEMAIS CONSULTAS (> 2o. paciente): 
  # verifica se medico 1 esta livre
  # |-> sim: medico 1 atende => paciente nao espera
  # |-> não: verifica se medico 2 esta livre
  #     |-> sim: medico 2 atende => paciente nao espera
  #     |-> não: paciente espera
  ans <- list()


  doctor <- c(1:3)
  for (i in 1:n.entra)
  {

    available <- c(t.chegada[i] >= tm1, t.chegada[i] >= tm2, t.chegada[i] >= tm3)
    isavailable <- sum(available)
    if(sum(available) > 0)
    {
      choose <- resample(doctor[available], 1)
    }
    if (t.chegada[i] >= tm1 && isavailable > 0 && choose == 1)
    {
      ans <- atendimento_medico(t.clinica, medico, t.inicio, t.chegada, t.saida, t.espera, dur.consulta, i, tm1, 1, t.chegada[i], t.entra, tol, atendido)
      tm1 <- ans$target
    }
    else if ( t.chegada[i] >= tm2 && isavailable > 0 && choose == 2)
    {     
      ans <- atendimento_medico(t.clinica, medico, t.inicio, t.chegada, t.saida, t.espera, dur.consulta, i, tm2, 2, t.chegada[i], t.entra, tol, atendido)
      tm2 <- ans$target
    }
    else if(t.chegada[i] >= tm3 && isavailable > 0 && choose == 3)
    {
      ans <- atendimento_medico(t.clinica, medico, t.inicio, t.chegada, t.saida, t.espera, dur.consulta, i, tm3, 3, t.chegada[i], t.entra, tol, atendido)
      tm3 <- ans$target
    }
    else if (tm1 < tm2 && tm1 < tm3)
    {
      ans <- atendimento_medico(t.clinica, medico, t.inicio, t.chegada, t.saida, t.espera, dur.consulta, i, tm1, 1, tm1, t.entra, tol, atendido)
      tm1 <- ans$target
    }
    else if(tm2 < tm1 && tm2 < tm3)
    {      
      ans <- atendimento_medico(t.clinica, medico, t.inicio, t.chegada, t.saida, t.espera, dur.consulta, i, tm2, 2, tm2, t.entra, tol, atendido)
      tm2 <- ans$target
    }
    else 
    {
      ans <- atendimento_medico(t.clinica, medico, t.inicio, t.chegada, t.saida, t.espera, dur.consulta, i, tm3, 3, tm3, t.entra, tol, atendido)
      tm3 <- ans$target
    }

        t.clinica = ans$clinica
        t.inicio = ans$inicio
        medico = ans$medico
        t.chegada = ans$chegada
        t.saida = ans$saida
        t.espera = ans$espera
        dur.consulta = ans$consulta
        atendido = ans$atendido

        log <- rbind(log, ans$log)

  }

  # print('writing csv')
  # write.csv(log, '../log.csv', row.names = FALSE)
  log <- log %>% filter(t_espera > 0)

  get.in.line <- log %>% select(medico, t_chegada)
  get.in.line <- get.in.line %>% mutate(line_1 = ifelse(medico == 1, +1, 0)) %>% 
  mutate(line_2 = ifelse(medico == 2, +1, 0)) %>% mutate(line_3 = ifelse(medico == 3, +1, 0)) %>% select(-medico)
  names(get.in.line) <- c("t", "line_1", "line_2", "line_3")

  get.out.line <- log %>% select(medico, t_inicio, atendido)
  get.out.line <- get.out.line %>% filter(atendido == TRUE) %>% mutate(line_1 = ifelse(medico == 1, -1, 0)) %>% 
  mutate(line_2 = ifelse(medico == 2, -1, 0)) %>% mutate(line_3 = ifelse(medico == 3, -1, 0)) %>% select(-medico, -atendido)
  names(get.out.line) <- c("t", "line_1", "line_2", "line_3")

  log <- rbind(get.in.line, get.out.line) %>% arrange(t) %>%
        mutate(line_1 = cumsum(line_1)) %>%
        mutate(line_2 = cumsum(line_2)) %>%
        mutate(line_3 = cumsum(line_3))

  ans <- data.frame(t.chegada = t.chegada,
                    t.saida   = t.saida,
                    t.espera  = t.espera,
                    t.clinica = t.clinica,
                    medico = medico, 
                    atendido = atendido)

  ans <- ans %>% filter(atendido == TRUE)

  return(list(ans = ans, log = log))
}

Nrep <- 1000

n.pac     <- c()
n.espera  <- c()
tm.espera <- c()
t.fim     <- c()
n.natendidos <- c()

lines <- list()
for( i in 1: Nrep) {
  res <- simula.funcionamento()
  log <- res$log
  res <- res$ans
  lines[[i]] <- log

  # Respostas:
  n.natendidos <- c(n.natendidos, log$line_1[length(log$line_1)] + log$line_2[length(log$line_2)] + log$line_3[length(log$line_3)] )
  n.pac        <- c(n.pac, nrow(res))                         # 1
  n.espera     <- c(n.espera, sum(res$t.espera > 0))          # 2
  tm.espera    <- c(tm.espera, mean(res$t.espera))            # 3
  t.fim        <- c(t.fim, 9 * 3600 + res$t.saida[nrow(res)]) # 4

}

# print(tm.espera)
# # Conversão de unidades temporais
# tm.espera <- tm.espera / 60 # em minutos
# t.fim <- t.fim / 3600       # em horas

# Histogramas
pdf(file="../first_stage_new.pdf", title = "3 Atendendo")

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
