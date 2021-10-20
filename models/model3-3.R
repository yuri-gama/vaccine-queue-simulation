rm(list = ls())

set.seed(13)  # para tornar a simulação reprodutível

# Função que simula um dia de funcionamento da clínica



simula.funcionamento <- function(t.abre = 9, 
                                 t.fecha = 16, 
                                 lambda.chegada = 1/10,
                                 dur.min =5,
                                 dur.max = 20, scnd_line = FALSE, data_1st = data.frame()){



atendimento_medico <- function(t.clinica, medico, t.inicio, t.chegada, t.saida, t.espera, dur.consulta, i, target, medico_num, t_inicio_consulta){
            medico[i]     <- medico_num
            t.inicio[i]  <- t_inicio_consulta                   # inicio da consulta
            t.saida[i]   <- t.inicio [i] + dur.consulta [i] # fim da consulta
            t.espera[i]  <- t.inicio [i] - t.chegada [i]    # tempo de espera ate consulta
            t.clinica[i] <- t.saida [i]  - t.chegada [i]    # tempo que passou na clinica
            target <- t.saida [i]
            ans <- list(clinica = t.clinica, medico = medico, inicio = t.inicio, 
            chegada = t.chegada, saida = t.saida, espera = t.espera, consulta = dur.consulta, target = target)
            return (ans)
                            # fim da consulta de M2
}


  ans <- list()
  
  ## DEFINICOES DE VARIAVEIS:
  # clinica
  t.entra  <- ( t.fecha - t.abre ) * 3600  # duração do expediente na clínica (em segundos)
                                           # tempo máximo de admissao de pacientes
  
  # pacientes
  lambda.chegada <- lambda.chegada / 60  # taxa de chegadas por segundo 
  x <- t.entra * lambda.chegada*5       # numero de pacientes que chegam ate a clinica
                                         # (valor superestimado => fator 5)
  t.chegada <- c()
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
  if(length(data_1st) != 0 && scnd_line == TRUE){
    t.chegada <- data_1st$t.saida
    dur.consulta <- dur.consulta [1:length(t.chegada)]
    n.entra      <- length(dur.consulta)  
    }
  else{
    t.chegada    <- t.chegada[pac.entra]         # tempo de chegada dos pacientes admitidos
  dur.consulta <- dur.consulta [pac.entra]     # duracao da consulta dos pacientes admitidos
  n.entra      <- length(pac.entra)        
  }    # no. de pacientes admitidos na clinica
  
  
  ## INICIALIZACAO:
  ## ANTES DE INICIAR AS CONSULTAS:
  t.inicio  <- c() # tempo de inicio da consulta
  t.espera  <- c() # tempo de espera dos pacientes
  t.saida   <- c() # tempo de saida dos pacientes
  t.clinica <- c() # tempo que os pacientes passaram na clinica
  medico    <- c() # medico que atendeu os pacientes
  tm1 <- tm2 <- tm3 <- 0  # instante em que M1 (médico 1) e M2 (médico 2) ficam livres

if(length(t.chegada) == 0){
    print("deu ruim")
    ans <- data.frame(t.chegada = t.chegada,
                t.saida   = t.saida,
                t.espera  = t.espera,
                t.clinica = t.clinica,
                medico = medico)
return(ans)
}
  ## PRIMEIRA CONSULTA: 
  # |-> medicos iniciam dia desocupados
  # |-> medico 1 atende
  # |-> paciente nao espera
  # |-> tamanho da fila = 0
  # t.inicio = tempo de chegada do 1o. paciente 
  # t.saida  = t.inicio + duracao da consulta

  medico[1]     <- 1
  t.espera [1]  <- 0
  t.inicio [1]  <- t.chegada [1]
  t.saida [1]   <- t.inicio [1] + dur.consulta [1]
  t.clinica [1] <- t.saida [1]  - t.chegada [1]
  tm1 <- t.saida [1]

  ## DEMAIS CONSULTAS (> 2o. paciente): 
  # verifica se medico 1 esta livre
  # |-> sim: medico 1 atende => paciente nao espera
  # |-> não: verifica se medico 2 esta livre
  #     |-> sim: medico 2 atende => paciente nao espera
  #     |-> não: paciente espera
  ans <- list()
  
  for (i in 2:n.entra){
    if (t.chegada[i] >= tm1){
        ans <- atendimento_medico(t.clinica, medico, t.inicio, t.chegada, t.saida, t.espera, dur.consulta, i, tm1, 1, t.chegada[i])
        tm1 <- ans$target
    }
    else if ( t.chegada[i] >= tm2 ){     
            ans <- atendimento_medico(t.clinica, medico, t.inicio, t.chegada, t.saida, t.espera, dur.consulta, i, tm2, 2, t.chegada[i])
            tm2 <- ans$target
            }
    else if(t.chegada[i] >= tm3){
            ans <- atendimento_medico(t.clinica, medico, t.inicio, t.chegada, t.saida, t.espera, dur.consulta, i, tm3, 3, t.chegada[i])
            tm3 <- ans$target
            }
    else if (tm1 < tm2 && tm1 < tm3){
            ans <- atendimento_medico(t.clinica, medico, t.inicio, t.chegada, t.saida, t.espera, dur.consulta, i, tm1, 1, tm1)
            tm1 <- ans$target
                    }
    else if(tm2 < tm1 && tm2 < tm3){      
            ans <- atendimento_medico(t.clinica, medico, t.inicio, t.chegada, t.saida, t.espera, dur.consulta, i, tm2, 1, tm2)
            tm2 <- ans$target
    }else {
        ans <- atendimento_medico(t.clinica, medico, t.inicio, t.chegada, t.saida, t.espera, dur.consulta, i, tm3, 1, tm3)
        tm3 <- ans$target
        }

        t.clinica = ans$clinica
        t.inicio = ans$inicio
        medico = ans$medico
        t.chegada = ans$chegada
        t.saida = ans$saida
        t.espera = ans$espera
        dur.consulta = ans$consulta

    }

  ans <- data.frame(t.chegada = t.chegada,
                    t.saida   = t.saida,
                    t.espera  = t.espera,
                    t.clinica = t.clinica,
                    medico = medico)
  return(ans)
}

Nrep <- 10000
n.pac     <- c()
n.espera  <- c()
tm.espera <- c()
t.fim     <- c()

n.pac2     <- c()
n.espera2  <- c()
tm.espera2 <- c()
t.esperatotal     <- c()

for( i in 1: Nrep) {
  res <- simula.funcionamento()
  res2 <- simula.funcionamento(scnd_line = TRUE, data_1st = res)
  # Respostas:
  n.pac     <- c(n.pac, nrow(res))                         # 1
  n.espera  <- c(n.espera, sum(res$t.espera > 0))          # 2
  tm.espera <- c(tm.espera, mean(res$t.espera))            # 3
  t.fim     <- c(t.fim, 9 * 3600 + res$t.saida[nrow(res)]) # 4

  n.pac2     <- c(n.pac2, nrow(res2))                         # 1
  n.espera2  <- c(n.espera2, sum(res2$t.espera > 0))          # 2
  tm.espera2 <- c(tm.espera2, mean(res2$t.espera))            # 3
  t.esperatotal     <- c(t.esperatotal, tm.espera + tm.espera2) # 4
}
# print(tm.espera)
# # Conversão de unidades temporais
# tm.espera <- tm.espera / 60 # em minutos
# t.fim <- t.fim / 3600       # em horas

# Histogramas
pdf(file="3Filass.pdf", title = "3 Atendendo")
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

# second line
hist(n.pac2, 
     col = "gray", 
     main = "No. de pacientes atendidos 2",
     xlab ="")
abline(v   = quantile(n.pac2, c(0.025, 0.5, 0.975)), 
       col = c(4, 2, 4), 
       lty = c(2, 1, 2), 
       lwd = 2)
       
hist(n.espera2, 
     col = "gray", 
     main = "No. de pacientes que esperaram 2",
     xlab ="")
abline(v   = quantile(n.espera2, c(0.025, 0.5, 0.975)), 
       col = c(4, 2, 4), 
       lty = c(2, 1, 2), 
       lwd = 2)

hist(tm.espera2/60, 
     col = "gray",
     main = "Tempo médio de Espera (min) 2",
     xlab ="")
abline(v   = quantile(tm.espera2/60, c(0.025, 0.5, 0.975)), 
       col = c(4, 2, 4), 
       lty = c(2, 1, 2), 
       lwd = 2)


       hist(t.esperatotal/60, 
     col = "gray",
     main = "Tempo médio de Espera Total (min)",
     xlab ="")
abline(v   = quantile(t.esperatotal/60, c(0.025, 0.5, 0.975)), 
       col = c(4, 2, 4), 
       lty = c(2, 1, 2), 
       lwd = 2)
