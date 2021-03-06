library(dplyr)
library(RJSONIO) # reads json files

# reads process parameters
distribution_parameters <- fromJSON("parameters.json")

# Dados segundo estágio

vaccination_shape <- distribution_parameters$vaccination[[1]]$shape
vaccination_mean <- distribution_parameters$vaccination[[1]]$mean
vaccination_sdlog <- vaccination_shape
vaccination_meanlog <- log(vaccination_mean)

# Função que simula um dia de funcionamento da clínica

resample <- function(x, ...) x[sample.int(length(x), ...)]

atendimento_medico_segundo_estagio <- function(t.clinica, medico, t.inicio, t.chegada, t.saida, t.espera, dur.consulta, i, target, medico_num, t_inicio_consulta, t.entra, tol, atendido){
    medico[i]     <- medico_num
    t.inicio[i]  <- t_inicio_consulta                   # inicio da consulta
    t.saida[i]   <- t.inicio [i] + dur.consulta [i] # fim da consulta
    t.espera[i]  <- t.inicio [i] - t.chegada [i]    # tempo de espera ate consulta
    t.clinica[i] <- t.saida [i]  - t.chegada [i]    # tempo que passou no posto
    target <- t.saida [i]

    atendido[i] <- TRUE

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

simula.funcionamento.segundo.estagio.2 <- function(chegada, 
                                 t.abre = 8, 
                                 t.fecha = 16
                                 ){
  log <- data.frame()
  ans <- list()
  tol <- 30*60
  
  ## DEFINICOES DE VARIAVEIS:
  # clinica
  t.entra  <- ( t.fecha - t.abre ) * 3600  # duração do expediente na clínica (em segundos)
                                           # tempo máximo de admissao de pacientes
  
  # pacientes
  lambda.chegada <- arrival_lambda  # taxa de chegadas por segundo 
  x <- t.entra * lambda.chegada *5      # numero de pacientes que chegam ate a clinica
                                         # (valor superestimado => fator 5)
  
  chegada <- chegada %>% arrange(desc(t.saida))

  t.chegada <- chegada$t.saida
 
  # gera duracao de cada consulta dos pacientes que entraram no posto
  dur.consulta <- rlnorm(x,  meanlog = vaccination_meanlog, sdlog = vaccination_sdlog)
  
  # seleciona pacientes que serao admitidos no posto
  # serão atendidos apenas aqueles que chegarem antes de `t.entra`:
  n.entra      <- length(t.chegada)            # no. de pacientes admitidos no posto
  dur.consulta <- dur.consulta [1:n.entra]     # duracao da consulta dos pacientes admitidos
  
 
  ## INICIALIZACAO:
  ## ANTES DE INICIAR AS CONSULTAS:
  t.inicio  <- c() # tempo de inicio da consulta
  t.espera  <- c() # tempo de espera dos pacientes
  t.saida   <- c() # tempo de saida dos pacientes
  t.clinica <- c() # tempo que os pacientes passaram no posto
  atendido <- rep(FALSE, length(t.chegada))
  medico    <- c() # medico que atendeu os pacientes
  tm1 <- tm2 <- 0  # instante em que M1 (médico 1) e M2 (médico 2) ficam livres
  
  ans <- list()


  doctor <- c(1:2)

  for (i in 1:n.entra)
  {

    available <- c(t.chegada[i] >= tm1, t.chegada[i] >= tm2)
    isavailable <- sum(available)
    if(sum(available) > 0)
    {
      choose <- resample(doctor[available], 1)
    }
    if (t.chegada[i] >= tm1 && isavailable > 0 && choose == 1)
    {
      ans <- atendimento_medico_segundo_estagio(t.clinica, medico, t.inicio, t.chegada, t.saida, t.espera, dur.consulta, i, tm1, 1, t.chegada[i], t.entra, tol, atendido)
      tm1 <- ans$target
    }
    else if ( t.chegada[i] >= tm2 && isavailable > 0 && choose == 2)
    {     
      ans <- atendimento_medico_segundo_estagio(t.clinica, medico, t.inicio, t.chegada, t.saida, t.espera, dur.consulta, i, tm2, 2, t.chegada[i], t.entra, tol, atendido)
      tm2 <- ans$target
    }
    else if (tm1 < tm2)
    {
      ans <- atendimento_medico_segundo_estagio(t.clinica, medico, t.inicio, t.chegada, t.saida, t.espera, dur.consulta, i, tm1, 1, tm1, t.entra, tol, atendido)
      tm1 <- ans$target
    }
    else
    {      
      ans <- atendimento_medico_segundo_estagio(t.clinica, medico, t.inicio, t.chegada, t.saida, t.espera, dur.consulta, i, tm2, 2, tm2, t.entra, tol, atendido)
      tm2 <- ans$target
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
  mutate(line_2 = ifelse(medico == 2, +1, 0)) %>% select(-medico)
  names(get.in.line) <- c("t", "line_1", "line_2")

  get.out.line <- log %>% select(medico, t_inicio, atendido)
  get.out.line <- get.out.line %>% filter(atendido == TRUE) %>% mutate(line_1 = ifelse(medico == 1, -1, 0)) %>% 
  mutate(line_2 = ifelse(medico == 2, -1, 0)) %>% select(-medico, -atendido)
  names(get.out.line) <- c("t", "line_1", "line_2")

  log <- rbind(get.in.line, get.out.line) %>% arrange(t) %>%
        mutate(line_1 = cumsum(line_1)) %>%
        mutate(line_2 = cumsum(line_2)) 

  ans <- data.frame(t.chegada = t.chegada,
                    t.saida   = t.saida,
                    t.espera  = t.espera,
                    t.clinica = t.clinica,
                    medico = medico, 
                    atendido = atendido,
                    t.total = t.saida - chegada$t.chegada)

  ans <- ans %>% filter(atendido == TRUE)

  return(list(ans = ans, log = log))
}

simula.funcionamento.segundo.estagio.3 <- function(chegada, 
                                 t.abre = 8, 
                                 t.fecha = 16
                                 ){
  log <- data.frame()
  ans <- list()
  tol <- 30*60
  
  ## DEFINICOES DE VARIAVEIS:
  # clinica
  t.entra  <- ( t.fecha - t.abre ) * 3600  # duração do expediente na clínica (em segundos)
                                           # tempo máximo de admissao de pacientes
  
  # pacientes
  lambda.chegada <- arrival_lambda  # taxa de chegadas por segundo 
  x <- t.entra * lambda.chegada *5      # numero de pacientes que chegam ate a clinica
                                         # (valor superestimado => fator 5)
  
  chegada <- chegada %>% arrange(desc(t.saida))

  t.chegada <- chegada$t.saida
 
  # gera duracao de cada consulta dos pacientes que entraram no posto
  dur.consulta <- rlnorm(x,  meanlog = vaccination_meanlog, sdlog = vaccination_sdlog)
  
  # seleciona pacientes que serao admitidos no posto
  # serão atendidos apenas aqueles que chegarem antes de `t.entra`:
  n.entra      <- length(t.chegada)            # no. de pacientes admitidos no posto
  dur.consulta <- dur.consulta [1:n.entra]     # duracao da consulta dos pacientes admitidos
  
 
  ## INICIALIZACAO:
  ## ANTES DE INICIAR AS CONSULTAS:
  t.inicio  <- c() # tempo de inicio da consulta
  t.espera  <- c() # tempo de espera dos pacientes
  t.saida   <- c() # tempo de saida dos pacientes
  t.clinica <- c() # tempo que os pacientes passaram no posto
  atendido <- rep(FALSE, length(t.chegada))
  medico    <- c() # medico que atendeu os pacientes
  tm1 <- tm2 <- tm3 <- 0  # instante em que M1 (médico 1) e M2 (médico 2) ficam livres
  
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
      ans <- atendimento_medico_segundo_estagio(t.clinica, medico, t.inicio, t.chegada, t.saida, t.espera, dur.consulta, i, tm1, 1, t.chegada[i], t.entra, tol, atendido)
      tm1 <- ans$target
    }
    else if ( t.chegada[i] >= tm2 && isavailable > 0 && choose == 2)
    {     
      ans <- atendimento_medico_segundo_estagio(t.clinica, medico, t.inicio, t.chegada, t.saida, t.espera, dur.consulta, i, tm2, 2, t.chegada[i], t.entra, tol, atendido)
      tm2 <- ans$target
    }
    else if(t.chegada[i] >= tm3 && isavailable > 0 && choose == 3)
    {
      ans <- atendimento_medico_segundo_estagio(t.clinica, medico, t.inicio, t.chegada, t.saida, t.espera, dur.consulta, i, tm3, 3, t.chegada[i], t.entra, tol, atendido)
      tm3 <- ans$target
    }
    else if (tm1 < tm2 && tm1 < tm3)
    {
      ans <- atendimento_medico_segundo_estagio(t.clinica, medico, t.inicio, t.chegada, t.saida, t.espera, dur.consulta, i, tm1, 1, tm1, t.entra, tol, atendido)
      tm1 <- ans$target
    }
    else if(tm2 < tm1 && tm2 < tm3)
    {      
      ans <- atendimento_medico_segundo_estagio(t.clinica, medico, t.inicio, t.chegada, t.saida, t.espera, dur.consulta, i, tm2, 2, tm2, t.entra, tol, atendido)
      tm2 <- ans$target
    }
    else 
    {
      ans <- atendimento_medico_segundo_estagio(t.clinica, medico, t.inicio, t.chegada, t.saida, t.espera, dur.consulta, i, tm3, 3, tm3, t.entra, tol, atendido)
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
                    atendido = atendido,
                    t.total = t.saida - chegada$t.chegada)

  ans <- ans %>% filter(atendido == TRUE)

  return(list(ans = ans, log = log))
}

simula.funcionamento.segundo.estagio.4 <- function(chegada, 
                                 t.abre = 8, 
                                 t.fecha = 16
                                 ){
  log <- data.frame()
  ans <- list()
  tol <- 30*60
  
  ## DEFINICOES DE VARIAVEIS:
  # clinica
  t.entra  <- ( t.fecha - t.abre ) * 3600  # duração do expediente na clínica (em segundos)
                                           # tempo máximo de admissao de pacientes
  
  # pacientes
  lambda.chegada <- arrival_lambda  # taxa de chegadas por segundo 
  x <- t.entra * lambda.chegada *5      # numero de pacientes que chegam ate a clinica
                                         # (valor superestimado => fator 5)
  
  chegada <- chegada %>% arrange(desc(t.saida))

  t.chegada <- chegada$t.saida
 
  # gera duracao de cada consulta dos pacientes que entraram no posto
  dur.consulta <- rlnorm(x,  meanlog = vaccination_meanlog, sdlog = vaccination_sdlog)
  
  # seleciona pacientes que serao admitidos no posto
  # serão atendidos apenas aqueles que chegarem antes de `t.entra`:
  n.entra      <- length(t.chegada)            # no. de pacientes admitidos no posto
  dur.consulta <- dur.consulta [1:n.entra]     # duracao da consulta dos pacientes admitidos
  
 
  ## INICIALIZACAO:
  ## ANTES DE INICIAR AS CONSULTAS:
  t.inicio  <- c() # tempo de inicio da consulta
  t.espera  <- c() # tempo de espera dos pacientes
  t.saida   <- c() # tempo de saida dos pacientes
  t.clinica <- c() # tempo que os pacientes passaram no posto
  atendido <- rep(FALSE, length(t.chegada))
  medico    <- c() # medico que atendeu os pacientes
  tm1 <- tm2 <- tm3 <- tm4 <- 0  # instante em que M1 (médico 1) e M2 (médico 2) ficam livres
  
  ans <- list()


  doctor <- c(1:4)

  for (i in 1:n.entra)
  {

    available <- c(t.chegada[i] >= tm1, t.chegada[i] >= tm2, t.chegada[i] >= tm3, t.chegada[i] >= tm4)
    isavailable <- sum(available)
    if(sum(available) > 0)
    {
      choose <- resample(doctor[available], 1)
    }
    if (t.chegada[i] >= tm1 && isavailable > 0 && choose == 1)
    {
      ans <- atendimento_medico_segundo_estagio(t.clinica, medico, t.inicio, t.chegada, t.saida, t.espera, dur.consulta, i, tm1, 1, t.chegada[i], t.entra, tol, atendido)
      tm1 <- ans$target
    }
    else if ( t.chegada[i] >= tm2 && isavailable > 0 && choose == 2)
    {     
      ans <- atendimento_medico_segundo_estagio(t.clinica, medico, t.inicio, t.chegada, t.saida, t.espera, dur.consulta, i, tm2, 2, t.chegada[i], t.entra, tol, atendido)
      tm2 <- ans$target
    }
    else if(t.chegada[i] >= tm3 && isavailable > 0 && choose == 3)
    {
      ans <- atendimento_medico_segundo_estagio(t.clinica, medico, t.inicio, t.chegada, t.saida, t.espera, dur.consulta, i, tm3, 3, t.chegada[i], t.entra, tol, atendido)
      tm3 <- ans$target
    }
    else if(t.chegada[i] >= tm4 && isavailable > 0 && choose == 4)
    {
      ans <- atendimento_medico_segundo_estagio(t.clinica, medico, t.inicio, t.chegada, t.saida, t.espera, dur.consulta, i, tm4, 4, t.chegada[i], t.entra, tol, atendido)
      tm4 <- ans$target
    }
    else if (tm1 < tm2 && tm1 < tm3 && tm1 < tm4)
    {
      ans <- atendimento_medico_segundo_estagio(t.clinica, medico, t.inicio, t.chegada, t.saida, t.espera, dur.consulta, i, tm1, 1, tm1, t.entra, tol, atendido)
      tm1 <- ans$target
    }
    else if(tm2 < tm1 && tm2 < tm3 && tm2 < tm4)
    {      
      ans <- atendimento_medico_segundo_estagio(t.clinica, medico, t.inicio, t.chegada, t.saida, t.espera, dur.consulta, i, tm2, 2, tm2, t.entra, tol, atendido)
      tm2 <- ans$target
    }
    else if(tm3 < tm1 && tm3 < tm2 && tm3 < tm4)
    {
      ans <- atendimento_medico_segundo_estagio(t.clinica, medico, t.inicio, t.chegada, t.saida, t.espera, dur.consulta, i, tm3, 3, tm3, t.entra, tol, atendido)
      tm3 <- ans$target
    }
    else
    {
      ans <- atendimento_medico_segundo_estagio(t.clinica, medico, t.inicio, t.chegada, t.saida, t.espera, dur.consulta, i, tm4, 4, tm4, t.entra, tol, atendido)
      tm4 <- ans$target
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
  mutate(line_2 = ifelse(medico == 2, +1, 0)) %>% mutate(line_3 = ifelse(medico == 3, +1, 0)) %>% mutate(line_4 = ifelse(medico == 4, +1, 0)) %>% select(-medico)
  names(get.in.line) <- c("t", "line_1", "line_2", "line_3", "line_4")

  get.out.line <- log %>% select(medico, t_inicio, atendido)
  get.out.line <- get.out.line %>% filter(atendido == TRUE) %>% mutate(line_1 = ifelse(medico == 1, -1, 0)) %>% 
  mutate(line_2 = ifelse(medico == 2, -1, 0)) %>% mutate(line_3 = ifelse(medico == 3, -1, 0)) %>% mutate(line_4 = ifelse(medico == 4, -1, 0)) %>% select(-medico, -atendido)
  names(get.out.line) <- c("t", "line_1", "line_2", "line_3", "line_4")

  log <- rbind(get.in.line, get.out.line) %>% arrange(t) %>%
        mutate(line_1 = cumsum(line_1)) %>%
        mutate(line_2 = cumsum(line_2)) %>%
        mutate(line_3 = cumsum(line_3)) %>%
        mutate(line_4 = cumsum(line_4))

  ans <- data.frame(t.chegada = t.chegada,
                    t.saida   = t.saida,
                    t.espera  = t.espera,
                    t.clinica = t.clinica,
                    medico = medico, 
                    atendido = atendido,
                    t.total = t.saida - chegada$t.chegada)

  ans <- ans %>% filter(atendido == TRUE)

  return(list(ans = ans, log = log))
}