##################################################################################
# Informar o nome do experimento (para ler o arquivo json)
##################################################################################
NomeExperimento       <- "exp057"
##################################################################################

##################################################################################
# Instalação de Pacotes
#
# instalar pacote para ler e gravar arquivos xlsx
# install.packages("xlsx")
#
# instalar pacote para ler arquivos JSON
# install.packages("rjson")
#
# instalar pacote para gravar arquivos JSON
# install.packages("jsonlite")
#
# instalar a biblioteca para permutations
# install.packages('gtools')
#
# instalar a biblioteca para ...
# install.packages("bnlearn")
#
# instalar a biblioteca para ...
# install.packages("datasets")
#
# instalar a biblioteca para ...
# install.packages("mmpf")
#
# instalar a biblioteca para ...
# install.packages("caTools")
##################################################################################

# carregar a biblioteca para ler e gravar arquivos xlsx
library("xlsx")
# carregar a biblioteca para ler arquivos JSON
library("rjson")
# carregar a biblioteca para gravar arquivos JSON
library("jsonlite")
# carregar a biblioteca para permutations
library(gtools)
# carregar a biblioteca para rede bayesiana
library(bnlearn)
# carregar a biblioteca para bases de dados
library(datasets)
# carregar a biblioteca para funções
library(mmpf)
# carregar a biblioteca para calcular resultados
library(caTools)

##################################################################################
# FUNCTION : gerarDadosCenario
#
# determina para qual cenário serão criados os dados
# 1 - Iluminação
# 2 - Irrigação
# 3 - Swamp
#
##################################################################################
gerarDadosCenario <- function(){
  cat("======================================================\n")
  cat(" GERAÇÃO DE ARQUIVOS PARA O EXPERIMENTO COMPUTACIONAL \n")
  cat("======================================================\n")
  cat(" Experimento..: ", lista_experimento$numero,"\n")
  cat(" Cenário......: ", lista_experimento$cenario,"\n")
  cat("======================================================\n")
  
  if(lista_experimento$cenario=="1") {geraArquivosCenario1()}
  if(lista_experimento$cenario=="2") {geraArquivosCenario2()}
  if(lista_experimento$cenario=="3") {geraArquivosCenario3()}
  if(lista_experimento$cenario=="4") {geraArquivosCenario4()}
  if(lista_experimento$cenario=="5") {geraArquivosCenario5()}
  if(lista_experimento$cenario=="6") {geraArquivosCenario6()}
  if(lista_experimento$cenario=="7") {geraArquivosCenario7()}
  
  cat("======================================================\n")
  cat("  ARQUIVOS PARA O EXPERIMENTO COMPUTACIONAL GERADOS!  \n")
  cat("======================================================\n")
  cat("\n\n")
}


##################################################################################
# FUNCTION : geraArquivosCenario1
#
# controla a geração dos diversos arquivos com diferentes quantidades
# de registros para o experimento computacional do cenario 1
#
##################################################################################
geraArquivosCenario1 <- function(){

    for(i in 1:length(lista_controle$arquivos)){
      
      totLinhas = lista_experimento$registros[i]
      nomeArquivo = paste(NomeExperimento,"_",totLinhas,".csv",sep="",collapse="")
      
      if(lista_controle$arquivos[i]==0 || !(file.exists(nomeArquivo))){
        
        geraUmArquivoCenario1(lista_experimento$registros[i], nomeArquivo)
        lista_controle$arquivos[i] <- 1
        write_json(lista_controle, ArquivoControle)
        
      } else {
        
        cat ("arquivo para ",lista_experimento$registros[i],"registros já existe!\n" )
        
      }
    }
}


##################################################################################
# FUNCTION : geraArquivosCenario2
#
# controla a geração dos diversos arquivos com diferentes quantidades
# de registros para o experimento computacional do cenario 2
#
##################################################################################
geraArquivosCenario2 <- function(){
  
  for(i in 1:length(lista_controle$arquivos)){

    totLinhas = lista_experimento$registros[i]
    nomeArquivo = paste(NomeExperimento,"_",totLinhas,".csv",sep="",collapse="")

    if(lista_controle$arquivos[i]==0 || !(file.exists(nomeArquivo))){
      
      geraUmArquivoCenario2(lista_experimento$registros[i], nomeArquivo)
      lista_controle$arquivos[i] <- 1
      write_json(lista_controle, ArquivoControle)
      
    } else {
      
      cat ("arquivo para ",lista_experimento$registros[i],"registros já existe!\n" )
      
    }
  }
}

##################################################################################
# FUNCTION : geraArquivosCenario3
#
# controla a geração dos diversos arquivos com diferentes quantidades
# de registros para o experimento computacional do cenario 3
#
##################################################################################
geraArquivosCenario3 <- function(){
  
  for(i in 1:length(lista_controle$arquivos)){
    
    totLinhas = lista_experimento$registros[i]
    nomeArquivo = paste(NomeExperimento,"_",totLinhas,".csv",sep="",collapse="")
    
    if(lista_controle$arquivos[i]==0 || !(file.exists(nomeArquivo))){
      
      geraUmArquivoCenario3(lista_experimento$registros[i], nomeArquivo)
      lista_controle$arquivos[i] <- 1
      write_json(lista_controle, ArquivoControle)
      
    } else {
      
      cat ("arquivo para ",lista_experimento$registros[i],"registros já existe!\n" )
      
    }
  }
}


##################################################################################
# FUNCTION : geraArquivosCenario4
#
# controla a geração dos diversos arquivos com diferentes quantidades
# de registros para o experimento computacional do cenario 4
# 
# O cenário 4 é a FOG do cenário 3
##################################################################################
geraArquivosCenario4 <- function(){
  
  for(i in 1:length(lista_controle$arquivos)){
    
    totLinhas = lista_experimento$registros[i]
    nomeArquivo = paste(NomeExperimento,"_",totLinhas,".csv",sep="",collapse="")
    
    if(lista_controle$arquivos[i]==0 || !(file.exists(nomeArquivo))){
      
      geraUmArquivoCenario4(lista_experimento$registros[i], nomeArquivo)
      lista_controle$arquivos[i] <- 1
      write_json(lista_controle, ArquivoControle)
      
    } else {
      
      cat ("arquivo para ",lista_experimento$registros[i],"registros já existe!\n" )
      
    }
  }
}

##################################################################################
# FUNCTION : geraArquivosCenario5
#
# controla a geração dos diversos arquivos com diferentes quantidades
# de registros para o experimento computacional do cenario 5
# 
# O cenário 5 é a CLOUD do cenário 3
##################################################################################
geraArquivosCenario5 <- function(){
  
  for(i in 1:length(lista_controle$arquivos)){
    
    totLinhas = lista_experimento$registros[i]
    nomeArquivo = paste(NomeExperimento,"_",totLinhas,".csv",sep="",collapse="")
    
    if(lista_controle$arquivos[i]==0 || !(file.exists(nomeArquivo))){
      
      geraUmArquivoCenario5(lista_experimento$registros[i], nomeArquivo)
      lista_controle$arquivos[i] <- 1
      write_json(lista_controle, ArquivoControle)
      
    } else {
      
      cat ("arquivo para ",lista_experimento$registros[i],"registros já existe!\n" )
      
    }
  }
}


##################################################################################
# FUNCTION : geraArquivosCenario6
#
# controla a geração dos diversos arquivos com diferentes quantidades
# de registros para o experimento computacional do cenario 6
#
# o cenário 6 é o cenário 1 com metade dos sensores e atuadores
##################################################################################
geraArquivosCenario6 <- function(){
  
  for(i in 1:length(lista_controle$arquivos)){
    
    totLinhas = lista_experimento$registros[i]
    nomeArquivo = paste(NomeExperimento,"_",totLinhas,".csv",sep="",collapse="")
    
    if(lista_controle$arquivos[i]==0 || !(file.exists(nomeArquivo))){
      
      geraUmArquivoCenario6(lista_experimento$registros[i], nomeArquivo)
      lista_controle$arquivos[i] <- 1
      write_json(lista_controle, ArquivoControle)
      
    } else {
      
      cat ("arquivo para ",lista_experimento$registros[i],"registros já existe!\n" )
      
    }
  }
}


##################################################################################
# FUNCTION : geraArquivosCenario7
#
# controla a geração dos diversos arquivos com diferentes quantidades
# de registros para o experimento computacional do cenario 2
#
# o cenário 7 é o cenário 2 com metade dos sensores e atuadores
##################################################################################
geraArquivosCenario7 <- function(){
  
  for(i in 1:length(lista_controle$arquivos)){
    
    totLinhas = lista_experimento$registros[i]
    nomeArquivo = paste(NomeExperimento,"_",totLinhas,".csv",sep="",collapse="")
    
    if(lista_controle$arquivos[i]==0 || !(file.exists(nomeArquivo))){
      
      geraUmArquivoCenario7(lista_experimento$registros[i], nomeArquivo)
      lista_controle$arquivos[i] <- 1
      write_json(lista_controle, ArquivoControle)
      
    } else {
      
      cat ("arquivo para ",lista_experimento$registros[i],"registros já existe!\n" )
      
    }
  }
}



##################################################################################
# FUNCTION : processaDadosCenario
#
# determina para qual cenário serão processados os dados
# 1 - Iluminação
# 2 - Irrigação
# 3 - Swamp
#
##################################################################################
processaDadosCenario <- function(){
  
  cat("======================================================\n")
  cat(" INÍCIO DO PROCESSAMENTO DO EXPERIMENTO COMPUTACIONAL \n")
  cat("======================================================\n")
  cat(" Experimento..: ", lista_experimento$numero,"\n")
  cat(" Cenário......: ", lista_experimento$cenario,"\n")
  cat("======================================================\n")
  
  # roda a bateria de testes
  for(vez in 1:lista_experimento$bateria){
     if(lista_experimento$cenario=="1") {processaDadosCenario1(vez)}
     if(lista_experimento$cenario=="2") {processaDadosCenario2(vez)}
     if(lista_experimento$cenario=="3") {processaDadosCenario3(vez)}
     if(lista_experimento$cenario=="4") {processaDadosCenario4(vez)}
     if(lista_experimento$cenario=="5") {processaDadosCenario5(vez)}
     if(lista_experimento$cenario=="6") {processaDadosCenario6(vez)}
     if(lista_experimento$cenario=="7") {processaDadosCenario7(vez)}
  }
  
  cat("======================================================\n")
  cat(" TÉRMINO DO PROCESSAMENTO DO EXPERIMENTO COMPUTACIONAL\n")
  cat("======================================================\n")
  cat(" Experimento..: ", lista_experimento$numero,"\n")
  cat(" Cenário......: ", lista_experimento$cenario,"\n")
  cat("======================================================\n")
  cat("\n\n")
}

##################################################################################
# FUNCTION : processaDadosCenario1
#
# processamento de cada arquivo para cada percentual de buracos para o cenario 1
#
##################################################################################

processaDadosCenario1 <- function(vez){
  
  ################################################################################
  # Criar uma Black List e uma White List
  ################################################################################
  #
  
  elemento <- c("ss1", "ss2", "ss3", "ss4", "somafusao", "acao", "a1", "a2", "a3", "a4")
  
  #  o código abaixo faz uma combinatória e não um arranjo
  #
  #  matriz_combinatoria = matrix(combn(elemento, 2), 
  #                               ncol = 2, 
  #                               byrow = TRUE,
  #                               dimnames = list(NULL, c("from", "to")))
  
  
  # a quantidade de elementos em um arranjo simples é n!/(n-p)!
  
  matriz_arranjo = matrix(permutations(n=length(elemento),r=2,v=elemento), 
                          ncol = 2, 
                          byrow = FALSE,
                          dimnames = list(NULL, c("from", "to")))
  
  df_white = matrix(c("ss1","somafusao",
                      "ss2","somafusao",
                      "ss3","somafusao",
                      "ss4","somafusao",
                      "somafusao", "acao",
                      "acao","a1",
                      "acao","a2",
                      "acao","a3",
                      "acao","a4"), 
                    ncol = 2, 
                    byrow = TRUE,
                    dimnames = list(NULL, c("from", "to")))
  
  # gravar a white list em arquivo  
  nomeArquivoWhiteList = paste(NomeExperimento,"_WhiteList.xlsx",sep="",collapse="")
  if(!file.exists(nomeArquivoWhiteList)){
    write.xlsx(df_white,nomeArquivoWhiteList, row.names = FALSE)
  }
  
  
  df_black = matrix(vector(), 
                    nrow = nrow(matriz_arranjo) - nrow(df_white), 
                    ncol = 2,
                    dimnames = list(NULL, c("from", "to")))
  
  df_black <- geraBlackList(matriz_arranjo, df_white, df_black)
  
  # total de processamentos
  totalProcessamentos = lista_experimento$bateria * 
    length(lista_experimento$registros) * 
    length(lista_experimento$furos)
  
  # processamento de um arquivo de cada vez...
  for(i in 1:length(lista_experimento$registros)){
    for(j in 1:length(lista_experimento$furos)){
      
      linhaResultado = (vez-1) * length(lista_experimento$registros) * 
        length(lista_experimento$furos) + 
        (i-1)   * length(lista_experimento$furos) + j
      
      # carregando o controle do processamento
      lista_controle <- fromJSON(ArquivoControle)
      
      # verificando se já foi processado
      if(lista_controle$processados[linhaResultado]==1){
        cat ("Já Processado.: #", linhaResultado, 
             "/", totalProcessamentos,
             "- Bateria", vez,
             "- Arquivo com", lista_experimento$registros[i],
             "registros e",  lista_experimento$furos[j],
             "% de furos.\n")      
        
      } else {
        cat ("Processando...: #", linhaResultado, 
             "/", totalProcessamentos,
             "- Bateria", vez,
             "- Arquivo com", lista_experimento$registros[i],
             "registros e",  lista_experimento$furos[j],
             "% de furos.\n")      
        
        processaUmArquivoCenario1(vez, 
                                  linhaResultado,
                                  lista_experimento$registros[i],
                                  lista_experimento$furos[j],
                                  df_white,
                                  df_black) 
        # registrando o que foi processado
        lista_controle$processados[linhaResultado]=1
        write_json(lista_controle, ArquivoControle)
      }
    } # for j
  } # for i
}


##################################################################################
# FUNCTION : processaDadosCenario2
#
# processamento de cada arquivo para cada percentual de buracos para o cenario 2
#
##################################################################################
processaDadosCenario2 <- function(vez){

  ################################################################################
  # Criar uma Black List e uma White List
  ################################################################################
  #
  elemento <- c("c","s1","s2","s3","s4","f1","f2","r","a1","a2")

#  o código abaixo faz uma combinatória e não um arranjo
#
#  matriz_combinatoria = matrix(combn(elemento, 2), 
#                               ncol = 2, 
#                               byrow = TRUE,
#                               dimnames = list(NULL, c("from", "to")))

  
  # a quantidade de elementos em um arranjo simples é n!/(n-p)!
  
  matriz_arranjo = matrix(permutations(n=length(elemento),r=2,v=elemento), 
                               ncol = 2, 
                               byrow = FALSE,
                               dimnames = list(NULL, c("from", "to")))
  
  df_white = matrix(c("c" ,"r" ,
                      "s1","f1",
                      "s2","f1",
                      "s3","f2",
                      "s4","f2",
                      "f1","r" ,
                      "f2","r" ,
                      "r" ,"a1",
                      "r" ,"a2"), 
                    ncol = 2, 
                    byrow = TRUE,
                    dimnames = list(NULL, c("from", "to")))
  
  # gravar a white list em arquivo  
  nomeArquivoWhiteList = paste(NomeExperimento,"_WhiteList.xlsx",sep="",collapse="")
  if(!file.exists(nomeArquivoWhiteList)){
    write.xlsx(df_white,nomeArquivoWhiteList, row.names = FALSE)
  }
  

  df_black = matrix(vector(), 
                    nrow = nrow(matriz_arranjo) - nrow(df_white), 
                    ncol = 2,
                    dimnames = list(NULL, c("from", "to")))
  
  df_black <- geraBlackList(matriz_arranjo, df_white, df_black)
  
  # total de processamentos
  totalProcessamentos = lista_experimento$bateria * 
                        length(lista_experimento$registros) * 
                        length(lista_experimento$furos)
  
  # processamento de um arquivo de cada vez...
  for(i in 1:length(lista_experimento$registros)){
    for(j in 1:length(lista_experimento$furos)){
      
      linhaResultado = (vez-1) * length(lista_experimento$registros) * 
                                 length(lista_experimento$furos) + 
                       (i-1)   * length(lista_experimento$furos) + j

      # carregando o controle do processamento
      lista_controle <- fromJSON(ArquivoControle)
      
      # verificando se já foi processado
      if(lista_controle$processados[linhaResultado]==1){
        cat ("Já Processado.: #", linhaResultado, 
             "/", totalProcessamentos,
             "- Bateria", vez,
             "- Arquivo com", lista_experimento$registros[i],
             "registros e",  lista_experimento$furos[j],
             "% de furos.\n")      
        
      } else {
        cat ("Processando...: #", linhaResultado, 
             "/", totalProcessamentos,
             "- Bateria", vez,
             "- Arquivo com", lista_experimento$registros[i],
             "registros e",  lista_experimento$furos[j],
             "% de furos.\n")      
        
        processaUmArquivoCenario2(vez, 
                                  linhaResultado,
                                  lista_experimento$registros[i],
                                  lista_experimento$furos[j],
                                  df_white,
                                  df_black) 
        # registrando o que foi processado
        lista_controle$processados[linhaResultado]=1
        write_json(lista_controle, ArquivoControle)
      }
    } # for j
  } # for i

}

##################################################################################
# FUNCTION : processaDadosCenario3
#
# processamento de cada arquivo para cada percentual de buracos para o cenario 3
#
##################################################################################
processaDadosCenario3 <- function(vez){
  
  ################################################################################
  # Criar uma Black List e uma White List
  ################################################################################
  #
  elemento <- c("en","mi","gtb","mq1","ls","mq2","las","outFog","aa1","iot","mo","ori","ql","cdb","aa2","outCloud")
                
  
  #  o código abaixo faz uma combinatória e não um arranjo
  #
  #  matriz_combinatoria = matrix(combn(elemento, 2), 
  #                               ncol = 2, 
  #                               byrow = TRUE,
  #                               dimnames = list(NULL, c("from", "to")))
  
  
  # a quantidade de elementos em um arranjo simples é n!/(n-p)!
  
  matriz_arranjo = matrix(permutations(n=length(elemento),r=2,v=elemento), 
                          ncol = 2, 
                          byrow = FALSE,
                          dimnames = list(NULL, c("from", "to")))
  
    df_white = matrix(c("en"  , "mi" ,
                        "mi"  , "gtb",
                        "gtb" , "mq1",
                        "mq1" , "ls" ,
                        "ls"  , "mq2",
                        "mq2" , "las",
                        "aa1" , "gtb",
                        "aa1" , "mq1",
                        "aa1" , "ls" ,
                        "aa1" , "mq2",
                        "aa1" , "las",
                        "las" , "outFog",
                        "outFog","iot",
                        "iot" , "ori",
                        "ori" , "mo" ,
                        "ori" , "ql" ,
                        "ql"  , "cdb",
                        "cdb" , "outCloud",
                        "aa2" , "iot",
                        "aa2" , "ori",
                        "aa2" , "mo" ,
                        "aa2" , "ql" ,
                        "aa2" , "cdb"),
                    ncol = 2, 
                    byrow = TRUE,
                    dimnames = list(NULL, c("from", "to")))
    
  # gravar a white list em arquivo  
    nomeArquivoWhiteList = paste(NomeExperimento,"_WhiteList.xlsx",sep="",collapse="")
    if(!file.exists(nomeArquivoWhiteList)){
      write.xlsx(df_white,nomeArquivoWhiteList, row.names = FALSE)
    }
  
  df_black = matrix(vector(), 
                    nrow = nrow(matriz_arranjo) - nrow(df_white), 
                    ncol = 2,
                    dimnames = list(NULL, c("from", "to")))
  
  df_black <- geraBlackList(matriz_arranjo, df_white, df_black)
  
  # total de processamentos
  totalProcessamentos = lista_experimento$bateria * 
                        length(lista_experimento$registros) * 
                        length(lista_experimento$furos)
  
  # processamento de um arquivo de cada vez...
  for(i in 1:length(lista_experimento$registros)){
    for(j in 1:length(lista_experimento$furos)){
      
      linhaResultado = (vez-1) * length(lista_experimento$registros) * 
        length(lista_experimento$furos) + 
        (i-1)   * length(lista_experimento$furos) + j
      
      # carregando o controle do processamento
      lista_controle <- fromJSON(ArquivoControle)
      
      # verificando se já foi processado
      if(lista_controle$processados[linhaResultado]==1){
        cat ("Já Processado.: #", linhaResultado, 
             "/", totalProcessamentos,
             "- Bateria", vez,
             "- Arquivo com", lista_experimento$registros[i],
             "registros e",  lista_experimento$furos[j],
             "% de furos.\n")      
        
      } else {
        cat ("Processando...: #", linhaResultado, 
             "/", totalProcessamentos,
             "- Bateria", vez,
             "- Arquivo com", lista_experimento$registros[i],
             "registros e",  lista_experimento$furos[j],
             "% de furos.\n")      

        
        
        
        processaUmArquivoCenario3(vez, 
                                  linhaResultado,
                                  lista_experimento$registros[i],
                                  lista_experimento$furos[j],
                                  df_white,
                                  df_black) 
        # registrando o que foi processado
        lista_controle$processados[linhaResultado]=1
        write_json(lista_controle, ArquivoControle)
      }
    } # for j
  } # for i
}


##################################################################################
# FUNCTION : processaDadosCenario4
#
# processamento de cada arquivo para cada percentual de buracos para o cenario 4
#
##################################################################################
processaDadosCenario4 <- function(vez){
  
  ################################################################################
  # Criar uma Black List e uma White List
  ################################################################################
  #
  elemento <- c("gtb","mq1","ls","mq2","las","outFog","aa1")

  #  o código abaixo faz uma combinatória e não um arranjo
  #
  #  matriz_combinatoria = matrix(combn(elemento, 2), 
  #                               ncol = 2, 
  #                               byrow = TRUE,
  #                               dimnames = list(NULL, c("from", "to")))
  
  
  # a quantidade de elementos em um arranjo simples é n!/(n-p)!
  
  matriz_arranjo = matrix(permutations(n=length(elemento),r=2,v=elemento), 
                          ncol = 2, 
                          byrow = FALSE,
                          dimnames = list(NULL, c("from", "to")))
  
  df_white = matrix(c("gtb" , "mq1",
                      "mq1" , "ls" ,
                      "ls"  , "mq2",
                      "mq2" , "las",
                      "aa1" , "gtb",
                      "aa1" , "mq1",
                      "aa1" , "ls" ,
                      "aa1" , "mq2",
                      "aa1" , "las",
                      "las" , "outFog"),
                    ncol = 2, 
                    byrow = TRUE,
                    dimnames = list(NULL, c("from", "to")))
  
  # gravar a white list em arquivo  
  nomeArquivoWhiteList = paste(NomeExperimento,"_WhiteList.xlsx",sep="",collapse="")
  if(!file.exists(nomeArquivoWhiteList)){
    write.xlsx(df_white,nomeArquivoWhiteList, row.names = FALSE)
  }
  
  df_black = matrix(vector(), 
                    nrow = nrow(matriz_arranjo) - nrow(df_white), 
                    ncol = 2,
                    dimnames = list(NULL, c("from", "to")))
  
  df_black <- geraBlackList(matriz_arranjo, df_white, df_black)
  
  # total de processamentos
  totalProcessamentos = lista_experimento$bateria * 
    length(lista_experimento$registros) * 
    length(lista_experimento$furos)
  
  # processamento de um arquivo de cada vez...
  for(i in 1:length(lista_experimento$registros)){
    for(j in 1:length(lista_experimento$furos)){
      
      linhaResultado = (vez-1) * length(lista_experimento$registros) * 
        length(lista_experimento$furos) + 
        (i-1)   * length(lista_experimento$furos) + j
      
      # carregando o controle do processamento
      lista_controle <- fromJSON(ArquivoControle)
      
      # verificando se já foi processado
      if(lista_controle$processados[linhaResultado]==1){
        cat ("Já Processado.: #", linhaResultado, 
             "/", totalProcessamentos,
             "- Bateria", vez,
             "- Arquivo com", lista_experimento$registros[i],
             "registros e",  lista_experimento$furos[j],
             "% de furos.\n")      
        
      } else {
        cat ("Processando...: #", linhaResultado, 
             "/", totalProcessamentos,
             "- Bateria", vez,
             "- Arquivo com", lista_experimento$registros[i],
             "registros e",  lista_experimento$furos[j],
             "% de furos.\n")      
        
        
        
        
        processaUmArquivoCenario4(vez, 
                                  linhaResultado,
                                  lista_experimento$registros[i],
                                  lista_experimento$furos[j],
                                  df_white,
                                  df_black) 
        # registrando o que foi processado
        lista_controle$processados[linhaResultado]=1
        write_json(lista_controle, ArquivoControle)
      }
    } # for j
  } # for i
}


##################################################################################
# FUNCTION : processaDadosCenario5
#
# processamento de cada arquivo para cada percentual de buracos para o cenario 5
#
##################################################################################
processaDadosCenario5 <- function(vez){
  
  ################################################################################
  # Criar uma Black List e uma White List
  ################################################################################
  #
  elemento <- c("iot","mo","ori","ql","cdb","aa2","outCloud")
  
  
  #  o código abaixo faz uma combinatória e não um arranjo
  #
  #  matriz_combinatoria = matrix(combn(elemento, 2), 
  #                               ncol = 2, 
  #                               byrow = TRUE,
  #                               dimnames = list(NULL, c("from", "to")))
  
  
  # a quantidade de elementos em um arranjo simples é n!/(n-p)!
  
  matriz_arranjo = matrix(permutations(n=length(elemento),r=2,v=elemento), 
                          ncol = 2, 
                          byrow = FALSE,
                          dimnames = list(NULL, c("from", "to")))
  
  df_white = matrix(c("iot" , "ori",
                      "ori" , "mo" ,
                      "ori" , "ql" ,
                      "ql"  , "cdb",
                      "cdb" , "outCloud",
                      "aa2" , "iot",
                      "aa2" , "ori",
                      "aa2" , "mo" ,
                      "aa2" , "ql" ,
                      "aa2" , "cdb"),
                    ncol = 2, 
                    byrow = TRUE,
                    dimnames = list(NULL, c("from", "to")))
  
  # gravar a white list em arquivo  
  nomeArquivoWhiteList = paste(NomeExperimento,"_WhiteList.xlsx",sep="",collapse="")
  if(!file.exists(nomeArquivoWhiteList)){
    write.xlsx(df_white,nomeArquivoWhiteList, row.names = FALSE)
  }
  
  df_black = matrix(vector(), 
                    nrow = nrow(matriz_arranjo) - nrow(df_white), 
                    ncol = 2,
                    dimnames = list(NULL, c("from", "to")))
  
  df_black <- geraBlackList(matriz_arranjo, df_white, df_black)
  
  # total de processamentos
  totalProcessamentos = lista_experimento$bateria * 
    length(lista_experimento$registros) * 
    length(lista_experimento$furos)
  
  # processamento de um arquivo de cada vez...
  for(i in 1:length(lista_experimento$registros)){
    for(j in 1:length(lista_experimento$furos)){
      
      linhaResultado = (vez-1) * length(lista_experimento$registros) * 
        length(lista_experimento$furos) + 
        (i-1)   * length(lista_experimento$furos) + j
      
      # carregando o controle do processamento
      lista_controle <- fromJSON(ArquivoControle)
      
      # verificando se já foi processado
      if(lista_controle$processados[linhaResultado]==1){
        cat ("Já Processado.: #", linhaResultado, 
             "/", totalProcessamentos,
             "- Bateria", vez,
             "- Arquivo com", lista_experimento$registros[i],
             "registros e",  lista_experimento$furos[j],
             "% de furos.\n")      
        
      } else {
        cat ("Processando...: #", linhaResultado, 
             "/", totalProcessamentos,
             "- Bateria", vez,
             "- Arquivo com", lista_experimento$registros[i],
             "registros e",  lista_experimento$furos[j],
             "% de furos.\n")      
        
        
        
        
        processaUmArquivoCenario5(vez, 
                                  linhaResultado,
                                  lista_experimento$registros[i],
                                  lista_experimento$furos[j],
                                  df_white,
                                  df_black) 
        # registrando o que foi processado
        lista_controle$processados[linhaResultado]=1
        write_json(lista_controle, ArquivoControle)
      }
    } # for j
  } # for i
}

##################################################################################
# FUNCTION : processaDadosCenario6
#
# processamento de cada arquivo para cada percentual de buracos para o cenario 6
#
##################################################################################

processaDadosCenario6 <- function(vez){
  
  ################################################################################
  # Criar uma Black List e uma White List
  ################################################################################
  #
  
  elemento <- c("ss1", "ss2", "somafusao", "acao", "a1", "a2")
  
  #  o código abaixo faz uma combinatória e não um arranjo
  #
  #  matriz_combinatoria = matrix(combn(elemento, 2), 
  #                               ncol = 2, 
  #                               byrow = TRUE,
  #                               dimnames = list(NULL, c("from", "to")))
  
  
  # a quantidade de elementos em um arranjo simples é n!/(n-p)!
  
  matriz_arranjo = matrix(permutations(n=length(elemento),r=2,v=elemento), 
                          ncol = 2, 
                          byrow = FALSE,
                          dimnames = list(NULL, c("from", "to")))
  
  df_white = matrix(c("ss1","somafusao",
                      "ss2","somafusao",
                      "somafusao", "acao",
                      "acao","a1",
                      "acao","a2"), 
                    ncol = 2, 
                    byrow = TRUE,
                    dimnames = list(NULL, c("from", "to")))
  
  # gravar a white list em arquivo  
  nomeArquivoWhiteList = paste(NomeExperimento,"_WhiteList.xlsx",sep="",collapse="")
  if(!file.exists(nomeArquivoWhiteList)){
    write.xlsx(df_white,nomeArquivoWhiteList, row.names = FALSE)
  }
  
  
  df_black = matrix(vector(), 
                    nrow = nrow(matriz_arranjo) - nrow(df_white), 
                    ncol = 2,
                    dimnames = list(NULL, c("from", "to")))
  
  df_black <- geraBlackList(matriz_arranjo, df_white, df_black)
  
  # total de processamentos
  totalProcessamentos = lista_experimento$bateria * 
    length(lista_experimento$registros) * 
    length(lista_experimento$furos)
  
  # processamento de um arquivo de cada vez...
  for(i in 1:length(lista_experimento$registros)){
    for(j in 1:length(lista_experimento$furos)){
      
      linhaResultado = (vez-1) * length(lista_experimento$registros) * 
        length(lista_experimento$furos) + 
        (i-1)   * length(lista_experimento$furos) + j
      
      # carregando o controle do processamento
      lista_controle <- fromJSON(ArquivoControle)
      
      # verificando se já foi processado
      if(lista_controle$processados[linhaResultado]==1){
        cat ("Já Processado.: #", linhaResultado, 
             "/", totalProcessamentos,
             "- Bateria", vez,
             "- Arquivo com", lista_experimento$registros[i],
             "registros e",  lista_experimento$furos[j],
             "% de furos.\n")      
        
      } else {
        cat ("Processando...: #", linhaResultado, 
             "/", totalProcessamentos,
             "- Bateria", vez,
             "- Arquivo com", lista_experimento$registros[i],
             "registros e",  lista_experimento$furos[j],
             "% de furos.\n")      
        
        processaUmArquivoCenario6(vez, 
                                  linhaResultado,
                                  lista_experimento$registros[i],
                                  lista_experimento$furos[j],
                                  df_white,
                                  df_black) 
        # registrando o que foi processado
        lista_controle$processados[linhaResultado]=1
        write_json(lista_controle, ArquivoControle)
      }
    } # for j
  } # for i
}


##################################################################################
# FUNCTION : processaDadosCenario7
#
# processamento de cada arquivo para cada percentual de buracos para o cenario 7
#
##################################################################################
processaDadosCenario7 <- function(vez){
  
  ################################################################################
  # Criar uma Black List e uma White List
  ################################################################################
  #
  elemento <- c("c","s1","s2","f1","r","a1")
  
  #  o código abaixo faz uma combinatória e não um arranjo
  #
  #  matriz_combinatoria = matrix(combn(elemento, 2), 
  #                               ncol = 2, 
  #                               byrow = TRUE,
  #                               dimnames = list(NULL, c("from", "to")))
  
  
  # a quantidade de elementos em um arranjo simples é n!/(n-p)!
  
  matriz_arranjo = matrix(permutations(n=length(elemento),r=2,v=elemento), 
                          ncol = 2, 
                          byrow = FALSE,
                          dimnames = list(NULL, c("from", "to")))
  
  df_white = matrix(c("c" ,"r" ,
                      "s1","f1",
                      "s2","f1",
                      "f1","r" ,
                      "r" ,"a1"), 
                    ncol = 2, 
                    byrow = TRUE,
                    dimnames = list(NULL, c("from", "to")))
  
  # gravar a white list em arquivo  
  nomeArquivoWhiteList = paste(NomeExperimento,"_WhiteList.xlsx",sep="",collapse="")
  if(!file.exists(nomeArquivoWhiteList)){
    write.xlsx(df_white,nomeArquivoWhiteList, row.names = FALSE)
  }
  
  
  df_black = matrix(vector(), 
                    nrow = nrow(matriz_arranjo) - nrow(df_white), 
                    ncol = 2,
                    dimnames = list(NULL, c("from", "to")))
  
  df_black <- geraBlackList(matriz_arranjo, df_white, df_black)
  
  # total de processamentos
  totalProcessamentos = lista_experimento$bateria * 
    length(lista_experimento$registros) * 
    length(lista_experimento$furos)
  
  # processamento de um arquivo de cada vez...
  for(i in 1:length(lista_experimento$registros)){
    for(j in 1:length(lista_experimento$furos)){
      
      linhaResultado = (vez-1) * length(lista_experimento$registros) * 
        length(lista_experimento$furos) + 
        (i-1)   * length(lista_experimento$furos) + j
      
      # carregando o controle do processamento
      lista_controle <- fromJSON(ArquivoControle)
      
      # verificando se já foi processado
      if(lista_controle$processados[linhaResultado]==1){
        cat ("Já Processado.: #", linhaResultado, 
             "/", totalProcessamentos,
             "- Bateria", vez,
             "- Arquivo com", lista_experimento$registros[i],
             "registros e",  lista_experimento$furos[j],
             "% de furos.\n")      
        
      } else {
        cat ("Processando...: #", linhaResultado, 
             "/", totalProcessamentos,
             "- Bateria", vez,
             "- Arquivo com", lista_experimento$registros[i],
             "registros e",  lista_experimento$furos[j],
             "% de furos.\n")      
        
        processaUmArquivoCenario7(vez, 
                                  linhaResultado,
                                  lista_experimento$registros[i],
                                  lista_experimento$furos[j],
                                  df_white,
                                  df_black) 
        # registrando o que foi processado
        lista_controle$processados[linhaResultado]=1
        write_json(lista_controle, ArquivoControle)
      }
    } # for j
  } # for i
  
}



##################################################################################
# FUNCTION : processaUmArquivoCenario1
#
# processamento de cada arquivo para cada percentual de buracos para o cenario 1
#
##################################################################################

processaUmArquivoCenario1 <- function (vez,
                                       linhaResultado,
                                       registros,
                                       furos,
                                       df_white,
                                       df_black) {
  
  ###################################################################################
  # carrega a base na variável bn_df como um dataframe
  ###################################################################################
  # 
  nomeArquivo = paste(NomeExperimento,"_",registros,".csv",sep="",collapse="")
  bn_df <- read.csv(nomeArquivo)  
  
  ###################################################################################
  # Converter as colunas para Factor para a rede bayesiana - função hc()
  ###################################################################################
  #
  bn_df$ss1       = as.factor(bn_df$ss1)
  bn_df$ss2       = as.factor(bn_df$ss2)
  bn_df$ss3       = as.factor(bn_df$ss3)
  bn_df$ss4       = as.factor(bn_df$ss4)
  bn_df$somafusao = as.factor(bn_df$somafusao)
  bn_df$acao      = as.factor(bn_df$acao)
  bn_df$a1        = as.factor(bn_df$a1)
  bn_df$a2        = as.factor(bn_df$a2)
  bn_df$a3        = as.factor(bn_df$a3)
  bn_df$a4        = as.factor(bn_df$a4)
  
  ###################################################################################
  # dividir a amostra em duas partes: treino e teste
  ###################################################################################
  #
  divisao = sample.split(bn_df$ss1,SplitRatio = 0.5)
  
  
  base_treinamento      = subset(bn_df, divisao == TRUE)
  base_teste            = subset(bn_df, divisao == FALSE)
  base_teste_sem_buraco = subset(bn_df, divisao == FALSE)
  
  ###################################################################################
  # aplica a função hc na variável bn_df e guarda o resultado na variável res
  # Sobre a função: Agrupamento hierárquico aglomerativo baseado em critérios de 
  # máxima verossimilhança para modelos de mistura gaussiana parametrizados por 
  # decomposição de autovalores.
  ###################################################################################
  # O que eu entendi: está criando a RB com o algoritmo "max-min hill climbing", 
  # que é um algoritmo guloso.
  ###################################################################################
  #
  res = hc(base_treinamento, whitelist = df_white, blacklist = df_black)
  
  ###################################################################################
  # Grava o Plot das dependências funcionais entre as variáveis
  ###################################################################################
  #
  nomeArquivoImagem = paste(NomeExperimento,"_imagem.png",sep="",collapse="")
  if(!file.exists(nomeArquivoImagem)){
    png(file = nomeArquivoImagem)
    plot(res)
    dev.off()
  }
  
  
  ###################################################################################
  # treinar a rede
  # Depois de aprender a estrutura, é preciso descobrir as tabelas de probabilidade
  # condicional (CPTs) em cada nó. A função bn.fit executa o algoritmo EM para 
  # aprender CPT para diferentes nós no gráfico acima.
  ###################################################################################
  #
  #cat("treinar a rede\n")
  fittedbn <- bn.fit(res, data = base_treinamento)
  
  ###################################################################################
  # Explorando um dos cálculos
  # Por exemplo, vamos ver o que está dentro do nó s1
  ###################################################################################
  #
  #cat("explorando um dos cálculos\n")
  # fittedbn é um objeto da classe list
  #print(fittedbn)
  # ss1 
  #print(fittedbn$ss1)
  
  
  ###################################################################################
  # insere nulos
  ###################################################################################
  #
  porcentagem = furos/100
  nlin <- nrow(base_teste)
  ncol <- ncol(base_teste)
  totalelementos <- nlin * ncol
  porc_aleatorio <- totalelementos * porcentagem
  
  if(porc_aleatorio<1){
    porc_aleatorio=1
  }
  
  aleatorios <- sort(sample(1:totalelementos, porc_aleatorio))
  
  for (i in 1:porc_aleatorio){
    substituir <- aleatorios[i]
    linsub     <- (substituir %/% ncol)
    colsub     <- (substituir %% ncol)
    if(colsub==0){
      colsub <- ncol
    }
    base_teste[linsub,colsub] <- 'NULL'
  }
  
  #cat("Inseriu nulos\n")  
  ###################################################################################
  # Verifica se deu linha 100% nula
  ###################################################################################
  #
  contaNulo <- 0
  for (i in 1:nlin){
    for(j in 1:ncol){
      if (is.na  (base_teste[i,j]) || 
          is.null(base_teste[i,j]) ||
          (base_teste[i,j]=="NULL")){
        contaNulo = contaNulo + 1
      }
    }
    if(contaNulo==ncol){
      contaNulo <- 0 # 24/07/2020 
      print("tudo nulo, não da pra fazer a predição")
    #stop("tudo nulo, não da pra fazer a predição")
	}
    else
      contaNulo <- 0
  }

  #cat("Verificou linhas todas de buracos")
  ###################################################################################
  # Conta total de buracos na base
  # Conta total de linhas sem buracos
  # Conta total de linhas com buracos
  ###################################################################################
  #
  contadorDeBuracos    <- 0
  qtdlinhasSemBuracos  <- 0
  qtdlinhasComBuracos  <- 0
  
  for (i in 1:nlin){
    contAux=0
    for(j in 1:ncol){
      if (is.na  (base_teste[i,j]) || 
          is.null(base_teste[i,j]) ||
          (base_teste[i,j]=="NULL")){
        contAux=contAux+1
        contadorDeBuracos = contadorDeBuracos + 1
      }
    }
    if(contAux==0)
      qtdlinhasSemBuracos = qtdlinhasSemBuracos + 1
    else
      qtdlinhasComBuracos = qtdlinhasComBuracos + 1
  }
  
  #cat("Contou linhas e buracos\n")  
  
  ###################################################################################
  # informa valores para as colunas e suas categorias
  # O ideal seria vir de algum arquivo...
  ###################################################################################
  #
  valores <- list(c('0', '1', 'D'),
                  c('0', '1', 'D'),
                  c('0', '1', 'D'),
                  c('0', '1', 'D'),
                  c('0','1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12', '13', '14', '15'),
                  c('0000', '0001', '0010', '0011', '0100', '0101', '0110', '0111', '1000', '1001', '1010', '1011', '1100', '1101', '1110', '1111'),
                  c('0', '1', 'D'),
                  c('0', '1', 'D'),
                  c('0', '1', 'D'),
                  c('0', '1', 'D'))
  
  categorias <- list(c('ss1','ss2','ss3','ss4'),
                     c('somafusao'),
                     c('acao'),
                     c('a1','a2','a3','a4'))
  
  ###################################################################################
  # inicialização dos contadores gerais do processamento
  ###################################################################################
  #
  contadorDeBuracosErradosPrimeira  <- 0
  contadorDeBuracosErradosSegunda   <- 0
  contadorDeBuracosErradosTerceira  <- 0
  qtdAcertouTudoPrimeira            <- 0
  qtdAcertouTudoSegunda             <- 0
  qtdAcertouTudoTerceira            <- 0
  
  contadorAcertouAtuador1Primeira   <- 0
  contadorAcertouAtuador1Segunda    <- 0
  contadorAcertouAtuador1Terceira   <- 0
  contadorAcertouAtuador2Primeira   <- 0
  contadorAcertouAtuador2Segunda    <- 0
  contadorAcertouAtuador2Terceira   <- 0
  contadorAcertouAtuador3Primeira   <- 0
  contadorAcertouAtuador3Segunda    <- 0
  contadorAcertouAtuador3Terceira   <- 0
  contadorAcertouAtuador4Primeira   <- 0
  contadorAcertouAtuador4Segunda    <- 0
  contadorAcertouAtuador4Terceira   <- 0
  contadorAcertouAcaoPrimeira       <- 0
  contadorAcertouAcaoSegunda        <- 0
  contadorAcertouAcaoTerceira       <- 0
  contadorAcertouSomafusaoPrimeira  <- 0
  contadorAcertouSomafusaoSegunda   <- 0
  contadorAcertouSomafusaoTerceira  <- 0
  contadorAcertouSensor1Primeira    <- 0
  contadorAcertouSensor1Segunda     <- 0
  contadorAcertouSensor1Terceira    <- 0
  contadorAcertouSensor2Primeira    <- 0
  contadorAcertouSensor2Segunda     <- 0
  contadorAcertouSensor2Terceira    <- 0
  contadorAcertouSensor3Primeira    <- 0
  contadorAcertouSensor3Segunda     <- 0
  contadorAcertouSensor3Terceira    <- 0
  contadorAcertouSensor4Primeira    <- 0
  contadorAcertouSensor4Segunda     <- 0
  contadorAcertouSensor4Terceira    <- 0
  
  contadorErrouAtuador1Primeira     <- 0
  contadorErrouAtuador1Segunda      <- 0
  contadorErrouAtuador1Terceira     <- 0
  contadorErrouAtuador2Primeira     <- 0
  contadorErrouAtuador2Segunda      <- 0
  contadorErrouAtuador2Terceira     <- 0
  contadorErrouAtuador3Primeira     <- 0
  contadorErrouAtuador3Segunda      <- 0
  contadorErrouAtuador3Terceira     <- 0
  contadorErrouAtuador4Primeira     <- 0
  contadorErrouAtuador4Segunda      <- 0
  contadorErrouAtuador4Terceira     <- 0
  contadorErrouAcaoPrimeira         <- 0
  contadorErrouAcaoSegunda          <- 0
  contadorErrouAcaoTerceira         <- 0
  contadorErrouSomafusaoPrimeira    <- 0
  contadorErrouSomafusaoSegunda     <- 0
  contadorErrouSomafusaoTerceira    <- 0
  contadorErrouSensor1Primeira      <- 0
  contadorErrouSensor1Segunda       <- 0
  contadorErrouSensor1Terceira      <- 0
  contadorErrouSensor2Primeira      <- 0
  contadorErrouSensor2Segunda       <- 0
  contadorErrouSensor2Terceira      <- 0
  contadorErrouSensor3Primeira      <- 0
  contadorErrouSensor3Segunda       <- 0
  contadorErrouSensor3Terceira      <- 0
  contadorErrouSensor4Primeira      <- 0
  contadorErrouSensor4Segunda       <- 0
  contadorErrouSensor4Terceira      <- 0
  
  ###################################################################################
  # Laço principal para todas as instâncias
  # do arquivo
  ###################################################################################
  #
  for(i1 in 1:nlin){
    #cat("***Laco Principal - processando linha ", i1, "\n")
    dado <- base_teste[i1,]
    
    #####################################################
    # Verificando se a linha tem buracos
    # se não tiver então pular para a próxima linha
    #####################################################
    qtdBuracosLinha = 0
    
    for (i2 in 1:length(dado)){
      if (is.na(dado[i2]) ||
          is.null(dado[i2]) ||
          (dado[i2]=="NULL"))
        qtdBuracosLinha = qtdBuracosLinha + 1        
    }
    
    
    if(qtdBuracosLinha==0 || 
       qtdBuracosLinha==length(dado) ||
       qtdBuracosLinha/length(dado) > 0.75){ # 24/07/2020 - se não tem buraco ou só tem buraco pular
      #não precisa fazer queries ... então ir para o próximo
      #print ("Linha sem buracos... pulada")
      next
    }
    
    ##########################################
    # monta as queries
    ##########################################
    
    # limpando as variáveis
    dfv1 <- NULL
    dfv2 <- NULL
    dadoCopia <- NULL
    
    # fazendo uma cópia do dado (dataframe)
    dadoCopia <- dado
    
    # guardando o nome das colunas do dado (dataframe)
    nomeColunas <- colnames(dado)
    
    flag <- TRUE
    for (i2 in length(dado):1)
      if (is.na(dado[i2]) ||
          is.null(dado[i2]) ||
          (dado[i2]=="NULL"))
      {
        dadoCopia <- dadoCopia[,-c(i2)]
        if(flag)
        {
          flag <- FALSE
          dfV1 <- data.frame(valores[i2])
          names(dfV1) <- nomeColunas[i2]
        } else {
          dfV2 <- data.frame(valores[i2])
          names(dfV2) <- nomeColunas[i2]
          dfV1 <- cartesianExpand(dfV1, dfV2)
        }
      }
    
    queries <- as.data.frame(cartesianExpand(dadoCopia, dfV1))
    
    # obs: valores nulos vão no evento e os não nulos nas evidencias
    strEventoColuna    <- NULL
    strEvidenciaColuna <- NULL
    strEventoValor     <- NULL
    strEvidenciaValor  <- NULL
    
    contadorNull = 0
    for (i3 in length(dado):1)
      if (is.na(dado[i3]) ||
          is.null(dado[i3]) ||
          (dado[i3]=="NULL")){
        contadorNull = contadorNull + 1
        strEventoColuna <- c(nomeColunas[i3],strEventoColuna)
      } else {
        strEvidenciaColuna <- c(nomeColunas[i3],strEvidenciaColuna)
        strEvidenciaValor <- c((as.character(dado[1,i3])),strEvidenciaValor)
      }
    
    # devo pegar as três maiores probabilidades
    maiorProbabilidade = -1
    segundaMaiorProbabilidade = -1
    terceiraMaiorProbabilidade = -1
    
    queryMaiorProbabilidade = NULL
    querySegundaMaiorProbabilidade = NULL
    queryTerceiraMaiorProbabilidade = NULL
    
    indiceMaior = -1
    indiceSegundoMaior = -1
    indiceTerceiroMaior = -1
    
    #cat("nrow(queries) ===> ", nrow(queries), "\n")
  
    # percorrendo cada linha das queries
    for(i4 in 1:nrow(queries)){
      for(i5 in (length(dado)-contadorNull+1):(length(dado))){
        strEventoValor <- c((as.character(queries[i4,i5])),strEventoValor)
      }
      
      str1 = paste("(", strEventoColuna   , " == \"",strEventoValor   , "\")", sep = "", collapse = " & ")
      str2 = paste("(", strEvidenciaColuna, " == \"",strEvidenciaValor, "\")", sep = "")
      
      cmd = paste("cpquery(fittedbn, event = ", str1, ", evidence = ", str2, ")", sep = "")
      probabilidade = eval(parse(text = cmd))
      
      #cat("probabilidade===>", probabilidade, "\n")
      
      
      #pegando as três maiores probabilidades
      if(probabilidade > maiorProbabilidade){
        # a segunda passa a ser a terceira
        terceiraMaiorProbabilidade = segundaMaiorProbabilidade
        queryTerceiraMaiorProbabilidade = querySegundaMaiorProbabilidade
        indiceTerceiroMaior = indiceSegundoMaior
        
        # a primeira passa a ser a segunda
        segundaMaiorProbabilidade = maiorProbabilidade
        querySegundaMaiorProbabilidade = queryMaiorProbabilidade
        indiceSegundoMaior = indiceMaior
        
        # a nova passa a ser a primeira
        maiorProbabilidade = probabilidade
        queryMaiorProbabilidade = queries[i4,]
        indiceMaior = i4
      } else {
        if(probabilidade > segundaMaiorProbabilidade){
          # a segunda passa a ser a terceira
          terceiraMaiorProbabilidade = segundaMaiorProbabilidade
          queryTerceiraMaiorProbabilidade = querySegundaMaiorProbabilidade
          indiceTerceiroMaior = indiceSegundoMaior
          
          # a nova passa a ser a segunda
          segundaMaiorProbabilidade = probabilidade
          querySegundaMaiorProbabilidade = queries[i4,]
          indiceSegundoMaior = i4
        } else {
          if(probabilidade > terceiraMaiorProbabilidade){
            # a nova passa a ser a terceira
            terceiraMaiorProbabilidade = probabilidade
            queryTerceiraMaiorProbabilidade = queries[i4,]
            indiceTerceiraMaior = i4
          }
        }
      }
      
      strEventoValor <- NULL
    } # for i4
    
    
    ###################################################################################
    #  
    #  __   
    # /_ |  
    #  | |              Analisando os acertos e erros de primeira
    #  | | 
    #  | |
    #  |_|
    #
    ###################################################################################
    if(maiorProbabilidade>-1){
      #########################################################
      #cat ("Maior Probabiliade = ", maiorProbabilidade,"\n")
      #print ("Linha com buracos  = ")
      #print (dado[,order(names(dado))])
      #########################################################    
      
      ## auxiliar para identificar os buracos originais
      aux1 <- dado[,order(names(dado))]
      aux2 <- colnames(aux1)
      
      #########################################################
      #print ("Query selecionada  = ")
      #print (queryMaiorProbabilidade[,order(names(queryMaiorProbabilidade))])
      #print ("Linha sem buracos  = ")
      #print (base_teste_sem_buraco[i1,order(names(bn_df))])
      #########################################################
      contadorDiferencasPrimeira = 0
      ContadorIgualdadesPrimeira = 0
      
      # AA guarda a query com os buracos preenchidos com os valores de maior probabilidade
      AA <- queryMaiorProbabilidade[,order(names(queryMaiorProbabilidade))]
      # BB guarda a linha com os valores originais para verificar se acertou ou não
      BB <- base_teste_sem_buraco[i1,order(names(bn_df))]
      
      for(ww in 1:length(dado)){
        if(as.character(AA[,ww]) == as.character(BB[,ww])){
          if(is.na(aux1[ww]) || is.null(aux1[ww]) || (aux1[ww]=="NULL")){
            ## acertou um buraco
            if (aux2[ww] == "a1"){contadorAcertouAtuador1Primeira = contadorAcertouAtuador1Primeira + 1} 
            else
              if (aux2[ww] == "a2"){contadorAcertouAtuador2Primeira = contadorAcertouAtuador2Primeira + 1}
            else
              if (aux2[ww] == "a3"){contadorAcertouAtuador3Primeira = contadorAcertouAtuador3Primeira + 1}  
            else
              if (aux2[ww] == "a4"){contadorAcertouAtuador4Primeira = contadorAcertouAtuador4Primeira + 1}
            else
              if(aux2[ww] == "acao"){contadorAcertouAcaoPrimeira = contadorAcertouAcaoPrimeira + 1}
            else 
              if(aux2[ww] == "somafusao"){contadorAcertouSomafusaoPrimeira = contadorAcertouSomafusaoPrimeira + 1}
            else 
              if(aux2[ww] == "ss1"){contadorAcertouSensor1Primeira = contadorAcertouSensor1Primeira + 1}
            else
              if(aux2[ww] == "ss2"){contadorAcertouSensor2Primeira = contadorAcertouSensor2Primeira + 1}
            else
              if(aux2[ww] == "ss3"){contadorAcertouSensor3Primeira = contadorAcertouSensor3Primeira + 1}
            else
              if(aux2[ww] == "ss4"){contadorAcertouSensor4Primeira = contadorAcertouSensor4Primeira + 1}
          }
          
          ContadorIgualdadesPrimeira <- ContadorIgualdadesPrimeira + 1
        } else {
          if(is.na(aux1[ww]) || is.null(aux1[ww]) || (aux1[ww]=="NULL")){
            ## errou um buraco
            if (aux2[ww] == "a1"){contadorErrouAtuador1Primeira = contadorErrouAtuador1Primeira + 1} 
            else
              if (aux2[ww] == "a2"){contadorErrouAtuador2Primeira = contadorErrouAtuador2Primeira + 1}
            else
              if (aux2[ww] == "a3"){contadorErrouAtuador3Primeira = contadorErrouAtuador3Primeira + 1}  
            else
              if (aux2[ww] == "a4"){contadorErrouAtuador4Primeira = contadorErrouAtuador4Primeira + 1}
            else
              if(aux2[ww] == "acao"){contadorErrouAcaoPrimeira = contadorErrouAcaoPrimeira + 1}
            else 
              if(aux2[ww] == "somafusao"){contadorErrouSomafusaoPrimeira = contadorErrouSomafusaoPrimeira + 1}
            else 
              if(aux2[ww] == "ss1"){contadorErrouSensor1Primeira = contadorErrouSensor1Primeira + 1}
            else
              if(aux2[ww] == "ss2"){contadorErrouSensor2Primeira = contadorErrouSensor2Primeira + 1}
            else
              if(aux2[ww] == "ss3"){contadorErrouSensor3Primeira = contadorErrouSensor3Primeira + 1}
            else
              if(aux2[ww] == "ss4"){contadorErrouSensor4Primeira = contadorErrouSensor4Primeira + 1}
          }
          contadorDiferencasPrimeira <- contadorDiferencasPrimeira + 1
          contadorDeBuracosErradosPrimeira = contadorDeBuracosErradosPrimeira + 1
          #########################################################        
          #cat("Q: ",colnames(AA[ww]),"...:",as.character(AA[,ww]),"\t",
          #    "B: ",colnames(BB[ww]),"...:",as.character(BB[,ww]),"\n")
          #########################################################        
        }
      } # for ww
      
      #########################################################    
      #cat ("Qtde de Buracos certos  Primeira  = ", (qtdBuracosLinha-contadorDiferencasPrimeira), "\n")
      #cat ("Qtde de Buracos errados Primeira  = ", contadorDiferencasPrimeira, "\n")
      #cat ("Qtde de Buracos                   = ", qtdBuracosLinha, "\n")
      #########################################################    
      
      if(contadorDiferencasPrimeira==0)
        qtdAcertouTudoPrimeira = qtdAcertouTudoPrimeira + 1    
      
    } # if(maiorProbabilidade>-1)
    
    
    
    ###################################################################################
    #  
    #  ___  
    # |__ \
    #    ) |            Analisando os acertos e erros de Segunda
    #   / / 
    #  / /_ 
    # |____|
    #
    ###################################################################################
    if(segundaMaiorProbabilidade>-1){
      #########################################################
      #cat ("Segunda Maior Probabiliade = ", segundaMaiorProbabilidade,"\n")
      #print ("Linha com buracos  = ")
      #print (dado[,order(names(dado))])
      #########################################################    
      
      ## auxiliar para identificar os buracos originais
      aux1 <- dado[,order(names(dado))]
      aux2 <- colnames(aux1)
      
      #########################################################
      #print ("Query selecionada  = ")
      #print (querySegundaMaiorProbabilidade[,order(names(querySegundaMaiorProbabilidade))])
      #print ("Linha sem buracos  = ")
      #print (base_teste_sem_buraco[i1,order(names(bn_df))])
      #########################################################
      contadorDiferencasSegunda = 0
      ContadorIgualdadesSegunda = 0
      
      # AA guarda a query com os buracos preenchidos com os valores de maior probabilidade
      AA <- querySegundaMaiorProbabilidade[,order(names(querySegundaMaiorProbabilidade))]
      # BB guarda a linha com os valores originais para verificar se acertou ou não
      BB <- base_teste_sem_buraco[i1,order(names(bn_df))]
      
      for(ww in 1:length(dado)){
        if(as.character(AA[,ww]) == as.character(BB[,ww])){
          if(is.na(aux1[ww]) || is.null(aux1[ww]) || (aux1[ww]=="NULL")){
            ## acertou um buraco
            if (aux2[ww] == "a1"){contadorAcertouAtuador1Segunda = contadorAcertouAtuador1Segunda + 1} 
            else
              if (aux2[ww] == "a2"){contadorAcertouAtuador2Segunda = contadorAcertouAtuador2Segunda + 1}
            else
              if (aux2[ww] == "a3"){contadorAcertouAtuador3Segunda = contadorAcertouAtuador3Segunda + 1}  
            else
              if (aux2[ww] == "a4"){contadorAcertouAtuador4Segunda = contadorAcertouAtuador4Segunda + 1}
            else
              if(aux2[ww] == "acao"){contadorAcertouAcaoSegunda = contadorAcertouAcaoSegunda + 1}
            else 
              if(aux2[ww] == "somafusao"){contadorAcertouSomafusaoSegunda = contadorAcertouSomafusaoSegunda + 1}
            else 
              if(aux2[ww] == "ss1"){contadorAcertouSensor1Segunda = contadorAcertouSensor1Segunda + 1}
            else
              if(aux2[ww] == "ss2"){contadorAcertouSensor2Segunda = contadorAcertouSensor2Segunda + 1}
            else
              if(aux2[ww] == "ss3"){contadorAcertouSensor3Segunda = contadorAcertouSensor3Segunda + 1}
            else
              if(aux2[ww] == "ss4"){contadorAcertouSensor4Segunda = contadorAcertouSensor4Segunda + 1}
          }
          
          ContadorIgualdadesSegunda <- ContadorIgualdadesSegunda + 1
        } else {
          if(is.na(aux1[ww]) || is.null(aux1[ww]) || (aux1[ww]=="NULL")){
            ## errou um buraco
            if (aux2[ww] == "a1"){contadorErrouAtuador1Segunda = contadorErrouAtuador1Segunda + 1} 
            else
              if (aux2[ww] == "a2"){contadorErrouAtuador2Segunda = contadorErrouAtuador2Segunda + 1}
            else
              if (aux2[ww] == "a3"){contadorErrouAtuador3Segunda = contadorErrouAtuador3Segunda + 1}  
            else
              if (aux2[ww] == "a4"){contadorErrouAtuador4Segunda = contadorErrouAtuador4Segunda + 1}
            else
              if(aux2[ww] == "acao"){contadorErrouAcaoSegunda = contadorErrouAcaoSegunda + 1}
            else 
              if(aux2[ww] == "somafusao"){contadorErrouSomafusaoSegunda = contadorErrouSomafusaoSegunda + 1}
            else 
              if(aux2[ww] == "ss1"){contadorErrouSensor1Segunda = contadorErrouSensor1Segunda + 1}
            else
              if(aux2[ww] == "ss2"){contadorErrouSensor2Segunda = contadorErrouSensor2Segunda + 1}
            else
              if(aux2[ww] == "ss3"){contadorErrouSensor3Segunda = contadorErrouSensor3Segunda + 1}
            else
              if(aux2[ww] == "ss4"){contadorErrouSensor4Segunda = contadorErrouSensor4Segunda + 1}
          }
          contadorDiferencasSegunda <- contadorDiferencasSegunda + 1
          contadorDeBuracosErradosSegunda = contadorDeBuracosErradosSegunda + 1
          #########################################################        
          #cat("Q: ",colnames(AA[ww]),"...:",as.character(AA[,ww]),"\t",
          #    "B: ",colnames(BB[ww]),"...:",as.character(BB[,ww]),"\n")
          #########################################################        
        }
      } # for ww
      
      
      #########################################################    
      #cat ("Qtde de Buracos certos  Segunda = ", (qtdBuracosLinha-contadorDiferencasSegunda), "\n")
      #cat ("Qtde de Buracos errados Segunda = ", contadorDiferencasSegunda, "\n")
      #cat ("Qtde de Buracos                 = ", qtdBuracosLinha, "\n")
      #########################################################    
      
      if(contadorDiferencasSegunda==0)
        qtdAcertouTudoSegunda = qtdAcertouTudoSegunda + 1    
      
    } # if(segundaMaiorProbabilidade>-1)
    
    ###################################################################################
    #  
    #  ____  
    # |___ \   
    #   __) |           Analisando os acertos e erros de Terceira
    #  |__ < 
    #  ___) |
    # |____/
    #
    ###################################################################################
    if(terceiraMaiorProbabilidade>-1){
      #########################################################
      #cat ("Terceira Maior Probabiliade = ", terceiraMaiorProbabilidade,"\n")
      #print ("Linha com buracos  = ")
      #print (dado[,order(names(dado))])
      #########################################################    
      
      ## auxiliar para identificar os buracos originais
      aux1 <- dado[,order(names(dado))]
      aux2 <- colnames(aux1)
      
      #########################################################
      #print ("Query selecionada  = ")
      #print (queryTerceiraMaiorProbabilidade[,order(names(queryTerceiraMaiorProbabilidade))])
      #print ("Linha sem buracos  = ")
      #print (base_teste_sem_buraco[i1,order(names(bn_df))])
      #########################################################
      contadorDiferencasTerceira = 0
      ContadorIgualdadesTerceira = 0
      
      # AA guarda a query com os buracos preenchidos com os valores de maior probabilidade
      AA <- queryTerceiraMaiorProbabilidade[,order(names(queryTerceiraMaiorProbabilidade))]
      # BB guarda a linha com os valores originais para verificar se acertou ou não
      BB <- base_teste_sem_buraco[i1,order(names(bn_df))]
      
      for(ww in 1:length(dado)){
        if(as.character(AA[,ww]) == as.character(BB[,ww])){
          if(is.na(aux1[ww]) || is.null(aux1[ww]) || (aux1[ww]=="NULL")){
            ## acertou um buraco
            if (aux2[ww] == "a1"){contadorAcertouAtuador1Terceira = contadorAcertouAtuador1Terceira + 1} 
            else
              if (aux2[ww] == "a2"){contadorAcertouAtuador2Terceira = contadorAcertouAtuador2Terceira + 1}
            else
              if (aux2[ww] == "a3"){contadorAcertouAtuador3Terceira = contadorAcertouAtuador3Terceira + 1}  
            else
              if (aux2[ww] == "a4"){contadorAcertouAtuador4Terceira = contadorAcertouAtuador4Terceira + 1}
            else
              if(aux2[ww] == "acao"){contadorAcertouAcaoTerceira = contadorAcertouAcaoTerceira + 1}
            else 
              if(aux2[ww] == "somafusao"){contadorAcertouSomafusaoTerceira = contadorAcertouSomafusaoTerceira + 1}
            else 
              if(aux2[ww] == "ss1"){contadorAcertouSensor1Terceira = contadorAcertouSensor1Terceira + 1}
            else
              if(aux2[ww] == "ss2"){contadorAcertouSensor2Terceira = contadorAcertouSensor2Terceira + 1}
            else
              if(aux2[ww] == "ss3"){contadorAcertouSensor3Terceira = contadorAcertouSensor3Terceira + 1}
            else
              if(aux2[ww] == "ss4"){contadorAcertouSensor4Terceira = contadorAcertouSensor4Terceira + 1}
          }
          
          ContadorIgualdadesTerceira <- ContadorIgualdadesTerceira + 1
        } else {
          if(is.na(aux1[ww]) || is.null(aux1[ww]) || (aux1[ww]=="NULL")){
            ## errou um buraco
            if (aux2[ww] == "a1"){contadorErrouAtuador1Terceira = contadorErrouAtuador1Terceira + 1} 
            else
              if (aux2[ww] == "a2"){contadorErrouAtuador2Terceira = contadorErrouAtuador2Terceira + 1}
            else
              if (aux2[ww] == "a3"){contadorErrouAtuador3Terceira = contadorErrouAtuador3Terceira + 1}  
            else
              if (aux2[ww] == "a4"){contadorErrouAtuador4Terceira = contadorErrouAtuador4Terceira + 1}
            else
              if(aux2[ww] == "acao"){contadorErrouAcaoTerceira = contadorErrouAcaoTerceira + 1}
            else 
              if(aux2[ww] == "somafusao"){contadorErrouSomafusaoTerceira = contadorErrouSomafusaoTerceira + 1}
            else 
              if(aux2[ww] == "ss1"){contadorErrouSensor1Terceira = contadorErrouSensor1Terceira + 1}
            else
              if(aux2[ww] == "ss2"){contadorErrouSensor2Terceira = contadorErrouSensor2Terceira + 1}
            else
              if(aux2[ww] == "ss3"){contadorErrouSensor3Terceira = contadorErrouSensor3Terceira + 1}
            else
              if(aux2[ww] == "ss4"){contadorErrouSensor4Terceira = contadorErrouSensor4Terceira + 1}
          }
          contadorDiferencasTerceira <- contadorDiferencasTerceira + 1
          contadorDeBuracosErradosTerceira = contadorDeBuracosErradosTerceira + 1
          #########################################################        
          #cat("Q: ",colnames(AA[ww]),"...:",as.character(AA[,ww]),"\t",
          #    "B: ",colnames(BB[ww]),"...:",as.character(BB[,ww]),"\n")
          #########################################################        
        }
      } # for ww
      
      
      #########################################################    
      #cat ("Qtde de Buracos certos  Terceira = ", (qtdBuracosLinha-contadorDiferencasTerceira), "\n")
      #cat ("Qtde de Buracos errados Terceira = ", contadorDiferencasTerceira, "\n")
      #cat ("Qtde de Buracos                  = ", qtdBuracosLinha, "\n")
      #########################################################    
      
      if(contadorDiferencasTerceira==0)
        qtdAcertouTudoTerceira = qtdAcertouTudoTerceira + 1    
      
    } # if(terceiraMaiorProbabilidade>-1)
    
  } #  i1
  ##########################################
  # fim do Laço principal
  ##########################################
  
  contadorAcertouSensorPrimeira <- contadorAcertouSensor1Primeira +
                                   contadorAcertouSensor2Primeira + 
                                   contadorAcertouSensor3Primeira + 
                                   contadorAcertouSensor4Primeira
  
  contadorAcertouSensorSegunda <- contadorAcertouSensor1Segunda +
                                  contadorAcertouSensor2Segunda + 
                                  contadorAcertouSensor3Segunda + 
                                  contadorAcertouSensor4Segunda
  
  contadorAcertouSensorTerceira <- contadorAcertouSensor1Terceira +
                                   contadorAcertouSensor2Terceira + 
                                   contadorAcertouSensor3Terceira + 
                                   contadorAcertouSensor4Terceira
  
  
  contadorErrouSensorPrimeira   <- contadorErrouSensor1Primeira   +
                                   contadorErrouSensor2Primeira   +
                                   contadorErrouSensor3Primeira   +
                                   contadorErrouSensor4Primeira

  contadorErrouSensorSegunda   <-  contadorErrouSensor1Segunda   +
                                   contadorErrouSensor2Segunda   +
                                   contadorErrouSensor3Segunda   +
                                   contadorErrouSensor4Segunda
  
  contadorErrouSensorTerceira   <- contadorErrouSensor1Terceira   +
                                   contadorErrouSensor2Terceira   +
                                   contadorErrouSensor3Terceira   +
                                   contadorErrouSensor4Terceira
  
  
  contadorAcertouAtuadorPrimeira <- contadorAcertouAtuador1Primeira +
                                    contadorAcertouAtuador2Primeira + 
                                    contadorAcertouAtuador3Primeira + 
                                    contadorAcertouAtuador4Primeira

  contadorAcertouAtuadorSegunda <-  contadorAcertouAtuador1Segunda +
                                    contadorAcertouAtuador2Segunda + 
                                    contadorAcertouAtuador3Segunda + 
                                    contadorAcertouAtuador4Segunda
  
  contadorAcertouAtuadorTerceira <- contadorAcertouAtuador1Terceira +
                                    contadorAcertouAtuador2Terceira + 
                                    contadorAcertouAtuador3Terceira + 
                                    contadorAcertouAtuador4Terceira
  
  contadorErrouAtuadorPrimeira   <- contadorErrouAtuador1Primeira   +
                                    contadorErrouAtuador2Primeira   +
                                    contadorErrouAtuador3Primeira   +
                                    contadorErrouAtuador4Primeira
  
  contadorErrouAtuadorSegunda   <- contadorErrouAtuador1Segunda   +
                                   contadorErrouAtuador2Segunda   +
                                   contadorErrouAtuador3Segunda   +
                                   contadorErrouAtuador4Segunda 
  
  contadorErrouAtuadorTerceira  <- contadorErrouAtuador1Terceira   +
                                   contadorErrouAtuador2Terceira   +
                                   contadorErrouAtuador3Terceira   +
                                   contadorErrouAtuador4Terceira 
  
  # atualizando o dataframe com base no ArquivoResultado
  df_resultado <- as.data.frame(fromJSON(ArquivoResultado))
  
  #variaveis para guardar no dataframe de resultado de processamento
  df_resultado$totBuracos[linhaResultado] = contadorDeBuracos
  
  df_resultado$totBuracosAcertouPrimeira[linhaResultado] = (contadorDeBuracos-contadorDeBuracosErradosPrimeira)
  df_resultado$totBuracosAcertouSegunda[linhaResultado] = (contadorDeBuracos-contadorDeBuracosErradosSegunda)
  df_resultado$totBuracosAcertouTerceira[linhaResultado] = (contadorDeBuracos-contadorDeBuracosErradosTerceira)
  
  df_resultado$totBuracosErrouPrimeira[linhaResultado] = contadorDeBuracosErradosPrimeira
  df_resultado$totBuracosErrouSegunda[linhaResultado] = contadorDeBuracosErradosSegunda
  df_resultado$totBuracosErrouTerceira[linhaResultado] = contadorDeBuracosErradosTerceira
  
  df_resultado$predicoes100CorretasPrimeira[linhaResultado] = qtdAcertouTudoPrimeira
  df_resultado$predicoes100CorretasSegunda[linhaResultado] = qtdAcertouTudoSegunda
  df_resultado$predicoes100CorretasTerceira[linhaResultado] = qtdAcertouTudoTerceira
  
  # acho melhor pegar a Primeira
  df_resultado$totBuracosSensores[linhaResultado] = contadorAcertouSensorPrimeira+contadorErrouSensorPrimeira
  
  df_resultado$acertosSensor1Primeira[linhaResultado] = contadorAcertouSensor1Primeira
  df_resultado$acertosSensor1Segunda[linhaResultado] = contadorAcertouSensor1Segunda
  df_resultado$acertosSensor1Terceira[linhaResultado] = contadorAcertouSensor1Terceira
  
  df_resultado$acertosSensor2Primeira[linhaResultado] = contadorAcertouSensor2Primeira
  df_resultado$acertosSensor2Segunda[linhaResultado] = contadorAcertouSensor2Segunda
  df_resultado$acertosSensor2Terceira[linhaResultado] = contadorAcertouSensor2Terceira
  
  df_resultado$acertosSensor3Primeira[linhaResultado] = contadorAcertouSensor3Primeira
  df_resultado$acertosSensor3Segunda[linhaResultado] = contadorAcertouSensor3Segunda
  df_resultado$acertosSensor3Terceira[linhaResultado] = contadorAcertouSensor3Terceira
  
  df_resultado$acertosSensor4Primeira[linhaResultado] = contadorAcertouSensor4Primeira
  df_resultado$acertosSensor4Segunda[linhaResultado] = contadorAcertouSensor4Segunda
  df_resultado$acertosSensor4Terceira[linhaResultado] = contadorAcertouSensor4Terceira
  
  df_resultado$errosSensor1Primeira[linhaResultado] = contadorErrouSensor1Primeira
  df_resultado$errosSensor1Segunda[linhaResultado] = contadorErrouSensor1Segunda
  df_resultado$errosSensor1Terceira[linhaResultado] = contadorErrouSensor1Terceira
  
  df_resultado$errosSensor2Primeira[linhaResultado] = contadorErrouSensor2Primeira
  df_resultado$errosSensor2Segunda[linhaResultado] = contadorErrouSensor2Segunda
  df_resultado$errosSensor2Terceira[linhaResultado] = contadorErrouSensor2Terceira
  
  df_resultado$errosSensor3Primeira[linhaResultado] = contadorErrouSensor3Primeira
  df_resultado$errosSensor3Segunda[linhaResultado] = contadorErrouSensor3Segunda
  df_resultado$errosSensor3Terceira[linhaResultado] = contadorErrouSensor3Terceira
  
  df_resultado$errosSensor4Primeira[linhaResultado] = contadorErrouSensor4Primeira
  df_resultado$errosSensor4Segunda[linhaResultado] = contadorErrouSensor4Segunda
  df_resultado$errosSensor4Terceira[linhaResultado] = contadorErrouSensor4Terceira
  
  # o calculo abaixo pode ter denominador zero e gerar NaN - colocar em um if
  if((contadorAcertouSensorPrimeira + contadorErrouSensorPrimeira)>0){
    df_resultado$porcAcertoSensorPrimeira[linhaResultado] = (contadorAcertouSensorPrimeira/(contadorAcertouSensorPrimeira + contadorErrouSensorPrimeira))*100.0    
  } else {
    df_resultado$porcAcertoSensorPrimeira[linhaResultado] = 0
  }
  
  if((contadorAcertouSensorSegunda + contadorErrouSensorSegunda)>0){
    df_resultado$porcAcertoSensorSegunda[linhaResultado] = (contadorAcertouSensorSegunda/(contadorAcertouSensorSegunda + contadorErrouSensorSegunda))*100.0    
  } else {
    df_resultado$porcAcertoSensorSegunda[linhaResultado] = 0
  }
  
  if((contadorAcertouSensorTerceira + contadorErrouSensorTerceira)>0){
    df_resultado$porcAcertoSensorTerceira[linhaResultado] = (contadorAcertouSensorTerceira/(contadorAcertouSensorTerceira + contadorErrouSensorTerceira))*100.0    
  } else {
    df_resultado$porcAcertoSensorTerceira[linhaResultado] = 0
  }
  
  # acho melhor pegar a Primeira
  df_resultado$totBuracosAcao[linhaResultado] = contadorAcertouAcaoPrimeira + contadorErrouAcaoPrimeira
  
  df_resultado$acertosAcaoPrimeira[linhaResultado] = contadorAcertouAcaoPrimeira
  df_resultado$acertosAcaoSegunda[linhaResultado] = contadorAcertouAcaoSegunda
  df_resultado$acertosAcaoTerceira[linhaResultado] = contadorAcertouAcaoTerceira
  
  df_resultado$errosAcaoPrimeira[linhaResultado] = contadorErrouAcaoPrimeira
  df_resultado$errosAcaoSegunda[linhaResultado] = contadorErrouAcaoSegunda
  df_resultado$errosAcaoTerceira[linhaResultado] = contadorErrouAcaoTerceira
  
  # o calculo abaixo pode ter denominador zero e gerar NaN - colocar em um if
  if((contadorAcertouAcaoPrimeira + contadorErrouAcaoPrimeira)>0){
    df_resultado$porcAcertoAcaoPrimeira[linhaResultado] = (contadorAcertouAcaoPrimeira/(contadorAcertouAcaoPrimeira+contadorErrouAcaoPrimeira))*100.0    
  } else {
    df_resultado$porcAcertoAcaoPrimeira[linhaResultado] = 0    
  }
  
  if((contadorAcertouAcaoSegunda + contadorErrouAcaoSegunda)>0){
    df_resultado$porcAcertoAcaoSegunda[linhaResultado] = (contadorAcertouAcaoSegunda/(contadorAcertouAcaoSegunda+contadorErrouAcaoSegunda))*100.0    
  } else {
    df_resultado$porcAcertoAcaoSegunda[linhaResultado] = 0    
  }
  
  if((contadorAcertouAcaoTerceira + contadorErrouAcaoTerceira)>0){
    df_resultado$porcAcertoAcaoTerceira[linhaResultado] = (contadorAcertouAcaoTerceira/(contadorAcertouAcaoTerceira+contadorErrouAcaoTerceira))*100.0    
  } else {
    df_resultado$porcAcertoAcaoTerceira[linhaResultado] = 0    
  }
  
  # acho melhor pegar a Primeira  
  df_resultado$totBuracosSomafusao[linhaResultado] = contadorAcertouSomafusaoPrimeira + contadorErrouSomafusaoPrimeira
  
  df_resultado$acertosSomafusaoPrimeira[linhaResultado] = contadorAcertouSomafusaoPrimeira
  df_resultado$acertosSomafusaoSegunda[linhaResultado] = contadorAcertouSomafusaoSegunda
  df_resultado$acertosSomafusaoTerceira[linhaResultado] = contadorAcertouSomafusaoTerceira
  
  df_resultado$errosSomafusaoPrimeira[linhaResultado] = contadorErrouSomafusaoPrimeira
  df_resultado$errosSomafusaoSegunda[linhaResultado] = contadorErrouSomafusaoSegunda
  df_resultado$errosSomafusaoTerceira[linhaResultado] = contadorErrouSomafusaoTerceira
  
  # o calculo abaixo pode ter denominador zero e gerar NaN - colocar em um if
  if((contadorAcertouSomafusaoPrimeira + contadorErrouSomafusaoPrimeira)>0){
    df_resultado$porcAcertoSomafusaoPrimeira[linhaResultado] = (contadorAcertouSomafusaoPrimeira/(contadorAcertouSomafusaoPrimeira+contadorErrouSomafusaoPrimeira))*100.0    
  } else {
    df_resultado$porcAcertoSomafusaoPrimeira[linhaResultado] = 0
  }
  
  if((contadorAcertouSomafusaoSegunda + contadorErrouSomafusaoSegunda)>0){
    df_resultado$porcAcertoSomafusaoSegunda[linhaResultado] = (contadorAcertouSomafusaoSegunda/(contadorAcertouSomafusaoSegunda+contadorErrouSomafusaoSegunda))*100.0    
  } else {
    df_resultado$porcAcertoSomafusaoSegunda[linhaResultado] = 0
  }
  
  if((contadorAcertouSomafusaoTerceira + contadorErrouSomafusaoTerceira)>0){
    df_resultado$porcAcertoSomafusaoTerceira[linhaResultado] = (contadorAcertouSomafusaoTerceira/(contadorAcertouSomafusaoTerceira+contadorErrouSomafusaoTerceira))*100.0    
  } else {
    df_resultado$porcAcertoSomafusaoTerceira[linhaResultado] = 0
  }
  
  # acho melhor pegar a Primeira  
  df_resultado$totBuracosAtuador[linhaResultado] = contadorAcertouAtuadorPrimeira + contadorErrouAtuadorPrimeira
  
  df_resultado$acertosAtuador1Primeira[linhaResultado] = contadorAcertouAtuador1Primeira
  df_resultado$acertosAtuador1Segunda[linhaResultado] = contadorAcertouAtuador1Segunda
  df_resultado$acertosAtuador1Terceira[linhaResultado] = contadorAcertouAtuador1Terceira
  
  df_resultado$acertosAtuador2Primeira[linhaResultado] = contadorAcertouAtuador2Primeira
  df_resultado$acertosAtuador2Segunda[linhaResultado] = contadorAcertouAtuador2Segunda
  df_resultado$acertosAtuador2Terceira[linhaResultado] = contadorAcertouAtuador2Terceira
  
  df_resultado$acertosAtuador3Primeira[linhaResultado] = contadorAcertouAtuador3Primeira
  df_resultado$acertosAtuador3Segunda[linhaResultado] = contadorAcertouAtuador3Segunda
  df_resultado$acertosAtuador3Terceira[linhaResultado] = contadorAcertouAtuador3Terceira

  df_resultado$acertosAtuador4Primeira[linhaResultado] = contadorAcertouAtuador4Primeira
  df_resultado$acertosAtuador4Segunda[linhaResultado] = contadorAcertouAtuador4Segunda
  df_resultado$acertosAtuador4Terceira[linhaResultado] = contadorAcertouAtuador4Terceira

  df_resultado$errosAtuador1Primeira[linhaResultado] = contadorErrouAtuador1Primeira
  df_resultado$errosAtuador1Segunda[linhaResultado] = contadorErrouAtuador1Segunda
  df_resultado$errosAtuador1Terceira[linhaResultado] = contadorErrouAtuador1Terceira
  
  df_resultado$errosAtuador2Primeira[linhaResultado] = contadorErrouAtuador2Primeira
  df_resultado$errosAtuador2Segunda[linhaResultado] = contadorErrouAtuador2Segunda
  df_resultado$errosAtuador2Terceira[linhaResultado] = contadorErrouAtuador2Terceira
  
  df_resultado$errosAtuador3Primeira[linhaResultado] = contadorErrouAtuador3Primeira
  df_resultado$errosAtuador3Segunda[linhaResultado] = contadorErrouAtuador3Segunda
  df_resultado$errosAtuador3Terceira[linhaResultado] = contadorErrouAtuador3Terceira
  
  df_resultado$errosAtuador4Primeira[linhaResultado] = contadorErrouAtuador4Primeira
  df_resultado$errosAtuador4Segunda[linhaResultado] = contadorErrouAtuador4Segunda
  df_resultado$errosAtuador4Terceira[linhaResultado] = contadorErrouAtuador4Terceira

  # o calculo abaixo pode ter denominador zero e gerar NaN - colocar em um if
  if((contadorAcertouAtuadorPrimeira + contadorErrouAtuadorPrimeira)>0){
    df_resultado$porcAcertosAtuadorPrimeira[linhaResultado] = (contadorAcertouAtuadorPrimeira/(contadorAcertouAtuadorPrimeira + contadorErrouAtuadorPrimeira))*100.0    
  } else {
    df_resultado$porcAcertosAtuadorPrimeira[linhaResultado] = 0    
  }
  
  if((contadorAcertouAtuadorSegunda + contadorErrouAtuadorSegunda)){
    df_resultado$porcAcertosAtuadorSegunda[linhaResultado] = (contadorAcertouAtuadorSegunda/(contadorAcertouAtuadorSegunda + contadorErrouAtuadorSegunda))*100.0    
  } else {
    df_resultado$porcAcertosAtuadorSegunda[linhaResultado] = 0    
  }
  
  if((contadorAcertouAtuadorTerceira + contadorErrouAtuadorTerceira)){
    df_resultado$porcAcertosAtuadorTerceira[linhaResultado] = (contadorAcertouAtuadorTerceira/(contadorAcertouAtuadorTerceira + contadorErrouAtuadorTerceira))*100.0    
  } else {
    df_resultado$porcAcertosAtuadorTerceira[linhaResultado] = 0    
  }
  
  
  ##################################################################
  # gravação do arquivo de resultado do processamento
  write_json(df_resultado, ArquivoResultado)
  
} # fim da função processaUmArquivoCenario1


##################################################################################
# FUNCTION : processaUmArquivoCenario2
#
# processamento de cada arquivo para cada percentual de buracos para o cenario 2
#
##################################################################################

processaUmArquivoCenario2 <- function (vez,
                                       linhaResultado,
                                       registros,
                                       furos,
                                       df_white,
                                       df_black) {

  ###################################################################################
  # carrega a base na variável bn_df como um dataframe
  ###################################################################################
  #
  nomeArquivo = paste(NomeExperimento,"_",registros,".csv",sep="",collapse="")
  bn_df <- read.csv(nomeArquivo)
  
  ###################################################################################
  # Converter as colunas para Factor para a rede bayesiana - função hc()
  ###################################################################################
  #
  bn_df$c   = as.factor(bn_df$c)
  bn_df$s1  = as.factor(bn_df$s1)
  bn_df$s2  = as.factor(bn_df$s2)
  bn_df$s3  = as.factor(bn_df$s3)
  bn_df$s4  = as.factor(bn_df$s4)
  bn_df$f1  = as.factor(bn_df$f1)
  bn_df$f2  = as.factor(bn_df$f2)
  bn_df$a1  = as.factor(bn_df$a1)
  bn_df$a2  = as.factor(bn_df$a2)
  bn_df$r   = as.factor(bn_df$r)
  
  ###################################################################################
  # dividir a amostra em duas partes: treino e teste
  ###################################################################################
  #
  divisao = sample.split(bn_df$a1,SplitRatio = 0.5)
  
  base_treinamento      = subset(bn_df, divisao == TRUE)
  base_teste            = subset(bn_df, divisao == FALSE)
  base_teste_sem_buraco = subset(bn_df, divisao == FALSE)
  
  ###################################################################################
  # aplica a função hc na variável bn_df e guarda o resultado na variável res
  # Sobre a função: Agrupamento hierárquico aglomerativo baseado em critérios de 
  # máxima verossimilhança para modelos de mistura gaussiana parametrizados por 
  # decomposição de autovalores.
  ###################################################################################
  # O que eu entendi: está criando a RB com o algoritmo "max-min hill climbing", 
  # que é um algoritmo guloso.
  ###################################################################################
  #
  res = hc(base_treinamento, whitelist = df_white, blacklist = df_black)
  
  ###################################################################################
  # Grava o Plot das dependências funcionais entre as variáveis
  ###################################################################################
  #
  nomeArquivoImagem = paste(NomeExperimento,"_imagem.png",sep="",collapse="")
  if(!file.exists(nomeArquivoImagem)){
     png(file = nomeArquivoImagem)
     plot(res)
     dev.off()
  }
  ###################################################################################
  # treinar a rede
  # Depois de aprender a estrutura, é preciso descobrir as tabelas de probabilidade
  # condicional (CPTs) em cada nó. A função bn.fit executa o algoritmo EM para 
  # aprender CPT para diferentes nós no gráfico acima.
  ###################################################################################
  #
  #cat("treinar a rede\n")
  fittedbn <- bn.fit(res, data = base_treinamento)
  
  ###################################################################################
  # Explorando um dos cálculos
  # Por exemplo, vamos ver o que está dentro do nó s1
  ###################################################################################
  #
  #cat("explorando um dos cálculos\n")
  # fittedbn é um objeto da classe list
  #print(fittedbn)
  # s1 
  #print(fittedbn$s1)
  
  
  ###################################################################################
  # insere nulos
  ###################################################################################
  #
  porcentagem = furos/100
  
  #cat("Porcentagem=",porcentagem,"\n")
  
  nlin <- nrow(base_teste)
  ncol <- ncol(base_teste)
  totalelementos <- nlin * ncol
  porc_aleatorio <- totalelementos * porcentagem
  
  if(porc_aleatorio<1){
    porc_aleatorio=1
  }
  
  aleatorios <- sort(sample(1:totalelementos, porc_aleatorio))
  
  for (i in 1:porc_aleatorio){
    substituir <- aleatorios[i]
    linsub     <- (substituir %/% ncol)
    colsub     <- (substituir %% ncol)
    if(colsub==0){
      colsub <- ncol
    }
    base_teste[linsub,colsub] <- 'NULL'
  }
 
  #cat("Inseriu nulos\n")  
  ###################################################################################
  # Verifica se deu linha 100% nula
  ###################################################################################
  #
  contaNulo <- 0
  for (i in 1:nlin){
    for(j in 1:ncol){
      if (is.na  (base_teste[i,j]) || 
          is.null(base_teste[i,j]) ||
          (base_teste[i,j]=="NULL")){
        contaNulo = contaNulo + 1
      }
    }
    if(contaNulo==ncol){
      print("tudo nulo, não da pra fazer a predição")
      contaNulo <- 0 #24/07/2020
      #stop("tudo nulo, não da pra fazer a predição")
	  }
    else
      contaNulo <- 0
  }
  #cat("Verificou linhas todas de buracos")
  ###################################################################################
  # Conta total de buracos na base
  # Conta total de linhas sem buracos
  # Conta total de linhas com buracos
  ###################################################################################
  #
  contadorDeBuracos    <- 0
  qtdlinhasSemBuracos  <- 0
  qtdlinhasComBuracos  <- 0
  
  for (i in 1:nlin){
    contAux=0
    for(j in 1:ncol){
      if (is.na  (base_teste[i,j]) || 
          is.null(base_teste[i,j]) ||
          (base_teste[i,j]=="NULL")){
        contAux=contAux+1
        contadorDeBuracos = contadorDeBuracos + 1
      }
    }
    if(contAux==0)
      qtdlinhasSemBuracos = qtdlinhasSemBuracos + 1
    else
      qtdlinhasComBuracos = qtdlinhasComBuracos + 1
  }
  
  #cat("Contou linhas e buracos\n")  
  
  ###################################################################################
  # informa valores para as colunas e suas categorias
  # O ideal seria vir de algum arquivo...
  ###################################################################################
  #
  valores <- list(c('0', '1'),
                  c('0', '1'),
                  c('0', '1'),
                  c('0', '1'),
                  c('0', '1'),
                  c('0', '1'),
                  c('0', '1'),
                  c('00', '01', '10', '11'),
                  c('0', '1'),
                  c('0', '1'))
  
  categorias <- list(c('c','s1','s2','s3','s4'),
                     c('f1','f2'),
                     c('r'),
                     c('a1','a2'))
  
  ###################################################################################
  # inicialização dos contadores gerais do processamento
  ###################################################################################
  #

  contadorDeBuracosErradosPrimeira  <- 0
  contadorDeBuracosErradosSegunda   <- 0
  contadorDeBuracosErradosTerceira  <- 0
  qtdAcertouTudoPrimeira            <- 0
  qtdAcertouTudoSegunda             <- 0
  qtdAcertouTudoTerceira            <- 0
  
  contadorAcertouAtuador1Primeira   <- 0
  contadorAcertouAtuador1Segunda    <- 0
  contadorAcertouAtuador1Terceira   <- 0
  contadorAcertouAtuador2Primeira   <- 0  
  contadorAcertouAtuador2Segunda    <- 0 
  contadorAcertouAtuador2Terceira   <- 0  
  contadorAcertouF1Primeira         <- 0  
  contadorAcertouF1Segunda          <- 0 
  contadorAcertouF1Terceira         <- 0  
  contadorAcertouF2Primeira         <- 0    
  contadorAcertouF2Segunda          <- 0  
  contadorAcertouF2Terceira         <- 0    
  contadorAcertouRPrimeira          <- 0  
  contadorAcertouRSegunda           <- 0 
  contadorAcertouRTerceira          <- 0  
  contadorAcertouSensor1Primeira    <- 0    
  contadorAcertouSensor1Segunda     <- 0  
  contadorAcertouSensor1Terceira    <- 0    
  contadorAcertouSensor2Primeira    <- 0  
  contadorAcertouSensor2Segunda     <- 0 
  contadorAcertouSensor2Terceira    <- 0  
  contadorAcertouSensor3Primeira    <- 0    
  contadorAcertouSensor3Segunda     <- 0  
  contadorAcertouSensor3Terceira    <- 0    
  contadorAcertouSensor4Primeira    <- 0  
  contadorAcertouSensor4Segunda     <- 0 
  contadorAcertouSensor4Terceira    <- 0  
  contadorAcertouSensorCPrimeira    <- 0    
  contadorAcertouSensorCSegunda     <- 0  
  contadorAcertouSensorCTerceira    <- 0    
  
  contadorErrouAtuador1Primeira     <- 0
  contadorErrouAtuador1Segunda      <- 0
  contadorErrouAtuador1Terceira     <- 0
  contadorErrouAtuador2Primeira     <- 0
  contadorErrouAtuador2Segunda      <- 0
  contadorErrouAtuador2Terceira     <- 0
  contadorErrouF1Primeira           <- 0
  contadorErrouF1Segunda            <- 0
  contadorErrouF1Terceira           <- 0
  contadorErrouF2Primeira           <- 0
  contadorErrouF2Segunda            <- 0
  contadorErrouF2Terceira           <- 0
  contadorErrouRPrimeira            <- 0
  contadorErrouRSegunda             <- 0
  contadorErrouRTerceira            <- 0
  contadorErrouSensor1Primeira      <- 0
  contadorErrouSensor1Segunda       <- 0
  contadorErrouSensor1Terceira      <- 0
  contadorErrouSensor2Primeira      <- 0
  contadorErrouSensor2Segunda       <- 0
  contadorErrouSensor2Terceira      <- 0
  contadorErrouSensor3Primeira      <- 0
  contadorErrouSensor3Segunda       <- 0
  contadorErrouSensor3Terceira      <- 0
  contadorErrouSensor4Primeira      <- 0
  contadorErrouSensor4Segunda       <- 0
  contadorErrouSensor4Terceira      <- 0
  contadorErrouSensorCPrimeira      <- 0
  contadorErrouSensorCSegunda       <- 0
  contadorErrouSensorCTerceira      <- 0
  
  ###################################################################################
  # Laço principal para todas as instâncias
  # do arquivo
  ###################################################################################
  #
  for(i1 in 1:nlin){
    #cat("***Laco Principal - processando linha ", i1, "\n")
    dado <- base_teste[i1,]
    
    #####################################################
    # Verificando se a linha tem buracos
    # se não tiver então pular para a próxima linha
    #####################################################
    qtdBuracosLinha = 0
    
    for (i2 in 1:length(dado)){
      if (is.na(dado[i2]) ||
          is.null(dado[i2]) ||
          (dado[i2]=="NULL"))
        qtdBuracosLinha = qtdBuracosLinha + 1        
    }
    
    
    if(qtdBuracosLinha==0 ||
       qtdBuracosLinha==length(dado) ||
       qtdBuracosLinha/length(dado) > 0.75){ # 24/07/2020 se não tem buraco ou só tem buraco, pular.
      #não precisa fazer queries ... então ir para o próximo
      #print ("Linha sem buracos... pulada")
      next
    }
    
    
    ##########################################
    # monta as queries
    ##########################################
    
    # limpando as variáveis
    dfv1 <- NULL
    dfv2 <- NULL
    dadoCopia <- NULL
    
    # fazendo uma cópia do dado (dataframe)
    dadoCopia <- dado
    
    # guardando o nome das colunas do dado (dataframe)
    nomeColunas <- colnames(dado)
    
    flag <- TRUE
    for (i2 in length(dado):1)
      if (is.na(dado[i2]) ||
          is.null(dado[i2]) ||
          (dado[i2]=="NULL"))
      {
        dadoCopia <- dadoCopia[,-c(i2)]
        if(flag)
        {
          flag <- FALSE
          dfV1 <- data.frame(valores[i2])
          names(dfV1) <- nomeColunas[i2]
        } else {
          dfV2 <- data.frame(valores[i2])
          names(dfV2) <- nomeColunas[i2]
          dfV1 <- cartesianExpand(dfV1, dfV2)
        }
      }
    
    
    queries <- as.data.frame(cartesianExpand(dadoCopia, dfV1))
    
    # obs: valores nulos vão no evento e os não nulos nas evidencias
    strEventoColuna    <- NULL
    strEvidenciaColuna <- NULL
    strEventoValor     <- NULL
    strEvidenciaValor  <- NULL
    
    contadorNull = 0
    for (i3 in length(dado):1)
      if (is.na(dado[i3]) ||
          is.null(dado[i3]) ||
          (dado[i3]=="NULL")){
        contadorNull = contadorNull + 1
        strEventoColuna <- c(nomeColunas[i3],strEventoColuna)
      } else {
        strEvidenciaColuna <- c(nomeColunas[i3],strEvidenciaColuna)
        strEvidenciaValor <- c((as.character(dado[1,i3])),strEvidenciaValor)
      }
    
    # devo pegar as três maiores probabilidades
    maiorProbabilidade = -1
    segundaMaiorProbabilidade = -1
    terceiraMaiorProbabilidade = -1
    
    queryMaiorProbabilidade = NULL
    querySegundaMaiorProbabilidade = NULL
    queryTerceiraMaiorProbabilidade = NULL
    
    indiceMaior = -1
    indiceSegundoMaior = -1
    indiceTerceiroMaior = -1
    
    #cat("nrow(queries) ===> ", nrow(queries), "\n")
    
    
    # percorrendo cada linha das queries
    for(i4 in 1:nrow(queries)){
      for(i5 in (length(dado)-contadorNull+1):(length(dado))){
        strEventoValor <- c((as.character(queries[i4,i5])),strEventoValor)
      }
      
      str1 = paste("(", strEventoColuna   , " == \"",strEventoValor   , "\")", sep = "", collapse = " & ")
      str2 = paste("(", strEvidenciaColuna, " == \"",strEvidenciaValor, "\")", sep = "")
      
      cmd = paste("cpquery(fittedbn, event = ", str1, ", evidence = ", str2, ")", sep = "")
      probabilidade = eval(parse(text = cmd))
      
      #cat("probabilidade===>", probabilidade, "\n")
      
     
      #pegando as três maiores probabilidades
      if(probabilidade > maiorProbabilidade){
        # a segunda passa a ser a terceira
        terceiraMaiorProbabilidade = segundaMaiorProbabilidade
        queryTerceiraMaiorProbabilidade = querySegundaMaiorProbabilidade
        indiceTerceiroMaior = indiceSegundoMaior
        
        # a primeira passa a ser a segunda
        segundaMaiorProbabilidade = maiorProbabilidade
        querySegundaMaiorProbabilidade = queryMaiorProbabilidade
        indiceSegundoMaior = indiceMaior
        
        # a nova passa a ser a primeira
        maiorProbabilidade = probabilidade
        queryMaiorProbabilidade = queries[i4,]
        indiceMaior = i4
      } else {
        if(probabilidade > segundaMaiorProbabilidade){
          # a segunda passa a ser a terceira
          terceiraMaiorProbabilidade = segundaMaiorProbabilidade
          queryTerceiraMaiorProbabilidade = querySegundaMaiorProbabilidade
          indiceTerceiroMaior = indiceSegundoMaior
          
          # a nova passa a ser a segunda
          segundaMaiorProbabilidade = probabilidade
          querySegundaMaiorProbabilidade = queries[i4,]
          indiceSegundoMaior = i4
        } else {
          if(probabilidade > terceiraMaiorProbabilidade){
            # a nova passa a ser a terceira
            terceiraMaiorProbabilidade = probabilidade
            queryTerceiraMaiorProbabilidade = queries[i4,]
            indiceTerceiraMaior = i4
          }
        }
      }
      
      strEventoValor <- NULL
    } # for i4

    
    ###################################################################################
    #  
    #  __   
    # /_ |  
    #  | |              Analisando os acertos e erros de primeira
    #  | | 
    #  | |
    #  |_|
    #
    ###################################################################################
    if(maiorProbabilidade>-1){
      #########################################################
      #cat ("Maior Probabiliade = ", maiorProbabilidade,"\n")
      #print ("Linha com buracos  = ")
      #print (dado[,order(names(dado))])
      #########################################################    
      
      ## auxiliar para identificar os buracos originais
      aux1 <- dado[,order(names(dado))]
      aux2 <- colnames(aux1)
      
      #########################################################
      #print ("Query selecionada  = ")
      #print (queryMaiorProbabilidade[,order(names(queryMaiorProbabilidade))])
      #print ("Linha sem buracos  = ")
      #print (base_teste_sem_buraco[i1,order(names(bn_df))])
      #########################################################
      contadorDiferencasPrimeira = 0
      ContadorIgualdadesPrimeira = 0
      
      # AA guarda a query com os buracos preenchidos com os valores de maior probabilidade
      AA <- queryMaiorProbabilidade[,order(names(queryMaiorProbabilidade))]
      # BB guarda a linha com os valores originais para verificar se acertou ou não
      BB <- base_teste_sem_buraco[i1,order(names(bn_df))]
      
      for(ww in 1:length(dado)){
        if(as.character(AA[,ww]) == as.character(BB[,ww])){
          if(is.na(aux1[ww]) || is.null(aux1[ww]) || (aux1[ww]=="NULL")){
            ## acertou um buraco
            if (aux2[ww] == "a1"){contadorAcertouAtuador1Primeira = contadorAcertouAtuador1Primeira + 1} 
            else
              if (aux2[ww] == "a2"){contadorAcertouAtuador2Primeira = contadorAcertouAtuador2Primeira + 1}
            else
              if(aux2[ww] == "r"){contadorAcertouRPrimeira = contadorAcertouRPrimeira + 1}
            else 
              if(aux2[ww] == "f1"){contadorAcertouF1Primeira = contadorAcertouF1Primeira + 1}
            else 
              if(aux2[ww] == "f2"){contadorAcertouF2Primeira = contadorAcertouF2Primeira + 1}
            else 
              if(aux2[ww] == "s1"){contadorAcertouSensor1Primeira = contadorAcertouSensor1Primeira + 1}
            else
              if(aux2[ww] == "s2"){contadorAcertouSensor2Primeira = contadorAcertouSensor2Primeira + 1}
            else
              if(aux2[ww] == "s3"){contadorAcertouSensor3Primeira = contadorAcertouSensor3Primeira + 1}
            else
              if(aux2[ww] == "s4"){contadorAcertouSensor4Primeira = contadorAcertouSensor4Primeira + 1}
            else
              if(aux2[ww] == "c"){contadorAcertouSensorcPrimeira = contadorAcertouSensorCPrimeira + 1}
          }
          
          ContadorIgualdadesPrimeira <- ContadorIgualdadesPrimeira + 1
        } else {
          if(is.na(aux1[ww]) || is.null(aux1[ww]) || (aux1[ww]=="NULL")){
            ## errou um buraco
            if (aux2[ww] == "a1"){contadorErrouAtuador1Primeira = contadorErrouAtuador1Primeira + 1} 
            else
              if (aux2[ww] == "a2"){contadorErrouAtuador2Primeira = contadorErrouAtuador2Primeira + 1}
            else
              if(aux2[ww] == "r"){contadorErrouRPrimeira = contadorErrouRPrimeira + 1}
            else 
              if(aux2[ww] == "f1"){contadorErrouF1Primeira = contadorErrouF1Primeira + 1}
            else 
              if(aux2[ww] == "f2"){contadorErrouF2Primeira = contadorErrouF2Primeira + 1}
            else 
              if(aux2[ww] == "s1"){contadorErrouSensor1Primeira = contadorErrouSensor1Primeira + 1}
            else
              if(aux2[ww] == "s2"){contadorErrouSensor2Primeira = contadorErrouSensor2Primeira + 1}
            else
              if(aux2[ww] == "s3"){contadorErrouSensor3Primeira = contadorErrouSensor3Primeira + 1}
            else
              if(aux2[ww] == "s4"){contadorErrouSensor4Primeira = contadorErrouSensor4Primeira + 1}
            else
              if(aux2[ww] == "c"){contadorErrouSensorCPrimeira = contadorErrouSensorCPrimeira + 1}
          }
          contadorDiferencasPrimeira <- contadorDiferencasPrimeira + 1
          contadorDeBuracosErradosPrimeira = contadorDeBuracosErradosPrimeira + 1
          #########################################################        
          #cat("Q: ",colnames(AA[ww]),"...:",as.character(AA[,ww]),"\t",
          #    "B: ",colnames(BB[ww]),"...:",as.character(BB[,ww]),"\n")
          #########################################################        
        }
      } # for ww
      
      #########################################################    
      #cat ("Qtde de Buracos certos  Primeira  = ", (qtdBuracosLinha-contadorDiferencasPrimeira), "\n")
      #cat ("Qtde de Buracos errados Primeira  = ", contadorDiferencasPrimeira, "\n")
      #cat ("Qtde de Buracos                   = ", qtdBuracosLinha, "\n")
      #########################################################    
      
      if(contadorDiferencasPrimeira==0)
        qtdAcertouTudoPrimeira = qtdAcertouTudoPrimeira + 1    
      
    } # if(maiorProbabilidade>-1)
    
    
    ###################################################################################
    #  
    #  ___  
    # |__ \
    #    ) |            Analisando os acertos e erros de Segunda
    #   / / 
    #  / /_ 
    # |____|
    #
    ###################################################################################
    if(segundaMaiorProbabilidade>-1){
      #########################################################
      #cat ("Segunda Maior Probabiliade = ", segundaMaiorProbabilidade,"\n")
      #print ("Linha com buracos  = ")
      #print (dado[,order(names(dado))])
      #########################################################    
      
      ## auxiliar para identificar os buracos originais
      aux1 <- dado[,order(names(dado))]
      aux2 <- colnames(aux1)
      
      #########################################################
      #print ("Query selecionada  = ")
      #print (querySegundaMaiorProbabilidade[,order(names(querySegundaMaiorProbabilidade))])
      #print ("Linha sem buracos  = ")
      #print (base_teste_sem_buraco[i1,order(names(bn_df))])
      #########################################################
      contadorDiferencasSegunda = 0
      ContadorIgualdadesSegunda = 0
      
      # AA guarda a query com os buracos preenchidos com os valores de maior probabilidade
      AA <- querySegundaMaiorProbabilidade[,order(names(querySegundaMaiorProbabilidade))]
      # BB guarda a linha com os valores originais para verificar se acertou ou não
      BB <- base_teste_sem_buraco[i1,order(names(bn_df))]
      
      for(ww in 1:length(dado)){
        if(as.character(AA[,ww]) == as.character(BB[,ww])){
          if(is.na(aux1[ww]) || is.null(aux1[ww]) || (aux1[ww]=="NULL")){
            ## acertou um buraco
            if (aux2[ww] == "a1"){contadorAcertouAtuador1Segunda = contadorAcertouAtuador1Segunda + 1} 
            else
              if (aux2[ww] == "a2"){contadorAcertouAtuador2Segunda = contadorAcertouAtuador2Segunda + 1}
            else
              if(aux2[ww] == "r"){contadorAcertouRSegunda = contadorAcertouRSegunda + 1}
            else 
              if(aux2[ww] == "f1"){contadorAcertouF1Segunda = contadorAcertouF1Segunda + 1}
            else 
              if(aux2[ww] == "f2"){contadorAcertouF2Segunda = contadorAcertouF2Segunda + 1}
            else 
              if(aux2[ww] == "s1"){contadorAcertouSensor1Segunda = contadorAcertouSensor1Segunda + 1}
            else
              if(aux2[ww] == "s2"){contadorAcertouSensor2Segunda = contadorAcertouSensor2Segunda + 1}
            else
              if(aux2[ww] == "s3"){contadorAcertouSensor3Segunda = contadorAcertouSensor3Segunda + 1}
            else
              if(aux2[ww] == "s4"){contadorAcertouSensor4Segunda = contadorAcertouSensor4Segunda + 1}
            else
              if(aux2[ww] == "c"){contadorAcertouSensorcSegunda = contadorAcertouSensorCSegunda + 1}
          }
          
          ContadorIgualdadesSegunda <- ContadorIgualdadesSegunda + 1
        } else {
          if(is.na(aux1[ww]) || is.null(aux1[ww]) || (aux1[ww]=="NULL")){
            ## errou um buraco
            if (aux2[ww] == "a1"){contadorErrouAtuador1Segunda = contadorErrouAtuador1Segunda + 1} 
            else
              if (aux2[ww] == "a2"){contadorErrouAtuador2Segunda = contadorErrouAtuador2Segunda + 1}
            else
              if(aux2[ww] == "r"){contadorErrouRSegunda = contadorErrouRSegunda + 1}
            else 
              if(aux2[ww] == "f1"){contadorErrouF1Segunda = contadorErrouF1Segunda + 1}
            else 
              if(aux2[ww] == "f2"){contadorErrouF2Segunda = contadorErrouF2Segunda + 1}
            else 
              if(aux2[ww] == "s1"){contadorErrouSensor1Segunda = contadorErrouSensor1Segunda + 1}
            else
              if(aux2[ww] == "s2"){contadorErrouSensor2Segunda = contadorErrouSensor2Segunda + 1}
            else
              if(aux2[ww] == "s3"){contadorErrouSensor3Segunda = contadorErrouSensor3Segunda + 1}
            else
              if(aux2[ww] == "s4"){contadorErrouSensor4Segunda = contadorErrouSensor4Segunda + 1}
            else
              if(aux2[ww] == "c"){contadorErrouSensorCSegunda = contadorErrouSensorCSegunda + 1}
          }
          contadorDiferencasSegunda <- contadorDiferencasSegunda + 1
          contadorDeBuracosErradosSegunda = contadorDeBuracosErradosSegunda + 1
          #########################################################        
          #cat("Q: ",colnames(AA[ww]),"...:",as.character(AA[,ww]),"\t",
          #    "B: ",colnames(BB[ww]),"...:",as.character(BB[,ww]),"\n")
          #########################################################        
        }
      } # for ww
      
      
      #########################################################    
      #cat ("Qtde de Buracos certos  Segunda = ", (qtdBuracosLinha-contadorDiferencasSegunda), "\n")
      #cat ("Qtde de Buracos errados Segunda = ", contadorDiferencasSegunda, "\n")
      #cat ("Qtde de Buracos                 = ", qtdBuracosLinha, "\n")
      #########################################################    
      
      if(contadorDiferencasSegunda==0)
        qtdAcertouTudoSegunda = qtdAcertouTudoSegunda + 1    
      
    } # if(segundaMaiorProbabilidade>-1)

    ###################################################################################
    #  
    #  ____  
    # |___ \   
    #   __) |           Analisando os acertos e erros de Terceira
    #  |__ < 
    #  ___) |
    # |____/
    #
    ###################################################################################
    if(terceiraMaiorProbabilidade>-1){
      #########################################################
      #cat ("Terceira Maior Probabiliade = ", terceiraMaiorProbabilidade,"\n")
      #print ("Linha com buracos  = ")
      #print (dado[,order(names(dado))])
      #########################################################    
      
      ## auxiliar para identificar os buracos originais
      aux1 <- dado[,order(names(dado))]
      aux2 <- colnames(aux1)
      
      #########################################################
      #print ("Query selecionada  = ")
      #print (queryTerceiraMaiorProbabilidade[,order(names(queryTerceiraMaiorProbabilidade))])
      #print ("Linha sem buracos  = ")
      #print (base_teste_sem_buraco[i1,order(names(bn_df))])
      #########################################################
      contadorDiferencasTerceira = 0
      ContadorIgualdadesTerceira = 0
      
      # AA guarda a query com os buracos preenchidos com os valores de maior probabilidade
      AA <- queryTerceiraMaiorProbabilidade[,order(names(queryTerceiraMaiorProbabilidade))]
      # BB guarda a linha com os valores originais para verificar se acertou ou não
      BB <- base_teste_sem_buraco[i1,order(names(bn_df))]
      
      for(ww in 1:length(dado)){
        if(as.character(AA[,ww]) == as.character(BB[,ww])){
          if(is.na(aux1[ww]) || is.null(aux1[ww]) || (aux1[ww]=="NULL")){
            ## acertou um buraco
            if (aux2[ww] == "a1"){contadorAcertouAtuador1Terceira = contadorAcertouAtuador1Terceira + 1} 
            else
              if (aux2[ww] == "a2"){contadorAcertouAtuador2Terceira = contadorAcertouAtuador2Terceira + 1}
            else
              if(aux2[ww] == "r"){contadorAcertouRTerceira = contadorAcertouRTerceira + 1}
            else 
              if(aux2[ww] == "f1"){contadorAcertouF1Terceira = contadorAcertouF1Terceira + 1}
            else 
              if(aux2[ww] == "f2"){contadorAcertouF2Terceira = contadorAcertouF2Terceira + 1}
            else 
              if(aux2[ww] == "s1"){contadorAcertouSensor1Terceira = contadorAcertouSensor1Terceira + 1}
            else
              if(aux2[ww] == "s2"){contadorAcertouSensor2Terceira = contadorAcertouSensor2Terceira + 1}
            else
              if(aux2[ww] == "s3"){contadorAcertouSensor3Terceira = contadorAcertouSensor3Terceira + 1}
            else
              if(aux2[ww] == "s4"){contadorAcertouSensor4Terceira = contadorAcertouSensor4Terceira + 1}
            else
              if(aux2[ww] == "c"){contadorAcertouSensorcTerceira = contadorAcertouSensorCTerceira + 1}
          }
          
          ContadorIgualdadesTerceira <- ContadorIgualdadesTerceira + 1
        } else {
          if(is.na(aux1[ww]) || is.null(aux1[ww]) || (aux1[ww]=="NULL")){
            ## errou um buraco
            if (aux2[ww] == "a1"){contadorErrouAtuador1Terceira = contadorErrouAtuador1Terceira + 1} 
            else
              if (aux2[ww] == "a2"){contadorErrouAtuador2Terceira = contadorErrouAtuador2Terceira + 1}
            else
              if(aux2[ww] == "r"){contadorErrouRTerceira = contadorErrouRTerceira + 1}
            else 
              if(aux2[ww] == "f1"){contadorErrouF1Terceira = contadorErrouF1Terceira + 1}
            else 
              if(aux2[ww] == "f2"){contadorErrouF2Terceira = contadorErrouF2Terceira + 1}
            else 
              if(aux2[ww] == "s1"){contadorErrouSensor1Terceira = contadorErrouSensor1Terceira + 1}
            else
              if(aux2[ww] == "s2"){contadorErrouSensor2Terceira = contadorErrouSensor2Terceira + 1}
            else
              if(aux2[ww] == "s3"){contadorErrouSensor3Terceira = contadorErrouSensor3Terceira + 1}
            else
              if(aux2[ww] == "s4"){contadorErrouSensor4Terceira = contadorErrouSensor4Terceira + 1}
            else
              if(aux2[ww] == "c"){contadorErrouSensorCTerceira = contadorErrouSensorCTerceira + 1}
          }
          contadorDiferencasTerceira <- contadorDiferencasTerceira + 1
          contadorDeBuracosErradosTerceira = contadorDeBuracosErradosTerceira + 1
          #########################################################        
          #cat("Q: ",colnames(AA[ww]),"...:",as.character(AA[,ww]),"\t",
          #    "B: ",colnames(BB[ww]),"...:",as.character(BB[,ww]),"\n")
          #########################################################        
        }
      } # for ww
      
      
      #########################################################    
      #cat ("Qtde de Buracos certos  Terceira = ", (qtdBuracosLinha-contadorDiferencasTerceira), "\n")
      #cat ("Qtde de Buracos errados Terceira = ", contadorDiferencasTerceira, "\n")
      #cat ("Qtde de Buracos                  = ", qtdBuracosLinha, "\n")
      #########################################################    
      
      if(contadorDiferencasTerceira==0)
        qtdAcertouTudoTerceira = qtdAcertouTudoTerceira + 1    
      
    } # if(terceiraMaiorProbabilidade>-1)

  } #  i1
  ##########################################
  # fim do Laço principal
  ##########################################
  contadorAcertouSensorPrimeira <- contadorAcertouSensor1Primeira +
                                   contadorAcertouSensor2Primeira + 
                                   contadorAcertouSensor3Primeira + 
                                   contadorAcertouSensor4Primeira +
                                   contadorAcertouSensorCPrimeira

  contadorAcertouSensorSegunda  <- contadorAcertouSensor1Segunda  +
                                   contadorAcertouSensor2Segunda  + 
                                   contadorAcertouSensor3Segunda  + 
                                   contadorAcertouSensor4Segunda  +
                                   contadorAcertouSensorCSegunda
  
  contadorAcertouSensorTerceira <- contadorAcertouSensor1Terceira +
                                   contadorAcertouSensor2Terceira + 
                                   contadorAcertouSensor3Terceira + 
                                   contadorAcertouSensor4Terceira +
                                   contadorAcertouSensorCTerceira
  
  contadorErrouSensorPrimeira   <- contadorErrouSensor1Primeira   +
                                   contadorErrouSensor2Primeira   +
                                   contadorErrouSensor3Primeira   +
                                   contadorErrouSensor4Primeira   +
                                   contadorErrouSensorCPrimeira

  contadorErrouSensorSegunda    <- contadorErrouSensor1Segunda    +
                                   contadorErrouSensor2Segunda    +
                                   contadorErrouSensor3Segunda    +
                                   contadorErrouSensor4Segunda    +
                                   contadorErrouSensorCSegunda

  contadorErrouSensorTerceira   <- contadorErrouSensor1Terceira   +
                                   contadorErrouSensor2Terceira   +
                                   contadorErrouSensor3Terceira   +
                                   contadorErrouSensor4Terceira   +
                                   contadorErrouSensorCTerceira
  

  contadorAcertouAtuadorPrimeira <- contadorAcertouAtuador1Primeira +
                                    contadorAcertouAtuador2Primeira 
  
  contadorAcertouAtuadorSegunda  <- contadorAcertouAtuador1Segunda  +
                                    contadorAcertouAtuador2Segunda 
  
  contadorAcertouAtuadorTerceira <- contadorAcertouAtuador1Terceira +
                                    contadorAcertouAtuador2Terceira 
  
  contadorErrouAtuadorPrimeira   <- contadorErrouAtuador1Primeira   +
                                    contadorErrouAtuador2Primeira   

  contadorErrouAtuadorSegunda    <- contadorErrouAtuador1Segunda    +
                                    contadorErrouAtuador2Segunda   
  
  contadorErrouAtuadorTerceira   <- contadorErrouAtuador1Terceira   +
                                    contadorErrouAtuador2Terceira   
  
  contadorAcertouFusaoPrimeira   <- contadorAcertouF1Primeira +
                                    contadorAcertouF2Primeira
  
  contadorAcertouFusaoSegunda    <- contadorAcertouF1Segunda +
                                    contadorAcertouF2Segunda
  
  contadorAcertouFusaoTerceira   <- contadorAcertouF1Terceira +
                                    contadorAcertouF2Terceira
  
  contadorErrouFusaoPrimeira     <- contadorErrouF1Primeira +
                                    contadorErrouF2Primeira
  
  contadorErrouFusaoSegunda      <- contadorErrouF1Segunda +
                                    contadorErrouF2Segunda
  
  contadorErrouFusaoTerceira     <- contadorErrouF1Terceira +
                                    contadorErrouF2Terceira
  
  
  # atualizando o dataframe com base no ArquivoResultado
  df_resultado <- as.data.frame(fromJSON(ArquivoResultado))
  
  #variaveis para guardar no dataframe de resultado de processamento
  df_resultado$totBuracos[linhaResultado] = contadorDeBuracos
  
  df_resultado$totBuracosAcertouPrimeira[linhaResultado] = (contadorDeBuracos-contadorDeBuracosErradosPrimeira)
  df_resultado$totBuracosAcertouSegunda[linhaResultado] = (contadorDeBuracos-contadorDeBuracosErradosSegunda)
  df_resultado$totBuracosAcertouTerceira[linhaResultado] = (contadorDeBuracos-contadorDeBuracosErradosTerceira)
  
  df_resultado$totBuracosErrouPrimeira[linhaResultado] = contadorDeBuracosErradosPrimeira
  df_resultado$totBuracosErrouSegunda[linhaResultado] = contadorDeBuracosErradosSegunda
  df_resultado$totBuracosErrouTerceira[linhaResultado] = contadorDeBuracosErradosTerceira
  
  df_resultado$predicoes100CorretasPrimeira[linhaResultado] = qtdAcertouTudoPrimeira
  df_resultado$predicoes100CorretasSegunda[linhaResultado] = qtdAcertouTudoSegunda
  df_resultado$predicoes100CorretasTerceira[linhaResultado] = qtdAcertouTudoTerceira
  
  # acho melhor pegar a Primeira
  df_resultado$totBuracosSensores[linhaResultado] = contadorAcertouSensorPrimeira+contadorErrouSensorPrimeira
  
  df_resultado$acertosSensor1Primeira[linhaResultado] = contadorAcertouSensor1Primeira
  df_resultado$acertosSensor1Segunda[linhaResultado] = contadorAcertouSensor1Segunda
  df_resultado$acertosSensor1Terceira[linhaResultado] = contadorAcertouSensor1Terceira
  
  df_resultado$acertosSensor2Primeira[linhaResultado] = contadorAcertouSensor2Primeira
  df_resultado$acertosSensor2Segunda[linhaResultado] = contadorAcertouSensor2Segunda
  df_resultado$acertosSensor2Terceira[linhaResultado] = contadorAcertouSensor2Terceira
  
  df_resultado$acertosSensor3Primeira[linhaResultado] = contadorAcertouSensor3Primeira
  df_resultado$acertosSensor3Segunda[linhaResultado] = contadorAcertouSensor3Segunda
  df_resultado$acertosSensor3Terceira[linhaResultado] = contadorAcertouSensor3Terceira
  
  df_resultado$acertosSensor4Primeira[linhaResultado] = contadorAcertouSensor4Primeira
  df_resultado$acertosSensor4Segunda[linhaResultado] = contadorAcertouSensor4Segunda
  df_resultado$acertosSensor4Terceira[linhaResultado] = contadorAcertouSensor4Terceira
  
  df_resultado$acertosSensorCPrimeira[linhaResultado] = contadorAcertouSensorCPrimeira
  df_resultado$acertosSensorCSegunda[linhaResultado] = contadorAcertouSensorCSegunda
  df_resultado$acertosSensorCTerceira[linhaResultado] = contadorAcertouSensorCTerceira
  
  df_resultado$errosSensor1Primeira[linhaResultado] = contadorErrouSensor1Primeira
  df_resultado$errosSensor1Segunda[linhaResultado] = contadorErrouSensor1Segunda
  df_resultado$errosSensor1Terceira[linhaResultado] = contadorErrouSensor1Terceira
  
  df_resultado$errosSensor2Primeira[linhaResultado] = contadorErrouSensor2Primeira
  df_resultado$errosSensor2Segunda[linhaResultado] = contadorErrouSensor2Segunda
  df_resultado$errosSensor2Terceira[linhaResultado] = contadorErrouSensor2Terceira
  
  df_resultado$errosSensor3Primeira[linhaResultado] = contadorErrouSensor3Primeira
  df_resultado$errosSensor3Segunda[linhaResultado] = contadorErrouSensor3Segunda
  df_resultado$errosSensor3Terceira[linhaResultado] = contadorErrouSensor3Terceira
  
  df_resultado$errosSensor4Primeira[linhaResultado] = contadorErrouSensor4Primeira
  df_resultado$errosSensor4Segunda[linhaResultado] = contadorErrouSensor4Segunda
  df_resultado$errosSensor4Terceira[linhaResultado] = contadorErrouSensor4Terceira
  
  df_resultado$errosSensorCPrimeira[linhaResultado] = contadorErrouSensorCPrimeira
  df_resultado$errosSensorCSegunda[linhaResultado] = contadorErrouSensorCSegunda
  df_resultado$errosSensorCTerceira[linhaResultado] = contadorErrouSensorCTerceira
  
  # o calculo abaixo pode ter denominador zero e gerar NaN - colocar em um if
  if((contadorAcertouSensorPrimeira + contadorErrouSensorPrimeira)>0){
    df_resultado$porcAcertoSensorPrimeira[linhaResultado] = (contadorAcertouSensorPrimeira/(contadorAcertouSensorPrimeira + contadorErrouSensorPrimeira))*100.0    
  } else {
    df_resultado$porcAcertoSensorPrimeira[linhaResultado] = 0
  }
  
  if((contadorAcertouSensorSegunda + contadorErrouSensorSegunda)>0){
    df_resultado$porcAcertoSensorSegunda[linhaResultado] = (contadorAcertouSensorSegunda/(contadorAcertouSensorSegunda + contadorErrouSensorSegunda))*100.0    
  } else {
    df_resultado$porcAcertoSensorSegunda[linhaResultado] = 0
  }
  
  if((contadorAcertouSensorTerceira + contadorErrouSensorTerceira)>0){
    df_resultado$porcAcertoSensorTerceira[linhaResultado] = (contadorAcertouSensorTerceira/(contadorAcertouSensorTerceira + contadorErrouSensorTerceira))*100.0    
  } else {
    df_resultado$porcAcertoSensorTerceira[linhaResultado] = 0
  }
  
  # acho melhor pegar a Primeira
  df_resultado$totBuracosAcao[linhaResultado] = contadorAcertouRPrimeira + contadorErrouRPrimeira
  
  df_resultado$acertosAcaoPrimeira[linhaResultado] = contadorAcertouRPrimeira
  df_resultado$acertosAcaoSegunda[linhaResultado] = contadorAcertouRSegunda
  df_resultado$acertosAcaoTerceira[linhaResultado] = contadorAcertouRTerceira
  
  df_resultado$errosAcaoPrimeira[linhaResultado] = contadorErrouRPrimeira
  df_resultado$errosAcaoSegunda[linhaResultado] = contadorErrouRSegunda
  df_resultado$errosAcaoTerceira[linhaResultado] = contadorErrouRTerceira
  
  # o calculo abaixo pode ter denominador zero e gerar NaN - colocar em um if
  if((contadorAcertouRPrimeira + contadorErrouRPrimeira)>0){
    df_resultado$porcAcertoAcaoPrimeira[linhaResultado] = (contadorAcertouRPrimeira/(contadorAcertouRPrimeira+contadorErrouRPrimeira))*100.0    
  } else {
    df_resultado$porcAcertoAcaoPrimeira[linhaResultado] = 0    
  }
  
  if((contadorAcertouRSegunda + contadorErrouRSegunda)>0){
    df_resultado$porcAcertoAcaoSegunda[linhaResultado] = (contadorAcertouRSegunda/(contadorAcertouRSegunda+contadorErrouRSegunda))*100.0    
  } else {
    df_resultado$porcAcertoAcaoSegunda[linhaResultado] = 0    
  }
  
  if((contadorAcertouRTerceira + contadorErrouRTerceira)>0){
    df_resultado$porcAcertoAcaoTerceira[linhaResultado] = (contadorAcertouRTerceira/(contadorAcertouRTerceira+contadorErrouRTerceira))*100.0    
  } else {
    df_resultado$porcAcertoAcaoTerceira[linhaResultado] = 0    
  }

  # acho melhor pegar a Primeira  
  df_resultado$totBuracosSomafusao[linhaResultado] = contadorAcertouFusaoPrimeira + contadorErrouFusaoPrimeira

  df_resultado$acertosSomafusao1Primeira[linhaResultado] = contadorAcertouF1Primeira
  df_resultado$acertosSomafusao1Segunda[linhaResultado] = contadorAcertouF1Segunda
  df_resultado$acertosSomafusao1Terceira[linhaResultado] = contadorAcertouF1Terceira

  df_resultado$acertosSomafusao2Primeira[linhaResultado] = contadorAcertouF2Primeira
  df_resultado$acertosSomafusao2Segunda[linhaResultado] = contadorAcertouF2Segunda
  df_resultado$acertosSomafusao2Terceira[linhaResultado] = contadorAcertouF2Terceira

  df_resultado$errosSomafusao1Primeira[linhaResultado] = contadorErrouF1Primeira
  df_resultado$errosSomafusao1Segunda[linhaResultado] = contadorErrouF1Segunda
  df_resultado$errosSomafusao1Terceira[linhaResultado] = contadorErrouF1Terceira
  
  df_resultado$errosSomafusao2Primeira[linhaResultado] = contadorErrouF2Primeira
  df_resultado$errosSomafusao2Segunda[linhaResultado] = contadorErrouF2Segunda
  df_resultado$errosSomafusao2Terceira[linhaResultado] = contadorErrouF2Terceira
  
  # o calculo abaixo pode ter denominador zero e gerar NaN - colocar em um if
  if((contadorAcertouFusaoPrimeira + contadorErrouFusaoPrimeira)>0){
    df_resultado$porcAcertoSomafusaoPrimeira[linhaResultado] = (contadorAcertouFusaoPrimeira/(contadorAcertouFusaoPrimeira+contadorErrouFusaoPrimeira))*100.0    
  } else {
    df_resultado$porcAcertoSomafusaoPrimeira[linhaResultado] = 0
  }
  
  if((contadorAcertouFusaoSegunda + contadorErrouFusaoSegunda)>0){
    df_resultado$porcAcertoSomafusaoSegunda[linhaResultado] = (contadorAcertouFusaoSegunda/(contadorAcertouFusaoSegunda+contadorErrouFusaoSegunda))*100.0    
  } else {
    df_resultado$porcAcertoSomafusaoSegunda[linhaResultado] = 0
  }
  
  if((contadorAcertouFusaoTerceira + contadorErrouFusaoTerceira)>0){
    df_resultado$porcAcertoSomafusaoTerceira[linhaResultado] = (contadorAcertouFusaoTerceira/(contadorAcertouFusaoTerceira+contadorErrouFusaoTerceira))*100.0    
  } else {
    df_resultado$porcAcertoSomafusaoTerceira[linhaResultado] = 0
  }
  
  # acho melhor pegar a Primeira  
  df_resultado$totBuracosAtuador[linhaResultado] = contadorAcertouAtuadorPrimeira + contadorErrouAtuadorPrimeira
  
  df_resultado$acertosAtuador1Primeira[linhaResultado] = contadorAcertouAtuador1Primeira
  df_resultado$acertosAtuador1Segunda[linhaResultado] = contadorAcertouAtuador1Segunda
  df_resultado$acertosAtuador1Terceira[linhaResultado] = contadorAcertouAtuador1Terceira

  df_resultado$acertosAtuador2Primeira[linhaResultado] = contadorAcertouAtuador2Primeira
  df_resultado$acertosAtuador2Segunda[linhaResultado] = contadorAcertouAtuador2Segunda
  df_resultado$acertosAtuador2Terceira[linhaResultado] = contadorAcertouAtuador2Terceira
  
  df_resultado$errosAtuador1Primeira[linhaResultado] = contadorErrouAtuador1Primeira
  df_resultado$errosAtuador1Segunda[linhaResultado] = contadorErrouAtuador1Segunda
  df_resultado$errosAtuador1Terceira[linhaResultado] = contadorErrouAtuador1Terceira

  df_resultado$errosAtuador2Primeira[linhaResultado] = contadorErrouAtuador2Primeira
  df_resultado$errosAtuador2Segunda[linhaResultado] = contadorErrouAtuador2Segunda
  df_resultado$errosAtuador2Terceira[linhaResultado] = contadorErrouAtuador2Terceira
  
  # o calculo abaixo pode ter denominador zero e gerar NaN - colocar em um if
  if((contadorAcertouAtuadorPrimeira + contadorErrouAtuadorPrimeira)>0){
    df_resultado$porcAcertosAtuadorPrimeira[linhaResultado] = (contadorAcertouAtuadorPrimeira/(contadorAcertouAtuadorPrimeira + contadorErrouAtuadorPrimeira))*100.0    
  } else {
    df_resultado$porcAcertosAtuadorPrimeira[linhaResultado] = 0    
  }

  if((contadorAcertouAtuadorSegunda + contadorErrouAtuadorSegunda)){
    df_resultado$porcAcertosAtuadorSegunda[linhaResultado] = (contadorAcertouAtuadorSegunda/(contadorAcertouAtuadorSegunda + contadorErrouAtuadorSegunda))*100.0    
  } else {
    df_resultado$porcAcertosAtuadorSegunda[linhaResultado] = 0    
  }
  
  if((contadorAcertouAtuadorTerceira + contadorErrouAtuadorTerceira)){
    df_resultado$porcAcertosAtuadorTerceira[linhaResultado] = (contadorAcertouAtuadorTerceira/(contadorAcertouAtuadorTerceira + contadorErrouAtuadorTerceira))*100.0    
  } else {
    df_resultado$porcAcertosAtuadorTerceira[linhaResultado] = 0    
  }
  
  
  ##################################################################
  # gravação do arquivo de resultado do processamento
  write_json(df_resultado, ArquivoResultado)

} # fim da função processaUmArquivoCenario2


##################################################################################
# FUNCTION : processaUmArquivoCenario3
#
# processamento de cada arquivo para cada percentual de buracos para o cenario 3
#
##################################################################################

processaUmArquivoCenario3 <- function (vez,
                                       linhaResultado,
                                       registros,
                                       furos,
                                       df_white,
                                       df_black) {
  
  ###################################################################################
  # carrega a base na variável bn_df como um dataframe
  ###################################################################################
  #
  nomeArquivo = paste(NomeExperimento,"_",registros,".csv",sep="",collapse="")
  bn_df <- read.csv(nomeArquivo)

  ###################################################################################
  # Converter as colunas para Factor para a rede bayesiana - função hc()
  ###################################################################################
  # swamp
  bn_df$en  = as.factor(bn_df$en)
  bn_df$mi  = as.factor(bn_df$mi)
  # fog
  bn_df$gtb = as.factor(bn_df$gtb)
  bn_df$mq1 = as.factor(bn_df$mq1)
  bn_df$ls  = as.factor(bn_df$ls)
  bn_df$mq2 = as.factor(bn_df$mq2)
  bn_df$las = as.factor(bn_df$las)
  bn_df$outFog = as.factor(bn_df$outFog)  
  bn_df$aa1 = as.factor(bn_df$aa1)
  # cloud
  bn_df$iot = as.factor(bn_df$iot)
  bn_df$ori = as.factor(bn_df$ori)
  bn_df$mo  = as.factor(bn_df$mo)
  bn_df$ql  = as.factor(bn_df$ql)
  bn_df$cdb = as.factor(bn_df$cdb)
  bn_df$outCloud = as.factor(bn_df$outCloud)
  bn_df$aa2 = as.factor(bn_df$aa2)

  ###################################################################################
  # dividir a amostra em duas partes: treino e teste
  ###################################################################################
  #
  divisao = sample.split(bn_df$en,SplitRatio = 0.5)

  base_treinamento      = subset(bn_df, divisao == TRUE)
  base_teste            = subset(bn_df, divisao == FALSE)
  base_teste_sem_buraco = subset(bn_df, divisao == FALSE)
  
  ###################################################################################
  # aplica a função hc na variável bn_df e guarda o resultado na variável res
  # Sobre a função: Agrupamento hierárquico aglomerativo baseado em critérios de 
  # máxima verossimilhança para modelos de mistura gaussiana parametrizados por 
  # decomposição de autovalores.
  ###################################################################################
  # O que eu entendi: está criando a RB com o algoritmo "max-min hill climbing", 
  # que é um algoritmo guloso.
  ###################################################################################
  #
  res = hc(base_treinamento, whitelist = df_white, blacklist = df_black)

  ###################################################################################
  # Grava o Plot das dependências funcionais entre as variáveis
  ###################################################################################
  #
  
  nomeArquivoImagem = paste(NomeExperimento,"_imagem.png",sep="",collapse="")
  if(!file.exists(nomeArquivoImagem)){
    png(file = nomeArquivoImagem)
    plot(res)
    dev.off()
  }
  
  ###################################################################################
  # treinar a rede
  # Depois de aprender a estrutura, é preciso descobrir as tabelas de probabilidade
  # condicional (CPTs) em cada nó. A função bn.fit executa o algoritmo EM para 
  # aprender CPT para diferentes nós no gráfico acima.
  ###################################################################################
  #
  #cat("treinar a rede\n")
  fittedbn <- bn.fit(res, data = base_treinamento)


  ###################################################################################
  # Explorando um dos cálculos
  # Por exemplo, vamos ver o que está dentro do nó s1
  ###################################################################################
  #
  #cat("explorando um dos cálculos\n")
  # fittedbn é um objeto da classe list
  #print(fittedbn)
  # s1 
  #print(fittedbn$s1)

  ###################################################################################
  # insere nulos
  ###################################################################################
  #
  porcentagem = furos/100
  
  #cat("Porcentagem=",porcentagem,"\n")
  
  nlin <- nrow(base_teste)
  ncol <- ncol(base_teste)
  totalelementos <- nlin * ncol
  porc_aleatorio <- totalelementos * porcentagem
  
  if(porc_aleatorio<1){
    porc_aleatorio=1
  }
  
  aleatorios <- sort(sample(1:totalelementos, porc_aleatorio))
  
  for (i in 1:porc_aleatorio){
    substituir <- aleatorios[i]
    linsub     <- (substituir %/% ncol)
    colsub     <- (substituir %% ncol)
    if(colsub==0){
      colsub <- ncol
    }
    base_teste[linsub,colsub] <- 'NULL'
  }
  
  #cat("Inseriu nulos\n")  
  
  
  ###################################################################################
  # Verifica se deu linha 100% nula
  ###################################################################################
  #
  contaNulo <- 0
  for (i in 1:nlin){
    for(j in 1:ncol){
      if (is.na  (base_teste[i,j]) || 
          is.null(base_teste[i,j]) ||
          (base_teste[i,j]=="NULL")){
        contaNulo = contaNulo + 1
      }
    }
    if(contaNulo==ncol){
      print("tudo nulo, não da pra fazer a predição")
      contaNulo <- 0 # 24/07/2020 
    #stop("tudo nulo, não da pra fazer a predição")
	}
    else
      contaNulo <- 0
  }
  #cat("Verificou linhas todas de buracos")

  ###################################################################################
  # Conta total de buracos na base
  # Conta total de linhas sem buracos
  # Conta total de linhas com buracos
  ###################################################################################
  #
  contadorDeBuracos    <- 0
  qtdlinhasSemBuracos  <- 0
  qtdlinhasComBuracos  <- 0
  
  for (i in 1:nlin){
    contAux=0
    for(j in 1:ncol){
      if (is.na  (base_teste[i,j]) || 
          is.null(base_teste[i,j]) ||
          (base_teste[i,j]=="NULL")){
        contAux=contAux+1
        contadorDeBuracos = contadorDeBuracos + 1
      }
    }
    if(contAux==0)
      qtdlinhasSemBuracos = qtdlinhasSemBuracos + 1
    else
      qtdlinhasComBuracos = qtdlinhasComBuracos + 1
  }
  
  #cat("Contou linhas e buracos\n")  
  
  ###################################################################################
  # informa valores para as colunas e suas categorias
  # O ideal seria vir de algum arquivo...
  ###################################################################################
  #
  valores <- list(c('0', '1'),
                  c('0', '1'),
                  c('0', '1'),
                  c('0', '1'),
                  c('0', '1'),
                  c('0', '1'),
                  c('0', '1'),
                  c('0', '1'),
                  c('0', '1'),
                  c('0', '1'),
                  c('0', '1'),
                  c('0', '1'),
                  c('0', '1'),
                  c('0', '1'),
                  c('0', '1'),
                  c('0', '1'))
  
  categorias <- list(c('en','mi','gtb','mq1','ls','mq2','las','outFog','aa1','iot','mo','ori','ql','cdb','aa2','outCloud'))
  
  ###################################################################################
  # inicialização dos contadores gerais do processamento
  ###################################################################################
  #
  
  contadorDeBuracosErradosPrimeira  <- 0
  contadorDeBuracosErradosSegunda   <- 0
  contadorDeBuracosErradosTerceira  <- 0
  qtdAcertouTudoPrimeira            <- 0
  qtdAcertouTudoSegunda             <- 0
  qtdAcertouTudoTerceira            <- 0

  contadorAcertouenPrimeira         <- 0
  contadorAcertouenSegunda          <- 0
  contadorAcertouenTerceira         <- 0
  contadorAcertoumiPrimeira         <- 0
  contadorAcertoumiSegunda          <- 0
  contadorAcertoumiTerceira         <- 0
  contadorAcertougtbPrimeira        <- 0
  contadorAcertougtbSegunda         <- 0
  contadorAcertougtbTerceira        <- 0
  contadorAcertoumq1Primeira        <- 0
  contadorAcertoumq1Segunda         <- 0
  contadorAcertoumq1Terceira        <- 0
  contadorAcertoulsPrimeira         <- 0
  contadorAcertoulsSegunda          <- 0
  contadorAcertoulsTerceira         <- 0
  contadorAcertoumq2Primeira        <- 0
  contadorAcertoumq2Segunda         <- 0
  contadorAcertoumq2Terceira        <- 0
  contadorAcertoulasPrimeira        <- 0
  contadorAcertoulasSegunda         <- 0
  contadorAcertoulasTerceira        <- 0
  
  contadorAcertououtFogPrimeira        <- 0
  contadorAcertououtFogSegunda         <- 0
  contadorAcertououtFogTerceira        <- 0
  
  contadorAcertouaa1Primeira        <- 0
  contadorAcertouaa1Segunda         <- 0
  contadorAcertouaa1Terceira        <- 0
  contadorAcertouiotPrimeira        <- 0
  contadorAcertouiotSegunda         <- 0
  contadorAcertouiotTerceira        <- 0
  contadorAcertoumoPrimeira         <- 0
  contadorAcertoumoSegunda          <- 0
  contadorAcertoumoTerceira         <- 0
  contadorAcertouoriPrimeira        <- 0
  contadorAcertouoriSegunda         <- 0
  contadorAcertouoriTerceira        <- 0
  contadorAcertouqlPrimeira         <- 0
  contadorAcertouqlSegunda          <- 0
  contadorAcertouqlTerceira         <- 0
  contadorAcertoucdbPrimeira        <- 0
  contadorAcertoucdbSegunda         <- 0
  contadorAcertoucdbTerceira        <- 0
  contadorAcertouaa2Primeira        <- 0
  contadorAcertouaa2Segunda         <- 0
  contadorAcertouaa2Terceira        <- 0
  
  contadorAcertououtCloudPrimeira        <- 0
  contadorAcertououtCloudSegunda         <- 0
  contadorAcertououtCloudTerceira        <- 0
  

  contadorErrouenPrimeira           <- 0
  contadorErrouenSegunda            <- 0
  contadorErrouenTerceira           <- 0
  contadorErroumiPrimeira           <- 0
  contadorErroumiSegunda            <- 0
  contadorErroumiTerceira           <- 0
  contadorErrougtbPrimeira          <- 0
  contadorErrougtbSegunda           <- 0
  contadorErrougtbTerceira          <- 0
  contadorErroumq1Primeira          <- 0
  contadorErroumq1Segunda           <- 0
  contadorErroumq1Terceira          <- 0
  contadorErroulsPrimeira           <- 0
  contadorErroulsSegunda            <- 0
  contadorErroulsTerceira           <- 0
  contadorErroumq2Primeira          <- 0
  contadorErroumq2Segunda           <- 0
  contadorErroumq2Terceira          <- 0
  contadorErroulasPrimeira          <- 0
  contadorErroulasSegunda           <- 0
  contadorErroulasTerceira          <- 0

  contadorErrououtFogPrimeira          <- 0
  contadorErrououtFogSegunda           <- 0
  contadorErrououtFogTerceira          <- 0

  contadorErrouaa1Primeira          <- 0
  contadorErrouaa1Segunda           <- 0
  contadorErrouaa1Terceira          <- 0
  contadorErrouiotPrimeira          <- 0
  contadorErrouiotSegunda           <- 0
  contadorErrouiotTerceira          <- 0
  contadorErroumoPrimeira           <- 0
  contadorErroumoSegunda            <- 0
  contadorErroumoTerceira           <- 0
  contadorErrouoriPrimeira          <- 0
  contadorErrouoriSegunda           <- 0
  contadorErrouoriTerceira          <- 0
  contadorErrouqlPrimeira           <- 0
  contadorErrouqlSegunda            <- 0
  contadorErrouqlTerceira           <- 0
  contadorErroucdbPrimeira          <- 0
  contadorErroucdbSegunda           <- 0
  contadorErroucdbTerceira          <- 0
  contadorErrouaa2Primeira          <- 0
  contadorErrouaa2Segunda           <- 0
  contadorErrouaa2Terceira          <- 0
  
  contadorErrououtCloudPrimeira          <- 0
  contadorErrououtCloudSegunda           <- 0
  contadorErrououtCloudTerceira          <- 0
  
  ###################################################################################
  # Laço principal para todas as instâncias
  # do arquivo
  ###################################################################################
  #
  for(i1 in 1:nlin){
    #cat("***Laco Principal - processando linha ", i1, "\n")
    dado <- base_teste[i1,]
    
    #####################################################
    # Verificando se a linha tem buracos
    # se não tiver então pular para a próxima linha
    #####################################################
    qtdBuracosLinha = 0
    
    for (i2 in 1:length(dado)){
      if (is.na(dado[i2]) ||
          is.null(dado[i2]) ||
          (dado[i2]=="NULL"))
        qtdBuracosLinha = qtdBuracosLinha + 1        
    }
    
    
    if(qtdBuracosLinha==0 ||
       qtdBuracosLinha==length(dado) ||
       qtdBuracosLinha/length(dado) > 0.75){
      #não precisa fazer queries ... então ir para o próximo
      #print ("Linha sem buracos... pulada")
      next
    }
    
    
    ##########################################
    # monta as queries
    ##########################################
    
    # limpando as variáveis
    dfv1 <- NULL
    dfv2 <- NULL
    dadoCopia <- NULL
    
    # fazendo uma cópia do dado (dataframe)
    dadoCopia <- dado
    
    # guardando o nome das colunas do dado (dataframe)
    nomeColunas <- colnames(dado)
    
    flag <- TRUE
    for (i2 in length(dado):1)
      if (is.na(dado[i2]) ||
          is.null(dado[i2]) ||
          (dado[i2]=="NULL"))
      {
        dadoCopia <- dadoCopia[,-c(i2)]
        if(flag)
        {
          flag <- FALSE
          dfV1 <- data.frame(valores[i2])
          names(dfV1) <- nomeColunas[i2]
        } else {
          dfV2 <- data.frame(valores[i2])
          names(dfV2) <- nomeColunas[i2]
          dfV1 <- cartesianExpand(dfV1, dfV2)
        }
      }
    
    queries <- as.data.frame(cartesianExpand(dadoCopia, dfV1))
    
    # obs: valores nulos vão no evento e os não nulos nas evidencias
    strEventoColuna    <- NULL
    strEvidenciaColuna <- NULL
    strEventoValor     <- NULL
    strEvidenciaValor  <- NULL
    
    contadorNull = 0
    for (i3 in length(dado):1)
      if (is.na(dado[i3]) ||
          is.null(dado[i3]) ||
          (dado[i3]=="NULL")){
        contadorNull = contadorNull + 1
        strEventoColuna <- c(nomeColunas[i3],strEventoColuna)
      } else {
        strEvidenciaColuna <- c(nomeColunas[i3],strEvidenciaColuna)
        strEvidenciaValor <- c((as.character(dado[1,i3])),strEvidenciaValor)
      }
    
    # devo pegar as três maiores probabilidades
    maiorProbabilidade = -1
    segundaMaiorProbabilidade = -1
    terceiraMaiorProbabilidade = -1
    
    queryMaiorProbabilidade = NULL
    querySegundaMaiorProbabilidade = NULL
    queryTerceiraMaiorProbabilidade = NULL
    
    indiceMaior = -1
    indiceSegundoMaior = -1
    indiceTerceiroMaior = -1
    
    #cat("nrow(queries) ===> ", nrow(queries), "\n")
    
    
    # percorrendo cada linha das queries
    for(i4 in 1:nrow(queries)){
      for(i5 in (length(dado)-contadorNull+1):(length(dado))){
        strEventoValor <- c((as.character(queries[i4,i5])),strEventoValor)
      }
      
      str1 = paste("(", strEventoColuna   , " == \"",strEventoValor   , "\")", sep = "", collapse = " & ")
      str2 = paste("(", strEvidenciaColuna, " == \"",strEvidenciaValor, "\")", sep = "")
      
      cmd = paste("cpquery(fittedbn, event = ", str1, ", evidence = ", str2, ")", sep = "")
      probabilidade = eval(parse(text = cmd))
      
      #cat("probabilidade===>", probabilidade, "\n")
      
      
      #pegando as três maiores probabilidades
      if(probabilidade > maiorProbabilidade){
        # a segunda passa a ser a terceira
        terceiraMaiorProbabilidade = segundaMaiorProbabilidade
        queryTerceiraMaiorProbabilidade = querySegundaMaiorProbabilidade
        indiceTerceiroMaior = indiceSegundoMaior
        
        # a primeira passa a ser a segunda
        segundaMaiorProbabilidade = maiorProbabilidade
        querySegundaMaiorProbabilidade = queryMaiorProbabilidade
        indiceSegundoMaior = indiceMaior
        
        # a nova passa a ser a primeira
        maiorProbabilidade = probabilidade
        queryMaiorProbabilidade = queries[i4,]
        indiceMaior = i4
      } else {
        if(probabilidade > segundaMaiorProbabilidade){
          # a segunda passa a ser a terceira
          terceiraMaiorProbabilidade = segundaMaiorProbabilidade
          queryTerceiraMaiorProbabilidade = querySegundaMaiorProbabilidade
          indiceTerceiroMaior = indiceSegundoMaior
          
          # a nova passa a ser a segunda
          segundaMaiorProbabilidade = probabilidade
          querySegundaMaiorProbabilidade = queries[i4,]
          indiceSegundoMaior = i4
        } else {
          if(probabilidade > terceiraMaiorProbabilidade){
            # a nova passa a ser a terceira
            terceiraMaiorProbabilidade = probabilidade
            queryTerceiraMaiorProbabilidade = queries[i4,]
            indiceTerceiraMaior = i4
          }
        }
      }
      
      strEventoValor <- NULL
    } # for i4
    
    
    ###################################################################################
    #  
    #  __   
    # /_ |  
    #  | |              Analisando os acertos e erros de primeira
    #  | | 
    #  | |
    #  |_|
    #
    ###################################################################################
    if(maiorProbabilidade>-1){
      #########################################################
      #cat ("Maior Probabiliade = ", maiorProbabilidade,"\n")
      #print ("Linha com buracos  = ")
      #print (dado[,order(names(dado))])
      #########################################################    
      
      ## auxiliar para identificar os buracos originais
      aux1 <- dado[,order(names(dado))]
      aux2 <- colnames(aux1)
      
      #########################################################
      #print ("Query selecionada  = ")
      #print (queryMaiorProbabilidade[,order(names(queryMaiorProbabilidade))])
      #print ("Linha sem buracos  = ")
      #print (base_teste_sem_buraco[i1,order(names(bn_df))])
      #########################################################
      contadorDiferencasPrimeira = 0
      ContadorIgualdadesPrimeira = 0
      
      # AA guarda a query com os buracos preenchidos com os valores de maior probabilidade
      AA <- queryMaiorProbabilidade[,order(names(queryMaiorProbabilidade))]
      # BB guarda a linha com os valores originais para verificar se acertou ou não
      BB <- base_teste_sem_buraco[i1,order(names(bn_df))]
      
      for(ww in 1:length(dado)){
        if(as.character(AA[,ww]) == as.character(BB[,ww])){
          if(is.na(aux1[ww]) || is.null(aux1[ww]) || (aux1[ww]=="NULL")){
            ## acertou um buraco
            if (aux2[ww] == "en"){contadorAcertouenPrimeira = contadorAcertouenPrimeira + 1} 
            else
              if (aux2[ww] == "mi"){contadorAcertoumiPrimeira = contadorAcertoumiPrimeira + 1}
            else
              if(aux2[ww] == "gtb"){contadorAcertougtbPrimeira = contadorAcertougtbPrimeira + 1}
            else 
              if(aux2[ww] == "mq1"){contadorAcertoumq1Primeira = contadorAcertoumq1Primeira + 1}
            else 
              if(aux2[ww] == "ls"){contadorAcertoulsPrimeira = contadorAcertoulsPrimeira + 1}
            else 
              if(aux2[ww] == "mq2"){contadorAcertoumq2Primeira = contadorAcertoumq2Primeira + 1}
            else
              if(aux2[ww] == "las"){contadorAcertoulasPrimeira = contadorAcertoulasPrimeira + 1}
            else
              if(aux2[ww] == "oufFog"){contadorAcertououtFogPrimeira = contadorAcertououtFogPrimeira + 1}
            else
              if(aux2[ww] == "aa1"){contadorAcertouaa1Primeira = contadorAcertouaa1Primeira + 1}
            else
              if(aux2[ww] == "iot"){contadorAcertouiotPrimeira = contadorAcertouiotPrimeira + 1}
            else
              if(aux2[ww] == "mo"){contadorAcertoumoPrimeira = contadorAcertoumoPrimeira + 1}
            else
              if(aux2[ww] == "ori"){contadorAcertouoriPrimeira = contadorAcertouoriPrimeira + 1}
            else
              if(aux2[ww] == "ql"){contadorAcertouqlPrimeira = contadorAcertouqlPrimeira + 1}
            else
              if(aux2[ww] == "cdb"){contadorAcertoucdbPrimeira = contadorAcertoucdbPrimeira + 1}
            else
              if(aux2[ww] == "aa2"){contadorAcertouaa2Primeira = contadorAcertouaa2Primeira + 1}
            else
              if(aux2[ww] == "outCloud"){contadorAcertououtCloudPrimeira = contadorAcertououtCloudPrimeira + 1}
          }
          
          ContadorIgualdadesPrimeira <- ContadorIgualdadesPrimeira + 1
        } else {
          if(is.na(aux1[ww]) || is.null(aux1[ww]) || (aux1[ww]=="NULL")){
            ## errou um buraco
            if (aux2[ww] == "en"){contadorErrouenPrimeira = contadorErrouenPrimeira + 1} 
            else
              if (aux2[ww] == "mi"){contadorErroumiPrimeira = contadorErroumiPrimeira + 1}
            else
              if(aux2[ww] == "gtb"){contadorErrougtbPrimeira = contadorErrougtbPrimeira + 1}
            else 
              if(aux2[ww] == "mq1"){contadorErroumq1Primeira = contadorErroumq1Primeira + 1}
            else 
              if(aux2[ww] == "ls"){contadorErroulsPrimeira = contadorErroulsPrimeira + 1}
            else 
              if(aux2[ww] == "mq2"){contadorErroumq2Primeira = contadorErroumq2Primeira + 1}
            else
              if(aux2[ww] == "las"){contadorErroulasPrimeira = contadorErroulasPrimeira + 1}
            else
              if(aux2[ww] == "outFog"){contadorErrououtFogPrimeira = contadorErrououtFogPrimeira + 1}
            else
              if(aux2[ww] == "aa1"){contadorErrouaa1Primeira = contadorErrouaa1Primeira + 1}
            else
              if(aux2[ww] == "iot"){contadorErrouiotPrimeira = contadorErrouiotPrimeira + 1}
            else
              if(aux2[ww] == "mo"){contadorErroumoPrimeira = contadorErroumoPrimeira + 1}
            else
              if(aux2[ww] == "ori"){contadorErrouoriPrimeira = contadorErrouoriPrimeira + 1}
            else
              if(aux2[ww] == "ql"){contadorErrouqlPrimeira = contadorErrouqlPrimeira + 1}
            else
              if(aux2[ww] == "cdb"){contadorErroucdbPrimeira = contadorErroucdbPrimeira + 1}
            else
              if(aux2[ww] == "aa2"){contadorErrouaa2Primeira = contadorErrouaa2Primeira + 1}
            else
              if(aux2[ww] == "outCloud"){contadorErrououtCloudPrimeira = contadorErrououtCloudPrimeira + 1}
            
          }
          contadorDiferencasPrimeira <- contadorDiferencasPrimeira + 1
          contadorDeBuracosErradosPrimeira = contadorDeBuracosErradosPrimeira + 1
          #########################################################        
          #cat("Q: ",colnames(AA[ww]),"...:",as.character(AA[,ww]),"\t",
          #    "B: ",colnames(BB[ww]),"...:",as.character(BB[,ww]),"\n")
          #########################################################        
        }
      } # for ww
      
      #########################################################    
      #cat ("Qtde de Buracos certos  Primeira  = ", (qtdBuracosLinha-contadorDiferencasPrimeira), "\n")
      #cat ("Qtde de Buracos errados Primeira  = ", contadorDiferencasPrimeira, "\n")
      #cat ("Qtde de Buracos                   = ", qtdBuracosLinha, "\n")
      #########################################################    
      
      if(contadorDiferencasPrimeira==0)
        qtdAcertouTudoPrimeira = qtdAcertouTudoPrimeira + 1    
      
    } # if(maiorProbabilidade>-1)
    
    
    ###################################################################################
    #  
    #  ___  
    # |__ \
    #    ) |            Analisando os acertos e erros de Segunda
    #   / / 
    #  / /_ 
    # |____|
    #
    ###################################################################################
    if(segundaMaiorProbabilidade>-1){
      #########################################################
      #cat ("Segunda Maior Probabiliade = ", segundaMaiorProbabilidade,"\n")
      #print ("Linha com buracos  = ")
      #print (dado[,order(names(dado))])
      #########################################################    
      
      ## auxiliar para identificar os buracos originais
      aux1 <- dado[,order(names(dado))]
      aux2 <- colnames(aux1)
      
      #########################################################
      #print ("Query selecionada  = ")
      #print (querySegundaMaiorProbabilidade[,order(names(querySegundaMaiorProbabilidade))])
      #print ("Linha sem buracos  = ")
      #print (base_teste_sem_buraco[i1,order(names(bn_df))])
      #########################################################
      contadorDiferencasSegunda = 0
      ContadorIgualdadesSegunda = 0
      
      # AA guarda a query com os buracos preenchidos com os valores de maior probabilidade
      AA <- querySegundaMaiorProbabilidade[,order(names(querySegundaMaiorProbabilidade))]
      # BB guarda a linha com os valores originais para verificar se acertou ou não
      BB <- base_teste_sem_buraco[i1,order(names(bn_df))]
      
      for(ww in 1:length(dado)){
        if(as.character(AA[,ww]) == as.character(BB[,ww])){
          if(is.na(aux1[ww]) || is.null(aux1[ww]) || (aux1[ww]=="NULL")){
            ## acertou um buraco
            if (aux2[ww] == "en"){contadorAcertouenSegunda = contadorAcertouenSegunda + 1} 
            else
              if (aux2[ww] == "mi"){contadorAcertoumiSegunda = contadorAcertoumiSegunda + 1}
            else
              if(aux2[ww] == "gtb"){contadorAcertougtbSegunda = contadorAcertougtbSegunda + 1}
            else 
              if(aux2[ww] == "mq1"){contadorAcertoumq1Segunda = contadorAcertoumq1Segunda + 1}
            else 
              if(aux2[ww] == "ls"){contadorAcertoulsSegunda = contadorAcertoulsSegunda + 1}
            else 
              if(aux2[ww] == "mq2"){contadorAcertoumq2Segunda = contadorAcertoumq2Segunda + 1}
            else
              if(aux2[ww] == "las"){contadorAcertoulasSegunda = contadorAcertoulasSegunda + 1}
            else
              if(aux2[ww] == "outFog"){contadorAcertououtFogSegunda = contadorAcertououtFogSegunda + 1}
            else
              if(aux2[ww] == "aa1"){contadorAcertouaa1Segunda = contadorAcertouaa1Segunda + 1}
            else
              if(aux2[ww] == "iot"){contadorAcertouiotSegunda = contadorAcertouiotSegunda + 1}
            else
              if(aux2[ww] == "mo"){contadorAcertoumoSegunda = contadorAcertoumoSegunda + 1}
            else
              if(aux2[ww] == "ori"){contadorAcertouoriSegunda = contadorAcertouoriSegunda + 1}
            else
              if(aux2[ww] == "ql"){contadorAcertouqlSegunda = contadorAcertouqlSegunda + 1}
            else
              if(aux2[ww] == "cdb"){contadorAcertoucdbSegunda = contadorAcertoucdbSegunda + 1}
            else
              if(aux2[ww] == "aa2"){contadorAcertouaa2Segunda = contadorAcertouaa2Segunda + 1}
            else
              if(aux2[ww] == "outCloud"){contadorAcertououtCloudSegunda = contadorAcertououtCloudSegunda + 1}
          }
          
          ContadorIgualdadesSegunda <- ContadorIgualdadesSegunda + 1
        } else {
          if(is.na(aux1[ww]) || is.null(aux1[ww]) || (aux1[ww]=="NULL")){
            ## errou um buraco
            if (aux2[ww] == "en"){contadorErrouenSegunda = contadorErrouenSegunda + 1} 
            else
              if (aux2[ww] == "mi"){contadorErroumiSegunda = contadorErroumiSegunda + 1}
            else
              if(aux2[ww] == "gtb"){contadorErrougtbSegunda = contadorErrougtbSegunda + 1}
            else 
              if(aux2[ww] == "mq1"){contadorErroumq1Segunda = contadorErroumq1Segunda + 1}
            else 
              if(aux2[ww] == "ls"){contadorErroulsSegunda = contadorErroulsSegunda + 1}
            else 
              if(aux2[ww] == "mq2"){contadorErroumq2Segunda = contadorErroumq2Segunda + 1}
            else
              if(aux2[ww] == "las"){contadorErroulasSegunda = contadorErroulasSegunda + 1}
            else
              if(aux2[ww] == "outFog"){contadorErrououtFogSegunda = contadorErrououtFogSegunda + 1}
            else
              if(aux2[ww] == "aa1"){contadorErrouaa1Segunda = contadorErrouaa1Segunda + 1}
            else
              if(aux2[ww] == "iot"){contadorErrouiotSegunda = contadorErrouiotSegunda + 1}
            else
              if(aux2[ww] == "mo"){contadorErroumoSegunda = contadorErroumoSegunda + 1}
            else
              if(aux2[ww] == "ori"){contadorErrouoriSegunda = contadorErrouoriSegunda + 1}
            else
              if(aux2[ww] == "ql"){contadorErrouqlSegunda = contadorErrouqlSegunda + 1}
            else
              if(aux2[ww] == "cdb"){contadorErroucdbSegunda = contadorErroucdbSegunda + 1}
            else
              if(aux2[ww] == "aa2"){contadorErrouaa2Segunda = contadorErrouaa2Segunda + 1}
            else
              if(aux2[ww] == "outCloud"){contadorErrououtCloudSegunda = contadorErrououtCloudSegunda + 1}
          }
          contadorDiferencasSegunda <- contadorDiferencasSegunda + 1
          contadorDeBuracosErradosSegunda = contadorDeBuracosErradosSegunda + 1
          #########################################################        
          #cat("Q: ",colnames(AA[ww]),"...:",as.character(AA[,ww]),"\t",
          #    "B: ",colnames(BB[ww]),"...:",as.character(BB[,ww]),"\n")
          #########################################################        
        }
      } # for ww
      
      
      #########################################################    
      #cat ("Qtde de Buracos certos  Segunda = ", (qtdBuracosLinha-contadorDiferencasSegunda), "\n")
      #cat ("Qtde de Buracos errados Segunda = ", contadorDiferencasSegunda, "\n")
      #cat ("Qtde de Buracos                 = ", qtdBuracosLinha, "\n")
      #########################################################    
      
      if(contadorDiferencasSegunda==0)
        qtdAcertouTudoSegunda = qtdAcertouTudoSegunda + 1    
      
    } # if(segundaMaiorProbabilidade>-1)
    
    ###################################################################################
    #  
    #  ____  
    # |___ \   
    #   __) |           Analisando os acertos e erros de Terceira
    #  |__ < 
    #  ___) |
    # |____/
    #
    ###################################################################################
    if(terceiraMaiorProbabilidade>-1){
      #########################################################
      #cat ("Terceira Maior Probabiliade = ", terceiraMaiorProbabilidade,"\n")
      #print ("Linha com buracos  = ")
      #print (dado[,order(names(dado))])
      #########################################################    
      
      ## auxiliar para identificar os buracos originais
      aux1 <- dado[,order(names(dado))]
      aux2 <- colnames(aux1)
      
      #########################################################
      #print ("Query selecionada  = ")
      #print (queryTerceiraMaiorProbabilidade[,order(names(queryTerceiraMaiorProbabilidade))])
      #print ("Linha sem buracos  = ")
      #print (base_teste_sem_buraco[i1,order(names(bn_df))])
      #########################################################
      contadorDiferencasTerceira = 0
      ContadorIgualdadesTerceira = 0
      
      # AA guarda a query com os buracos preenchidos com os valores de maior probabilidade
      AA <- queryTerceiraMaiorProbabilidade[,order(names(queryTerceiraMaiorProbabilidade))]
      # BB guarda a linha com os valores originais para verificar se acertou ou não
      BB <- base_teste_sem_buraco[i1,order(names(bn_df))]
      
      for(ww in 1:length(dado)){
        if(as.character(AA[,ww]) == as.character(BB[,ww])){
          if(is.na(aux1[ww]) || is.null(aux1[ww]) || (aux1[ww]=="NULL")){
            ## acertou um buraco
            if (aux2[ww] == "en"){contadorAcertouenTerceira = contadorAcertouenTerceira + 1} 
            else
              if (aux2[ww] == "mi"){contadorAcertoumiTerceira = contadorAcertoumiTerceira + 1}
            else
              if(aux2[ww] == "gtb"){contadorAcertougtbTerceira = contadorAcertougtbTerceira + 1}
            else 
              if(aux2[ww] == "mq1"){contadorAcertoumq1Terceira = contadorAcertoumq1Terceira + 1}
            else 
              if(aux2[ww] == "ls"){contadorAcertoulsTerceira = contadorAcertoulsTerceira + 1}
            else 
              if(aux2[ww] == "mq2"){contadorAcertoumq2Terceira = contadorAcertoumq2Terceira + 1}
            else
              if(aux2[ww] == "las"){contadorAcertoulasTerceira = contadorAcertoulasTerceira + 1}
            else
              if(aux2[ww] == "outFog"){contadorAcertououtFogTerceira = contadorAcertououtFogTerceira + 1}
            else
              if(aux2[ww] == "aa1"){contadorAcertouaa1Terceira = contadorAcertouaa1Terceira + 1}
            else
              if(aux2[ww] == "iot"){contadorAcertouiotTerceira = contadorAcertouiotTerceira + 1}
            else
              if(aux2[ww] == "mo"){contadorAcertoumoTerceira = contadorAcertoumoTerceira + 1}
            else
              if(aux2[ww] == "ori"){contadorAcertouoriTerceira = contadorAcertouoriTerceira + 1}
            else
              if(aux2[ww] == "ql"){contadorAcertouqlTerceira = contadorAcertouqlTerceira + 1}
            else
              if(aux2[ww] == "cdb"){contadorAcertoucdbTerceira = contadorAcertoucdbTerceira + 1}
            else
              if(aux2[ww] == "aa2"){contadorAcertouaa2Terceira = contadorAcertouaa2Terceira + 1}
            else
              if(aux2[ww] == "outCloud"){contadorAcertououtCloudTerceira = contadorAcertououtCloudTerceira + 1}
          }
          
          ContadorIgualdadesTerceira <- ContadorIgualdadesTerceira + 1
        } else {
          if(is.na(aux1[ww]) || is.null(aux1[ww]) || (aux1[ww]=="NULL")){
            ## errou um buraco
            if (aux2[ww] == "en"){contadorErrouenTerceira = contadorErrouenTerceira + 1} 
            else
              if (aux2[ww] == "mi"){contadorErroumiTerceira = contadorErroumiTerceira + 1}
            else
              if(aux2[ww] == "gtb"){contadorErrougtbTerceira = contadorErrougtbTerceira + 1}
            else 
              if(aux2[ww] == "mq1"){contadorErroumq1Terceira = contadorErroumq1Terceira + 1}
            else 
              if(aux2[ww] == "ls"){contadorErroulsTerceira = contadorErroulsTerceira + 1}
            else 
              if(aux2[ww] == "mq2"){contadorErroumq2Terceira = contadorErroumq2Terceira + 1}
            else
              if(aux2[ww] == "las"){contadorErroulasTerceira = contadorErroulasTerceira + 1}
            else
              if(aux2[ww] == "outFog"){contadorErrououtFogTerceira = contadorErrououtFogTerceira + 1}
            else
              if(aux2[ww] == "aa1"){contadorErrouaa1Terceira = contadorErrouaa1Terceira + 1}
            else
              if(aux2[ww] == "iot"){contadorErrouiotTerceira = contadorErrouiotTerceira + 1}
            else
              if(aux2[ww] == "mo"){contadorErroumoTerceira = contadorErroumoTerceira + 1}
            else
              if(aux2[ww] == "ori"){contadorErrouoriTerceira = contadorErrouoriTerceira + 1}
            else
              if(aux2[ww] == "ql"){contadorErrouqlTerceira = contadorErrouqlTerceira + 1}
            else
              if(aux2[ww] == "cdb"){contadorErroucdbTerceira = contadorErroucdbTerceira + 1}
            else
              if(aux2[ww] == "aa2"){contadorErrouaa2Terceira = contadorErrouaa2Terceira + 1}
            else
              if(aux2[ww] == "outCloud"){contadorErrououtCloudTerceira = contadorErrououtCloudTerceira + 1}
          }
          contadorDiferencasTerceira <- contadorDiferencasTerceira + 1
          contadorDeBuracosErradosTerceira = contadorDeBuracosErradosTerceira + 1
          #########################################################        
          #cat("Q: ",colnames(AA[ww]),"...:",as.character(AA[,ww]),"\t",
          #    "B: ",colnames(BB[ww]),"...:",as.character(BB[,ww]),"\n")
          #########################################################        
        }
      } # for ww
      
      
      #########################################################    
      #cat ("Qtde de Buracos certos  Terceira = ", (qtdBuracosLinha-contadorDiferencasTerceira), "\n")
      #cat ("Qtde de Buracos errados Terceira = ", contadorDiferencasTerceira, "\n")
      #cat ("Qtde de Buracos                  = ", qtdBuracosLinha, "\n")
      #########################################################    
      
      if(contadorDiferencasTerceira==0)
        qtdAcertouTudoTerceira = qtdAcertouTudoTerceira + 1    
      
    } # if(terceiraMaiorProbabilidade>-1)
    
  } #  i1
  ##########################################
  # fim do Laço principal
  ##########################################

  # atualizando o dataframe com base no ArquivoResultado
  df_resultado <- as.data.frame(fromJSON(ArquivoResultado))
  
  #variaveis para guardar no dataframe de resultado de processamento
  df_resultado$totBuracos[linhaResultado] = contadorDeBuracos
  
  df_resultado$totBuracosAcertouPrimeira[linhaResultado] = (contadorDeBuracos-contadorDeBuracosErradosPrimeira)
  df_resultado$totBuracosAcertouSegunda[linhaResultado] = (contadorDeBuracos-contadorDeBuracosErradosSegunda)
  df_resultado$totBuracosAcertouTerceira[linhaResultado] = (contadorDeBuracos-contadorDeBuracosErradosTerceira)
  
  df_resultado$totBuracosErrouPrimeira[linhaResultado] = contadorDeBuracosErradosPrimeira
  df_resultado$totBuracosErrouSegunda[linhaResultado] = contadorDeBuracosErradosSegunda
  df_resultado$totBuracosErrouTerceira[linhaResultado] = contadorDeBuracosErradosTerceira
  
  df_resultado$predicoes100CorretasPrimeira[linhaResultado] = qtdAcertouTudoPrimeira
  df_resultado$predicoes100CorretasSegunda[linhaResultado] = qtdAcertouTudoSegunda
  df_resultado$predicoes100CorretasTerceira[linhaResultado] = qtdAcertouTudoTerceira
  
  df_resultado$acertosenPrimeira[linhaResultado] =  contadorAcertouenPrimeira     
  df_resultado$acertosenSegunda[linhaResultado]  =  contadorAcertouenSegunda      
  df_resultado$acertosenTerceira[linhaResultado] =  contadorAcertouenTerceira     
  
  df_resultado$acertosmiPrimeira[linhaResultado] =  contadorAcertoumiPrimeira     
  df_resultado$acertosmiSegunda[linhaResultado]  =  contadorAcertoumiSegunda      
  df_resultado$acertosmiTerceira[linhaResultado] =  contadorAcertoumiTerceira     
  
  df_resultado$acertosgtbPrimeira[linhaResultado] = contadorAcertougtbPrimeira    
  df_resultado$acertosgtbSegunda[linhaResultado]  = contadorAcertougtbSegunda     
  df_resultado$acertosgtbTerceira[linhaResultado] = contadorAcertougtbTerceira    
  
  df_resultado$acertosmq1Primeira[linhaResultado] = contadorAcertoumq1Primeira    
  df_resultado$acertosmq1Segunda[linhaResultado]  = contadorAcertoumq1Segunda     
  df_resultado$acertosmq1Terceira[linhaResultado] = contadorAcertoumq1Terceira    
  
  df_resultado$acertoslsPrimeira[linhaResultado] =  contadorAcertoulsPrimeira     
  df_resultado$acertoslsSegunda[linhaResultado]  =  contadorAcertoulsSegunda      
  df_resultado$acertoslsTerceira[linhaResultado] =  contadorAcertoulsTerceira     
  
  df_resultado$acertosmq2Primeira[linhaResultado] = contadorAcertoumq2Primeira    
  df_resultado$acertosmq2Segunda[linhaResultado]  = contadorAcertoumq2Segunda     
  df_resultado$acertosmq2Terceira[linhaResultado] = contadorAcertoumq2Terceira    
  
  df_resultado$acertoslasPrimeira[linhaResultado] = contadorAcertoulasPrimeira    
  df_resultado$acertoslasSegunda[linhaResultado]  = contadorAcertoulasSegunda     
  df_resultado$acertoslasTerceira[linhaResultado] = contadorAcertoulasTerceira    
  
  df_resultado$acertosoutFogPrimeira[linhaResultado] = contadorAcertououtFogPrimeira    
  df_resultado$acertosoutFogSegunda[linhaResultado]  = contadorAcertououtFogSegunda     
  df_resultado$acertosoutFogTerceira[linhaResultado] = contadorAcertououtFogTerceira    
  
  df_resultado$acertosaa1Primeira[linhaResultado] = contadorAcertouaa1Primeira    
  df_resultado$acertosaa1Segunda[linhaResultado]  = contadorAcertouaa1Segunda     
  df_resultado$acertosaa1Terceira[linhaResultado] = contadorAcertouaa1Terceira    
  
  df_resultado$acertosiotPrimeira[linhaResultado] = contadorAcertouiotPrimeira    
  df_resultado$acertosiotSegunda[linhaResultado]  = contadorAcertouiotSegunda     
  df_resultado$acertosiotTerceira[linhaResultado] = contadorAcertouiotTerceira    
  
  df_resultado$acertosmoPrimeira[linhaResultado] = contadorAcertoumoPrimeira     
  df_resultado$acertosmoSegunda[linhaResultado]  = contadorAcertoumoSegunda      
  df_resultado$acertosmoTerceira[linhaResultado] = contadorAcertoumoTerceira     
  
  df_resultado$acertosoriPrimeira[linhaResultado] = contadorAcertouoriPrimeira    
  df_resultado$acertosoriSegunda[linhaResultado]  = contadorAcertouoriSegunda     
  df_resultado$acertosoriTerceira[linhaResultado] = contadorAcertouoriTerceira    
  
  df_resultado$acertosqlPrimeira[linhaResultado] =  contadorAcertouqlPrimeira     
  df_resultado$acertosqlSegunda[linhaResultado]  =  contadorAcertouqlSegunda      
  df_resultado$acertosqlTerceira[linhaResultado] =  contadorAcertouqlTerceira     
  
  df_resultado$acertoscdbPrimeira[linhaResultado] = contadorAcertoucdbPrimeira    
  df_resultado$acertoscdbSegunda[linhaResultado]  = contadorAcertoucdbSegunda     
  df_resultado$acertoscdbTerceira[linhaResultado] = contadorAcertoucdbTerceira    
  
  df_resultado$acertosaa2Primeira[linhaResultado] = contadorAcertouaa2Primeira    
  df_resultado$acertosaa2Segunda[linhaResultado]  = contadorAcertouaa2Segunda     
  df_resultado$acertosaa2Terceira[linhaResultado] = contadorAcertouaa2Terceira
  
  df_resultado$acertosoutCloudPrimeira[linhaResultado] = contadorAcertououtCloudPrimeira    
  df_resultado$acertosoutCloudSegunda[linhaResultado]  = contadorAcertououtCloudSegunda     
  df_resultado$acertosoutCloudTerceira[linhaResultado] = contadorAcertououtCloudTerceira
  
  df_resultado$errosenPrimeira[linhaResultado] =  contadorErrouenPrimeira     
  df_resultado$errosenSegunda[linhaResultado]  =  contadorErrouenSegunda      
  df_resultado$errosenTerceira[linhaResultado] =  contadorErrouenTerceira     
  
  df_resultado$errosmiPrimeira[linhaResultado] =  contadorErroumiPrimeira     
  df_resultado$errosmiSegunda[linhaResultado]  =  contadorErroumiSegunda      
  df_resultado$errosmiTerceira[linhaResultado] =  contadorErroumiTerceira     
  
  df_resultado$errosgtbPrimeira[linhaResultado] = contadorErrougtbPrimeira    
  df_resultado$errosgtbSegunda[linhaResultado]  = contadorErrougtbSegunda     
  df_resultado$errosgtbTerceira[linhaResultado] = contadorErrougtbTerceira    
  
  df_resultado$errosmq1Primeira[linhaResultado] = contadorErroumq1Primeira    
  df_resultado$errosmq1Segunda[linhaResultado]  = contadorErroumq1Segunda     
  df_resultado$errosmq1Terceira[linhaResultado] = contadorErroumq1Terceira    
  
  df_resultado$erroslsPrimeira[linhaResultado] =  contadorErroulsPrimeira     
  df_resultado$erroslsSegunda[linhaResultado]  =  contadorErroulsSegunda      
  df_resultado$erroslsTerceira[linhaResultado] =  contadorErroulsTerceira     
  
  df_resultado$errosmq2Primeira[linhaResultado] = contadorErroumq2Primeira    
  df_resultado$errosmq2Segunda[linhaResultado]  = contadorErroumq2Segunda     
  df_resultado$errosmq2Terceira[linhaResultado] = contadorErroumq2Terceira    
  
  df_resultado$erroslasPrimeira[linhaResultado] = contadorErroulasPrimeira    
  df_resultado$erroslasSegunda[linhaResultado]  = contadorErroulasSegunda     
  df_resultado$erroslasTerceira[linhaResultado] = contadorErroulasTerceira    
  
  df_resultado$errosoutFogPrimeira[linhaResultado] = contadorErrououtFogPrimeira    
  df_resultado$errosoutFogSegunda[linhaResultado]  = contadorErrououtFogSegunda     
  df_resultado$errosoutFogTerceira[linhaResultado] = contadorErrououtFogTerceira    
  
  df_resultado$errosaa1Primeira[linhaResultado] = contadorErrouaa1Primeira    
  df_resultado$errosaa1Segunda[linhaResultado]  = contadorErrouaa1Segunda     
  df_resultado$errosaa1Terceira[linhaResultado] = contadorErrouaa1Terceira    
  
  df_resultado$errosiotPrimeira[linhaResultado] = contadorErrouiotPrimeira    
  df_resultado$errosiotSegunda[linhaResultado]  = contadorErrouiotSegunda     
  df_resultado$errosiotTerceira[linhaResultado] = contadorErrouiotTerceira    
  
  df_resultado$errosmoPrimeira[linhaResultado] = contadorErroumoPrimeira     
  df_resultado$errosmoSegunda[linhaResultado]  = contadorErroumoSegunda      
  df_resultado$errosmoTerceira[linhaResultado] = contadorErroumoTerceira     
  
  df_resultado$errosoriPrimeira[linhaResultado] = contadorErrouoriPrimeira    
  df_resultado$errosoriSegunda[linhaResultado]  = contadorErrouoriSegunda     
  df_resultado$errosoriTerceira[linhaResultado] = contadorErrouoriTerceira    
  
  df_resultado$errosqlPrimeira[linhaResultado] =  contadorErrouqlPrimeira     
  df_resultado$errosqlSegunda[linhaResultado]  =  contadorErrouqlSegunda      
  df_resultado$errosqlTerceira[linhaResultado] =  contadorErrouqlTerceira     
  
  df_resultado$erroscdbPrimeira[linhaResultado] = contadorErroucdbPrimeira    
  df_resultado$erroscdbSegunda[linhaResultado]  = contadorErroucdbSegunda     
  df_resultado$erroscdbTerceira[linhaResultado] = contadorErroucdbTerceira    
  
  df_resultado$errosaa2Primeira[linhaResultado] = contadorErrouaa2Primeira    
  df_resultado$errosaa2Segunda[linhaResultado]  = contadorErrouaa2Segunda     
  df_resultado$errosaa2Terceira[linhaResultado] = contadorErrouaa2Terceira
  
  df_resultado$errosoutCloudPrimeira[linhaResultado] = contadorErrououtCloudPrimeira    
  df_resultado$errosoutCloudSegunda[linhaResultado]  = contadorErrououtCloudSegunda     
  df_resultado$errosoutCloudTerceira[linhaResultado] = contadorErrououtCloudTerceira
  
  
  # o calculo abaixo pode ter denominador zero e gerar NaN - colocar em um if
  # end node => en
  if((contadorAcertouenPrimeira + contadorErrouenPrimeira)>0){
    df_resultado$porcAcertoenPrimeira[linhaResultado] = (contadorAcertouenPrimeira/(contadorAcertouenPrimeira + contadorErrouenPrimeira))*100.0    
  } else {
    df_resultado$porcAcertoenPrimeira[linhaResultado] = 0
  }
  
  if((contadorAcertouenSegunda + contadorErrouenSegunda)>0){
    df_resultado$porcAcertoenSegunda[linhaResultado] = (contadorAcertouenSegunda/(contadorAcertouenSegunda + contadorErrouenSegunda))*100.0    
  } else {
    df_resultado$porcAcertoenSegunda[linhaResultado] = 0
  }
  
  if((contadorAcertouenTerceira + contadorErrouenTerceira)>0){
    df_resultado$porcAcertoenTerceira[linhaResultado] = (contadorAcertouenTerceira/(contadorAcertouenTerceira + contadorErrouenTerceira))*100.0    
  } else {
    df_resultado$porcAcertoenTerceira[linhaResultado] = 0
  }
  
  # mist => mi
  if((contadorAcertoumiPrimeira + contadorErroumiPrimeira)>0){
    df_resultado$porcAcertomiPrimeira[linhaResultado] = (contadorAcertoumiPrimeira/(contadorAcertoumiPrimeira + contadorErroumiPrimeira))*100.0    
  } else {
    df_resultado$porcAcertomiPrimeira[linhaResultado] = 0
  }
  
  if((contadorAcertoumiSegunda + contadorErroumiSegunda)>0){
    df_resultado$porcAcertomiSegunda[linhaResultado] = (contadorAcertoumiSegunda/(contadorAcertoumiSegunda + contadorErroumiSegunda))*100.0    
  } else {
    df_resultado$porcAcertomiSegunda[linhaResultado] = 0
  }
  
  if((contadorAcertoumiTerceira + contadorErroumiTerceira)>0){
    df_resultado$porcAcertomiTerceira[linhaResultado] = (contadorAcertoumiTerceira/(contadorAcertoumiTerceira + contadorErroumiTerceira))*100.0    
  } else {
    df_resultado$porcAcertomiTerceira[linhaResultado] = 0
  }
  
  # gateway bridge => gtb
  if((contadorAcertougtbPrimeira + contadorErrougtbPrimeira)>0){
    df_resultado$porcAcertogtbPrimeira[linhaResultado] = (contadorAcertougtbPrimeira/(contadorAcertougtbPrimeira + contadorErrougtbPrimeira))*100.0    
  } else {
    df_resultado$porcAcertogtbPrimeira[linhaResultado] = 0
  }
  
  if((contadorAcertougtbSegunda + contadorErrougtbSegunda)>0){
    df_resultado$porcAcertogtbSegunda[linhaResultado] = (contadorAcertougtbSegunda/(contadorAcertougtbSegunda + contadorErrougtbSegunda))*100.0    
  } else {
    df_resultado$porcAcertogtbSegunda[linhaResultado] = 0
  }
  
  if((contadorAcertougtbTerceira + contadorErrougtbTerceira)>0){
    df_resultado$porcAcertogtbTerceira[linhaResultado] = (contadorAcertougtbTerceira/(contadorAcertougtbTerceira + contadorErrougtbTerceira))*100.0    
  } else {
    df_resultado$porcAcertogtbTerceira[linhaResultado] = 0
  }
  
  # mqqt1 => mq1
  if((contadorAcertoumq1Primeira + contadorErroumq1Primeira)>0){
    df_resultado$porcAcertomq1Primeira[linhaResultado] = (contadorAcertoumq1Primeira/(contadorAcertoumq1Primeira + contadorErroumq1Primeira))*100.0    
  } else {
    df_resultado$porcAcertomq1Primeira[linhaResultado] = 0
  }
  
  if((contadorAcertoumq1Segunda + contadorErroumq1Segunda)>0){
    df_resultado$porcAcertomq1Segunda[linhaResultado] = (contadorAcertoumq1Segunda/(contadorAcertoumq1Segunda + contadorErroumq1Segunda))*100.0    
  } else {
    df_resultado$porcAcertomq1Segunda[linhaResultado] = 0
  }
  
  if((contadorAcertoumq1Terceira + contadorErroumq1Terceira)>0){
    df_resultado$porcAcertomq1Terceira[linhaResultado] = (contadorAcertoumq1Terceira/(contadorAcertoumq1Terceira + contadorErroumq1Terceira))*100.0    
  } else {
    df_resultado$porcAcertomq1Terceira[linhaResultado] = 0
  }
  
  # Lora Server => ls
  if((contadorAcertoulsPrimeira + contadorErroulsPrimeira)>0){
    df_resultado$porcAcertolsPrimeira[linhaResultado] = (contadorAcertoulsPrimeira/(contadorAcertoulsPrimeira + contadorErroulsPrimeira))*100.0    
  } else {
    df_resultado$porcAcertolsPrimeira[linhaResultado] = 0
  }
  
  if((contadorAcertoulsSegunda + contadorErroulsSegunda)>0){
    df_resultado$porcAcertolsSegunda[linhaResultado] = (contadorAcertoulsSegunda/(contadorAcertoulsSegunda + contadorErroulsSegunda))*100.0    
  } else {
    df_resultado$porcAcertolsSegunda[linhaResultado] = 0
  }
  
  if((contadorAcertoulsTerceira + contadorErroulsTerceira)>0){
    df_resultado$porcAcertolsTerceira[linhaResultado] = (contadorAcertoulsTerceira/(contadorAcertoulsTerceira + contadorErroulsTerceira))*100.0    
  } else {
    df_resultado$porcAcertolsTerceira[linhaResultado] = 0
  }
  
  # mqtt2 => mq2
  if((contadorAcertoumq2Primeira + contadorErroumq2Primeira)>0){
    df_resultado$porcAcertomq2Primeira[linhaResultado] = (contadorAcertoumq2Primeira/(contadorAcertoumq2Primeira + contadorErroumq2Primeira))*100.0    
  } else {
    df_resultado$porcAcertomq2Primeira[linhaResultado] = 0
  }
  
  if((contadorAcertoumq2Segunda + contadorErroumq2Segunda)>0){
    df_resultado$porcAcertomq2Segunda[linhaResultado] = (contadorAcertoumq2Segunda/(contadorAcertoumq2Segunda + contadorErroumq2Segunda))*100.0    
  } else {
    df_resultado$porcAcertomq2Segunda[linhaResultado] = 0
  }
  
  if((contadorAcertoumq2Terceira + contadorErroumq2Terceira)>0){
    df_resultado$porcAcertomq2Terceira[linhaResultado] = (contadorAcertoumq2Terceira/(contadorAcertoumq2Terceira + contadorErroumq2Terceira))*100.0    
  } else {
    df_resultado$porcAcertomq2Terceira[linhaResultado] = 0
  }
  
  # lora as => las
  if((contadorAcertoulasPrimeira + contadorErroulasPrimeira)>0){
    df_resultado$porcAcertolasPrimeira[linhaResultado] = (contadorAcertoulasPrimeira/(contadorAcertoulasPrimeira + contadorErroulasPrimeira))*100.0    
  } else {
    df_resultado$porcAcertolasPrimeira[linhaResultado] = 0
  }
  
  if((contadorAcertoulasSegunda + contadorErroulasSegunda)>0){
    df_resultado$porcAcertolasSegunda[linhaResultado] = (contadorAcertoulasSegunda/(contadorAcertoulasSegunda + contadorErroulasSegunda))*100.0    
  } else {
    df_resultado$porcAcertolasSegunda[linhaResultado] = 0
  }

  if((contadorAcertoulasTerceira + contadorErroulasTerceira)>0){
    df_resultado$porcAcertolasTerceira[linhaResultado] = (contadorAcertoulasTerceira/(contadorAcertoulasTerceira + contadorErroulasTerceira))*100.0    
  } else {
    df_resultado$porcAcertolasTerceira[linhaResultado] = 0
  }
  
  # saida da Fog => outFog
  if((contadorAcertououtFogPrimeira + contadorErrououtFogPrimeira)>0){
    df_resultado$porcAcertooutFogPrimeira[linhaResultado] = (contadorAcertououtFogPrimeira/(contadorAcertououtFogPrimeira + contadorErrououtFogPrimeira))*100.0    
  } else {
    df_resultado$porcAcertooutFogPrimeira[linhaResultado] = 0
  }
  
  if((contadorAcertououtFogSegunda + contadorErrououtFogSegunda)>0){
    df_resultado$porcAcertooutFogSegunda[linhaResultado] = (contadorAcertououtFogSegunda/(contadorAcertououtFogSegunda + contadorErrououtFogSegunda))*100.0    
  } else {
    df_resultado$porcAcertooutFogSegunda[linhaResultado] = 0
  }
  
  if((contadorAcertououtFogTerceira + contadorErrououtFogTerceira)>0){
    df_resultado$porcAcertooutFogTerceira[linhaResultado] = (contadorAcertououtFogTerceira/(contadorAcertououtFogTerceira + contadorErrououtFogTerceira))*100.0    
  } else {
    df_resultado$porcAcertooutFogTerceira[linhaResultado] = 0
  }
  
  # aa1 => aa1
  if((contadorAcertouaa1Primeira + contadorErrouaa1Primeira)>0){
    df_resultado$porcAcertoaa1Primeira[linhaResultado] = (contadorAcertouaa1Primeira/(contadorAcertouaa1Primeira + contadorErrouaa1Primeira))*100.0    
  } else {
    df_resultado$porcAcertoaa1Primeira[linhaResultado] = 0
  }
  
  if((contadorAcertouaa1Segunda + contadorErrouaa1Segunda)>0){
    df_resultado$porcAcertoaa1Segunda[linhaResultado] = (contadorAcertouaa1Segunda/(contadorAcertouaa1Segunda + contadorErrouaa1Segunda))*100.0    
  } else {
    df_resultado$porcAcertoaa1Segunda[linhaResultado] = 0
  }
  
  if((contadorAcertouaa1Terceira + contadorErrouaa1Terceira)>0){
    df_resultado$porcAcertoaa1Terceira[linhaResultado] = (contadorAcertouaa1Terceira/(contadorAcertouaa1Terceira + contadorErrouaa1Terceira))*100.0    
  } else {
    df_resultado$porcAcertoaa1Terceira[linhaResultado] = 0
  }
  
  # iot Agent => iot
  if((contadorAcertouiotPrimeira + contadorErrouiotPrimeira)>0){
    df_resultado$porcAcertoiotPrimeira[linhaResultado] = (contadorAcertouiotPrimeira/(contadorAcertouiotPrimeira + contadorErrouiotPrimeira))*100.0    
  } else {
    df_resultado$porcAcertoiotPrimeira[linhaResultado] = 0
  }
  
  if((contadorAcertouiotSegunda + contadorErrouiotSegunda)>0){
    df_resultado$porcAcertoiotSegunda[linhaResultado] = (contadorAcertouiotSegunda/(contadorAcertouiotSegunda + contadorErrouiotSegunda))*100.0    
  } else {
    df_resultado$porcAcertoiotSegunda[linhaResultado] = 0
  }
  
  if((contadorAcertouiotTerceira + contadorErrouiotTerceira)>0){
    df_resultado$porcAcertoiotTerceira[linhaResultado] = (contadorAcertouiotTerceira/(contadorAcertouiotTerceira + contadorErrouiotTerceira))*100.0    
  } else {
    df_resultado$porcAcertoiotTerceira[linhaResultado] = 0
  }
  
  # mongo => mo
  if((contadorAcertoumoPrimeira + contadorErroumoPrimeira)>0){
    df_resultado$porcAcertomoPrimeira[linhaResultado] = (contadorAcertoumoPrimeira/(contadorAcertoumoPrimeira + contadorErroumoPrimeira))*100.0    
  } else {
    df_resultado$porcAcertomoPrimeira[linhaResultado] = 0
  }
  
  if((contadorAcertoumoSegunda + contadorErroumoSegunda)>0){
    df_resultado$porcAcertomoSegunda[linhaResultado] = (contadorAcertoumoSegunda/(contadorAcertoumoSegunda + contadorErroumoSegunda))*100.0    
  } else {
    df_resultado$porcAcertomoSegunda[linhaResultado] = 0
  }
  
  if((contadorAcertoumoTerceira + contadorErroumoTerceira)>0){
    df_resultado$porcAcertomoTerceira[linhaResultado] = (contadorAcertoumoTerceira/(contadorAcertoumoTerceira + contadorErroumoTerceira))*100.0    
  } else {
    df_resultado$porcAcertomoTerceira[linhaResultado] = 0
  }
  
  # orion => ori
  if((contadorAcertouoriPrimeira + contadorErrouoriPrimeira)>0){
    df_resultado$porcAcertooriPrimeira[linhaResultado] = (contadorAcertouoriPrimeira/(contadorAcertouoriPrimeira + contadorErrouoriPrimeira))*100.0    
  } else {
    df_resultado$porcAcertooriPrimeira[linhaResultado] = 0
  }
  
  if((contadorAcertouoriSegunda + contadorErrouoriSegunda)>0){
    df_resultado$porcAcertooriSegunda[linhaResultado] = (contadorAcertouoriSegunda/(contadorAcertouoriSegunda + contadorErrouoriSegunda))*100.0    
  } else {
    df_resultado$porcAcertooriSegunda[linhaResultado] = 0
  }
  
  if((contadorAcertouoriTerceira + contadorErrouoriTerceira)>0){
    df_resultado$porcAcertooriTerceira[linhaResultado] = (contadorAcertouoriTerceira/(contadorAcertouoriTerceira + contadorErrouoriTerceira))*100.0    
  } else {
    df_resultado$porcAcertooriTerceira[linhaResultado] = 0
  }
  
  # ql => ql
  if((contadorAcertouqlPrimeira + contadorErrouqlPrimeira)>0){
    df_resultado$porcAcertoqlPrimeira[linhaResultado] = (contadorAcertouqlPrimeira/(contadorAcertouqlPrimeira + contadorErrouqlPrimeira))*100.0    
  } else {
    df_resultado$porcAcertoqlPrimeira[linhaResultado] = 0
  }
  
  if((contadorAcertouqlSegunda + contadorErrouqlSegunda)>0){
    df_resultado$porcAcertoqlSegunda[linhaResultado] = (contadorAcertouqlSegunda/(contadorAcertouqlSegunda + contadorErrouqlSegunda))*100.0    
  } else {
    df_resultado$porcAcertoqlSegunda[linhaResultado] = 0
  }
  
  if((contadorAcertouqlTerceira + contadorErrouqlTerceira)>0){
    df_resultado$porcAcertoqlTerceira[linhaResultado] = (contadorAcertouqlTerceira/(contadorAcertouqlTerceira + contadorErrouqlTerceira))*100.0    
  } else {
    df_resultado$porcAcertoqlTerceira[linhaResultado] = 0
  }
  
  # Crate DB => cdb
  if((contadorAcertoucdbPrimeira + contadorErroucdbPrimeira)>0){
    df_resultado$porcAcertocdbPrimeira[linhaResultado] = (contadorAcertoucdbPrimeira/(contadorAcertoucdbPrimeira + contadorErroucdbPrimeira))*100.0    
  } else {
    df_resultado$porcAcertocdbPrimeira[linhaResultado] = 0
  }
  
  if((contadorAcertoucdbSegunda + contadorErroucdbSegunda)>0){
    df_resultado$porcAcertocdbSegunda[linhaResultado] = (contadorAcertoucdbSegunda/(contadorAcertoucdbSegunda + contadorErroucdbSegunda))*100.0    
  } else {
    df_resultado$porcAcertocdbSegunda[linhaResultado] = 0
  }
  
  if((contadorAcertoucdbTerceira + contadorErroucdbTerceira)>0){
    df_resultado$porcAcertocdbTerceira[linhaResultado] = (contadorAcertoucdbTerceira/(contadorAcertoucdbTerceira + contadorErroucdbTerceira))*100.0    
  } else {
    df_resultado$porcAcertocdbTerceira[linhaResultado] = 0
  }
  
  # aa2 => aa2
  if((contadorAcertouaa2Primeira + contadorErrouaa2Primeira)>0){
    df_resultado$porcAcertoaa2Primeira[linhaResultado] = (contadorAcertouaa2Primeira/(contadorAcertouaa2Primeira + contadorErrouaa2Primeira))*100.0    
  } else {
    df_resultado$porcAcertoaa2Primeira[linhaResultado] = 0
  }
  
  if((contadorAcertouaa2Segunda + contadorErrouaa2Segunda)>0){
    df_resultado$porcAcertoaa2Segunda[linhaResultado] = (contadorAcertouaa2Segunda/(contadorAcertouaa2Segunda + contadorErrouaa2Segunda))*100.0    
  } else {
    df_resultado$porcAcertoaa2Segunda[linhaResultado] = 0
  }

  if((contadorAcertouaa2Terceira + contadorErrouaa2Terceira)>0){
    df_resultado$porcAcertoaa2Terceira[linhaResultado] = (contadorAcertouaa2Terceira/(contadorAcertouaa2Terceira + contadorErrouaa2Terceira))*100.0    
  } else {
    df_resultado$porcAcertoaa2Terceira[linhaResultado] = 0
  }
  
  # saída da Cloud => outCloud
  if((contadorAcertououtCloudPrimeira + contadorErrououtCloudPrimeira)>0){
    df_resultado$porcAcertooutCloudPrimeira[linhaResultado] = (contadorAcertououtCloudPrimeira/(contadorAcertououtCloudPrimeira + contadorErrououtCloudPrimeira))*100.0    
  } else {
    df_resultado$porcAcertooutCloudPrimeira[linhaResultado] = 0
  }
  
  if((contadorAcertououtCloudSegunda + contadorErrououtCloudSegunda)>0){
    df_resultado$porcAcertooutCloudSegunda[linhaResultado] = (contadorAcertououtCloudSegunda/(contadorAcertououtCloudSegunda + contadorErrououtCloudSegunda))*100.0    
  } else {
    df_resultado$porcAcertooutCloudSegunda[linhaResultado] = 0
  }
  
  if((contadorAcertououtCloudTerceira + contadorErrououtCloudTerceira)>0){
    df_resultado$porcAcertooutCloudTerceira[linhaResultado] = (contadorAcertououtCloudTerceira/(contadorAcertououtCloudTerceira + contadorErrououtCloudTerceira))*100.0    
  } else {
    df_resultado$porcAcertooutCloudTerceira[linhaResultado] = 0
  }
  ##################################################################
  # gravação do arquivo de resultado do processamento
  write_json(df_resultado, ArquivoResultado)
  
} # fim da função processaUmArquivoCenario3


##################################################################################
# FUNCTION : processaUmArquivoCenario4
#
# processamento de cada arquivo para cada percentual de buracos para o cenario 4
#
##################################################################################

processaUmArquivoCenario4 <- function (vez,
                                       linhaResultado,
                                       registros,
                                       furos,
                                       df_white,
                                       df_black) {
  
  ###################################################################################
  # carrega a base na variável bn_df como um dataframe
  ###################################################################################
  #
  nomeArquivo = paste(NomeExperimento,"_",registros,".csv",sep="",collapse="")
  bn_df <- read.csv(nomeArquivo)
  
  ###################################################################################
  # Converter as colunas para Factor para a rede bayesiana - função hc()
  ###################################################################################
  # fog
  bn_df$gtb = as.factor(bn_df$gtb)
  bn_df$mq1 = as.factor(bn_df$mq1)
  bn_df$ls  = as.factor(bn_df$ls)
  bn_df$mq2 = as.factor(bn_df$mq2)
  bn_df$las = as.factor(bn_df$las)
  bn_df$outFog = as.factor(bn_df$outFog)  
  bn_df$aa1 = as.factor(bn_df$aa1)

  ###################################################################################
  # dividir a amostra em duas partes: treino e teste
  ###################################################################################
  #
  divisao = sample.split(bn_df$gtb,SplitRatio = 0.5)
  
  base_treinamento      = subset(bn_df, divisao == TRUE)
  base_teste            = subset(bn_df, divisao == FALSE)
  base_teste_sem_buraco = subset(bn_df, divisao == FALSE)
  
  ###################################################################################
  # aplica a função hc na variável bn_df e guarda o resultado na variável res
  # Sobre a função: Agrupamento hierárquico aglomerativo baseado em critérios de 
  # máxima verossimilhança para modelos de mistura gaussiana parametrizados por 
  # decomposição de autovalores.
  ###################################################################################
  # O que eu entendi: está criando a RB com o algoritmo "max-min hill climbing", 
  # que é um algoritmo guloso.
  ###################################################################################
  #
  res = hc(base_treinamento, whitelist = df_white, blacklist = df_black)
  
  ###################################################################################
  # Grava o Plot das dependências funcionais entre as variáveis
  ###################################################################################
  #
  
  nomeArquivoImagem = paste(NomeExperimento,"_imagem.png",sep="",collapse="")
  if(!file.exists(nomeArquivoImagem)){
    png(file = nomeArquivoImagem)
    plot(res)
    dev.off()
  }
  
  ###################################################################################
  # treinar a rede
  # Depois de aprender a estrutura, é preciso descobrir as tabelas de probabilidade
  # condicional (CPTs) em cada nó. A função bn.fit executa o algoritmo EM para 
  # aprender CPT para diferentes nós no gráfico acima.
  ###################################################################################
  #
  #cat("treinar a rede\n")
  fittedbn <- bn.fit(res, data = base_treinamento)
  
  
  ###################################################################################
  # Explorando um dos cálculos
  # Por exemplo, vamos ver o que está dentro do nó s1
  ###################################################################################
  #
  #cat("explorando um dos cálculos\n")
  # fittedbn é um objeto da classe list
  #print(fittedbn)
  # s1 
  #print(fittedbn$s1)
  
  ###################################################################################
  # insere nulos
  ###################################################################################
  #
  porcentagem = furos/100
  
  #cat("Porcentagem=",porcentagem,"\n")
  
  nlin <- nrow(base_teste)
  ncol <- ncol(base_teste)
  totalelementos <- nlin * ncol
  porc_aleatorio <- totalelementos * porcentagem
  
  if(porc_aleatorio<1){
    porc_aleatorio=1
  }
  
  aleatorios <- sort(sample(1:totalelementos, porc_aleatorio))
  
  for (i in 1:porc_aleatorio){
    substituir <- aleatorios[i]
    linsub     <- (substituir %/% ncol)
    colsub     <- (substituir %% ncol)
    if(colsub==0){
      colsub <- ncol
    }
    base_teste[linsub,colsub] <- 'NULL'
  }
  
  #cat("Inseriu nulos\n")  
  
  
  ###################################################################################
  # Verifica se deu linha 100% nula
  ###################################################################################
  #
  contaNulo <- 0
  for (i in 1:nlin){
    for(j in 1:ncol){
      if (is.na  (base_teste[i,j]) || 
          is.null(base_teste[i,j]) ||
          (base_teste[i,j]=="NULL")){
        contaNulo = contaNulo + 1
      }
    }
    if(contaNulo==ncol){
      print("tudo nulo, não da pra fazer a predição")
      contaNulo <- 0 
    #stop("tudo nulo, não da pra fazer a predição")
	}
    else
      contaNulo <- 0
  }
  #cat("Verificou linhas todas de buracos")
  
  ###################################################################################
  # Conta total de buracos na base
  # Conta total de linhas sem buracos
  # Conta total de linhas com buracos
  ###################################################################################
  #
  contadorDeBuracos    <- 0
  qtdlinhasSemBuracos  <- 0
  qtdlinhasComBuracos  <- 0
  
  for (i in 1:nlin){
    contAux=0
    for(j in 1:ncol){
      if (is.na  (base_teste[i,j]) || 
          is.null(base_teste[i,j]) ||
          (base_teste[i,j]=="NULL")){
        contAux=contAux+1
        contadorDeBuracos = contadorDeBuracos + 1
      }
    }
    if(contAux==0)
      qtdlinhasSemBuracos = qtdlinhasSemBuracos + 1
    else
      qtdlinhasComBuracos = qtdlinhasComBuracos + 1
  }
  
  #cat("Contou linhas e buracos\n")  
  
  ###################################################################################
  # informa valores para as colunas e suas categorias
  # O ideal seria vir de algum arquivo...
  ###################################################################################
  #
  valores <- list(c('0', '1'),
                  c('0', '1'),
                  c('0', '1'),
                  c('0', '1'),
                  c('0', '1'),
                  c('0', '1'),
                  c('0', '1'))
  
  categorias <- list(c('gtb','mq1','ls','mq2','las','outFog','aa1'))
  
  ###################################################################################
  # inicialização dos contadores gerais do processamento
  ###################################################################################
  #
  
  contadorDeBuracosErradosPrimeira  <- 0
  contadorDeBuracosErradosSegunda   <- 0
  contadorDeBuracosErradosTerceira  <- 0
  qtdAcertouTudoPrimeira            <- 0
  qtdAcertouTudoSegunda             <- 0
  qtdAcertouTudoTerceira            <- 0
  
  contadorAcertougtbPrimeira        <- 0
  contadorAcertougtbSegunda         <- 0
  contadorAcertougtbTerceira        <- 0

  contadorAcertoumq1Primeira        <- 0
  contadorAcertoumq1Segunda         <- 0
  contadorAcertoumq1Terceira        <- 0

  contadorAcertoulsPrimeira         <- 0
  contadorAcertoulsSegunda          <- 0
  contadorAcertoulsTerceira         <- 0

  contadorAcertoumq2Primeira        <- 0
  contadorAcertoumq2Segunda         <- 0
  contadorAcertoumq2Terceira        <- 0
  
  contadorAcertoulasPrimeira        <- 0
  contadorAcertoulasSegunda         <- 0
  contadorAcertoulasTerceira        <- 0
  
  contadorAcertououtFogPrimeira        <- 0
  contadorAcertououtFogSegunda         <- 0
  contadorAcertououtFogTerceira        <- 0
  
  contadorAcertouaa1Primeira        <- 0
  contadorAcertouaa1Segunda         <- 0
  contadorAcertouaa1Terceira        <- 0
  
  contadorErrougtbPrimeira          <- 0
  contadorErrougtbSegunda           <- 0
  contadorErrougtbTerceira          <- 0
  
  contadorErroumq1Primeira          <- 0
  contadorErroumq1Segunda           <- 0
  contadorErroumq1Terceira          <- 0
  
  contadorErroulsPrimeira           <- 0
  contadorErroulsSegunda            <- 0
  contadorErroulsTerceira           <- 0
  
  contadorErroumq2Primeira          <- 0
  contadorErroumq2Segunda           <- 0
  contadorErroumq2Terceira          <- 0
  
  contadorErroulasPrimeira          <- 0
  contadorErroulasSegunda           <- 0
  contadorErroulasTerceira          <- 0
  
  contadorErrououtFogPrimeira          <- 0
  contadorErrououtFogSegunda           <- 0
  contadorErrououtFogTerceira          <- 0
  
  contadorErrouaa1Primeira          <- 0
  contadorErrouaa1Segunda           <- 0
  contadorErrouaa1Terceira          <- 0
  
  ###################################################################################
  # Laço principal para todas as instâncias
  # do arquivo
  ###################################################################################
  #
  for(i1 in 1:nlin){
    #cat("***Laco Principal - processando linha ", i1, "\n")
    dado <- base_teste[i1,]
    
    #####################################################
    # Verificando se a linha tem buracos
    # se não tiver então pular para a próxima linha
    #####################################################
    qtdBuracosLinha = 0
    
    for (i2 in 1:length(dado)){
      if (is.na(dado[i2]) ||
          is.null(dado[i2]) ||
          (dado[i2]=="NULL"))
        qtdBuracosLinha = qtdBuracosLinha + 1        
    }
    
    
    if(qtdBuracosLinha==0 || 
       qtdBuracosLinha==length(dado) ||
       qtdBuracosLinha/length(dado) > 0.75){ 
	  # 24/07/2020
      #não precisa fazer queries ... então ir para o próximo
      #print ("Linha sem buracos... pulada")
      next
    }
    
    
    ##########################################
    # monta as queries
    ##########################################
    
    # limpando as variáveis
    dfv1 <- NULL
    dfv2 <- NULL
    dadoCopia <- NULL
    
    # fazendo uma cópia do dado (dataframe)
    dadoCopia <- dado
    
    # guardando o nome das colunas do dado (dataframe)
    nomeColunas <- colnames(dado)
    
    flag <- TRUE
    for (i2 in length(dado):1)
      if (is.na(dado[i2]) ||
          is.null(dado[i2]) ||
          (dado[i2]=="NULL"))
      {
        dadoCopia <- dadoCopia[,-c(i2)]
        if(flag)
        {
          flag <- FALSE
          dfV1 <- data.frame(valores[i2])
          names(dfV1) <- nomeColunas[i2]
        } else {
          dfV2 <- data.frame(valores[i2])
          names(dfV2) <- nomeColunas[i2]
          dfV1 <- cartesianExpand(dfV1, dfV2)
        }
      }
    
    queries <- as.data.frame(cartesianExpand(dadoCopia, dfV1))
    
    # obs: valores nulos vão no evento e os não nulos nas evidencias
    strEventoColuna    <- NULL
    strEvidenciaColuna <- NULL
    strEventoValor     <- NULL
    strEvidenciaValor  <- NULL
    
    contadorNull = 0
    for (i3 in length(dado):1)
      if (is.na(dado[i3]) ||
          is.null(dado[i3]) ||
          (dado[i3]=="NULL")){
        contadorNull = contadorNull + 1
        strEventoColuna <- c(nomeColunas[i3],strEventoColuna)
      } else {
        strEvidenciaColuna <- c(nomeColunas[i3],strEvidenciaColuna)
        strEvidenciaValor <- c((as.character(dado[1,i3])),strEvidenciaValor)
      }
    
    # devo pegar as três maiores probabilidades
    maiorProbabilidade = -1
    segundaMaiorProbabilidade = -1
    terceiraMaiorProbabilidade = -1
    
    queryMaiorProbabilidade = NULL
    querySegundaMaiorProbabilidade = NULL
    queryTerceiraMaiorProbabilidade = NULL
    
    indiceMaior = -1
    indiceSegundoMaior = -1
    indiceTerceiroMaior = -1
    
    #cat("nrow(queries) ===> ", nrow(queries), "\n")
    
    
    # percorrendo cada linha das queries
    for(i4 in 1:nrow(queries)){
      for(i5 in (length(dado)-contadorNull+1):(length(dado))){
        strEventoValor <- c((as.character(queries[i4,i5])),strEventoValor)
      }
      
      str1 = paste("(", strEventoColuna   , " == \"",strEventoValor   , "\")", sep = "", collapse = " & ")
      str2 = paste("(", strEvidenciaColuna, " == \"",strEvidenciaValor, "\")", sep = "")
      
      cmd = paste("cpquery(fittedbn, event = ", str1, ", evidence = ", str2, ")", sep = "")
      probabilidade = eval(parse(text = cmd))
      
      #cat("probabilidade===>", probabilidade, "\n")
      
      
      #pegando as três maiores probabilidades
      if(probabilidade > maiorProbabilidade){
        # a segunda passa a ser a terceira
        terceiraMaiorProbabilidade = segundaMaiorProbabilidade
        queryTerceiraMaiorProbabilidade = querySegundaMaiorProbabilidade
        indiceTerceiroMaior = indiceSegundoMaior
        
        # a primeira passa a ser a segunda
        segundaMaiorProbabilidade = maiorProbabilidade
        querySegundaMaiorProbabilidade = queryMaiorProbabilidade
        indiceSegundoMaior = indiceMaior
        
        # a nova passa a ser a primeira
        maiorProbabilidade = probabilidade
        queryMaiorProbabilidade = queries[i4,]
        indiceMaior = i4
      } else {
        if(probabilidade > segundaMaiorProbabilidade){
          # a segunda passa a ser a terceira
          terceiraMaiorProbabilidade = segundaMaiorProbabilidade
          queryTerceiraMaiorProbabilidade = querySegundaMaiorProbabilidade
          indiceTerceiroMaior = indiceSegundoMaior
          
          # a nova passa a ser a segunda
          segundaMaiorProbabilidade = probabilidade
          querySegundaMaiorProbabilidade = queries[i4,]
          indiceSegundoMaior = i4
        } else {
          if(probabilidade > terceiraMaiorProbabilidade){
            # a nova passa a ser a terceira
            terceiraMaiorProbabilidade = probabilidade
            queryTerceiraMaiorProbabilidade = queries[i4,]
            indiceTerceiraMaior = i4
          }
        }
      }
      
      strEventoValor <- NULL
    } # for i4
    
    
    ###################################################################################
    #  
    #  __   
    # /_ |  
    #  | |              Analisando os acertos e erros de primeira
    #  | | 
    #  | |
    #  |_|
    #
    ###################################################################################
    if(maiorProbabilidade>-1){
      #########################################################
      #cat ("Maior Probabiliade = ", maiorProbabilidade,"\n")
      #print ("Linha com buracos  = ")
      #print (dado[,order(names(dado))])
      #########################################################    
      
      ## auxiliar para identificar os buracos originais
      aux1 <- dado[,order(names(dado))]
      aux2 <- colnames(aux1)
      
      #########################################################
      #print ("Query selecionada  = ")
      #print (queryMaiorProbabilidade[,order(names(queryMaiorProbabilidade))])
      #print ("Linha sem buracos  = ")
      #print (base_teste_sem_buraco[i1,order(names(bn_df))])
      #########################################################
      contadorDiferencasPrimeira = 0
      ContadorIgualdadesPrimeira = 0
      
      # AA guarda a query com os buracos preenchidos com os valores de maior probabilidade
      AA <- queryMaiorProbabilidade[,order(names(queryMaiorProbabilidade))]
      # BB guarda a linha com os valores originais para verificar se acertou ou não
      BB <- base_teste_sem_buraco[i1,order(names(bn_df))]
      
      for(ww in 1:length(dado)){
        if(as.character(AA[,ww]) == as.character(BB[,ww])){
          if(is.na(aux1[ww]) || is.null(aux1[ww]) || (aux1[ww]=="NULL")){
            ## acertou um buraco
              if(aux2[ww] == "gtb"){contadorAcertougtbPrimeira = contadorAcertougtbPrimeira + 1}
            else 
              if(aux2[ww] == "mq1"){contadorAcertoumq1Primeira = contadorAcertoumq1Primeira + 1}
            else 
              if(aux2[ww] == "ls"){contadorAcertoulsPrimeira = contadorAcertoulsPrimeira + 1}
            else 
              if(aux2[ww] == "mq2"){contadorAcertoumq2Primeira = contadorAcertoumq2Primeira + 1}
            else
              if(aux2[ww] == "las"){contadorAcertoulasPrimeira = contadorAcertoulasPrimeira + 1}
            else
              if(aux2[ww] == "oufFog"){contadorAcertououtFogPrimeira = contadorAcertououtFogPrimeira + 1}
            else
              if(aux2[ww] == "aa1"){contadorAcertouaa1Primeira = contadorAcertouaa1Primeira + 1}
          }
          
          ContadorIgualdadesPrimeira <- ContadorIgualdadesPrimeira + 1
        } else {
          if(is.na(aux1[ww]) || is.null(aux1[ww]) || (aux1[ww]=="NULL")){
            ## errou um buraco
              if(aux2[ww] == "gtb"){contadorErrougtbPrimeira = contadorErrougtbPrimeira + 1}
            else 
              if(aux2[ww] == "mq1"){contadorErroumq1Primeira = contadorErroumq1Primeira + 1}
            else 
              if(aux2[ww] == "ls"){contadorErroulsPrimeira = contadorErroulsPrimeira + 1}
            else 
              if(aux2[ww] == "mq2"){contadorErroumq2Primeira = contadorErroumq2Primeira + 1}
            else
              if(aux2[ww] == "las"){contadorErroulasPrimeira = contadorErroulasPrimeira + 1}
            else
              if(aux2[ww] == "outFog"){contadorErrououtFogPrimeira = contadorErrououtFogPrimeira + 1}
            else
              if(aux2[ww] == "aa1"){contadorErrouaa1Primeira = contadorErrouaa1Primeira + 1}
          }
          contadorDiferencasPrimeira <- contadorDiferencasPrimeira + 1
          contadorDeBuracosErradosPrimeira = contadorDeBuracosErradosPrimeira + 1
          #########################################################        
          #cat("Q: ",colnames(AA[ww]),"...:",as.character(AA[,ww]),"\t",
          #    "B: ",colnames(BB[ww]),"...:",as.character(BB[,ww]),"\n")
          #########################################################        
        }
      } # for ww
      
      #########################################################    
      #cat ("Qtde de Buracos certos  Primeira  = ", (qtdBuracosLinha-contadorDiferencasPrimeira), "\n")
      #cat ("Qtde de Buracos errados Primeira  = ", contadorDiferencasPrimeira, "\n")
      #cat ("Qtde de Buracos                   = ", qtdBuracosLinha, "\n")
      #########################################################    
      
      if(contadorDiferencasPrimeira==0)
        qtdAcertouTudoPrimeira = qtdAcertouTudoPrimeira + 1    
      
    } # if(maiorProbabilidade>-1)
    
    
    ###################################################################################
    #  
    #  ___  
    # |__ \
    #    ) |            Analisando os acertos e erros de Segunda
    #   / / 
    #  / /_ 
    # |____|
    #
    ###################################################################################
    if(segundaMaiorProbabilidade>-1){
      #########################################################
      #cat ("Segunda Maior Probabiliade = ", segundaMaiorProbabilidade,"\n")
      #print ("Linha com buracos  = ")
      #print (dado[,order(names(dado))])
      #########################################################    
      
      ## auxiliar para identificar os buracos originais
      aux1 <- dado[,order(names(dado))]
      aux2 <- colnames(aux1)
      
      #########################################################
      #print ("Query selecionada  = ")
      #print (querySegundaMaiorProbabilidade[,order(names(querySegundaMaiorProbabilidade))])
      #print ("Linha sem buracos  = ")
      #print (base_teste_sem_buraco[i1,order(names(bn_df))])
      #########################################################
      contadorDiferencasSegunda = 0
      ContadorIgualdadesSegunda = 0
      
      # AA guarda a query com os buracos preenchidos com os valores de maior probabilidade
      AA <- querySegundaMaiorProbabilidade[,order(names(querySegundaMaiorProbabilidade))]
      # BB guarda a linha com os valores originais para verificar se acertou ou não
      BB <- base_teste_sem_buraco[i1,order(names(bn_df))]
      
      for(ww in 1:length(dado)){
        if(as.character(AA[,ww]) == as.character(BB[,ww])){
          if(is.na(aux1[ww]) || is.null(aux1[ww]) || (aux1[ww]=="NULL")){
            ## acertou um buraco
              if(aux2[ww] == "gtb"){contadorAcertougtbSegunda = contadorAcertougtbSegunda + 1}
            else 
              if(aux2[ww] == "mq1"){contadorAcertoumq1Segunda = contadorAcertoumq1Segunda + 1}
            else 
              if(aux2[ww] == "ls"){contadorAcertoulsSegunda = contadorAcertoulsSegunda + 1}
            else 
              if(aux2[ww] == "mq2"){contadorAcertoumq2Segunda = contadorAcertoumq2Segunda + 1}
            else
              if(aux2[ww] == "las"){contadorAcertoulasSegunda = contadorAcertoulasSegunda + 1}
            else
              if(aux2[ww] == "outFog"){contadorAcertououtFogSegunda = contadorAcertououtFogSegunda + 1}
            else
              if(aux2[ww] == "aa1"){contadorAcertouaa1Segunda = contadorAcertouaa1Segunda + 1}
          }
          
          ContadorIgualdadesSegunda <- ContadorIgualdadesSegunda + 1
        } else {
          if(is.na(aux1[ww]) || is.null(aux1[ww]) || (aux1[ww]=="NULL")){
            ## errou um buraco
              if(aux2[ww] == "gtb"){contadorErrougtbSegunda = contadorErrougtbSegunda + 1}
            else 
              if(aux2[ww] == "mq1"){contadorErroumq1Segunda = contadorErroumq1Segunda + 1}
            else 
              if(aux2[ww] == "ls"){contadorErroulsSegunda = contadorErroulsSegunda + 1}
            else 
              if(aux2[ww] == "mq2"){contadorErroumq2Segunda = contadorErroumq2Segunda + 1}
            else
              if(aux2[ww] == "las"){contadorErroulasSegunda = contadorErroulasSegunda + 1}
            else
              if(aux2[ww] == "outFog"){contadorErrououtFogSegunda = contadorErrououtFogSegunda + 1}
            else
              if(aux2[ww] == "aa1"){contadorErrouaa1Segunda = contadorErrouaa1Segunda + 1}
          }
          contadorDiferencasSegunda <- contadorDiferencasSegunda + 1
          contadorDeBuracosErradosSegunda = contadorDeBuracosErradosSegunda + 1
          #########################################################        
          #cat("Q: ",colnames(AA[ww]),"...:",as.character(AA[,ww]),"\t",
          #    "B: ",colnames(BB[ww]),"...:",as.character(BB[,ww]),"\n")
          #########################################################        
        }
      } # for ww
      
      
      #########################################################    
      #cat ("Qtde de Buracos certos  Segunda = ", (qtdBuracosLinha-contadorDiferencasSegunda), "\n")
      #cat ("Qtde de Buracos errados Segunda = ", contadorDiferencasSegunda, "\n")
      #cat ("Qtde de Buracos                 = ", qtdBuracosLinha, "\n")
      #########################################################    
      
      if(contadorDiferencasSegunda==0)
        qtdAcertouTudoSegunda = qtdAcertouTudoSegunda + 1    
      
    } # if(segundaMaiorProbabilidade>-1) - cenario 4
    
    ###################################################################################
    #  
    #  ____  
    # |___ \   
    #   __) |           Analisando os acertos e erros de Terceira
    #  |__ < 
    #  ___) |
    # |____/
    #
    ###################################################################################
    if(terceiraMaiorProbabilidade>-1){
      #########################################################
      #cat ("Terceira Maior Probabiliade = ", terceiraMaiorProbabilidade,"\n")
      #print ("Linha com buracos  = ")
      #print (dado[,order(names(dado))])
      #########################################################    
      
      ## auxiliar para identificar os buracos originais
      aux1 <- dado[,order(names(dado))]
      aux2 <- colnames(aux1)
      
      #########################################################
      #print ("Query selecionada  = ")
      #print (queryTerceiraMaiorProbabilidade[,order(names(queryTerceiraMaiorProbabilidade))])
      #print ("Linha sem buracos  = ")
      #print (base_teste_sem_buraco[i1,order(names(bn_df))])
      #########################################################
      contadorDiferencasTerceira = 0
      ContadorIgualdadesTerceira = 0
      
      # AA guarda a query com os buracos preenchidos com os valores de maior probabilidade
      AA <- queryTerceiraMaiorProbabilidade[,order(names(queryTerceiraMaiorProbabilidade))]
      # BB guarda a linha com os valores originais para verificar se acertou ou não
      BB <- base_teste_sem_buraco[i1,order(names(bn_df))]
      
      for(ww in 1:length(dado)){
        if(as.character(AA[,ww]) == as.character(BB[,ww])){
          if(is.na(aux1[ww]) || is.null(aux1[ww]) || (aux1[ww]=="NULL")){
            ## acertou um buraco
              if(aux2[ww] == "gtb"){contadorAcertougtbTerceira = contadorAcertougtbTerceira + 1}
            else 
              if(aux2[ww] == "mq1"){contadorAcertoumq1Terceira = contadorAcertoumq1Terceira + 1}
            else 
              if(aux2[ww] == "ls"){contadorAcertoulsTerceira = contadorAcertoulsTerceira + 1}
            else 
              if(aux2[ww] == "mq2"){contadorAcertoumq2Terceira = contadorAcertoumq2Terceira + 1}
            else
              if(aux2[ww] == "las"){contadorAcertoulasTerceira = contadorAcertoulasTerceira + 1}
            else
              if(aux2[ww] == "outFog"){contadorAcertououtFogTerceira = contadorAcertououtFogTerceira + 1}
            else
              if(aux2[ww] == "aa1"){contadorAcertouaa1Terceira = contadorAcertouaa1Terceira + 1}
          }
          
          ContadorIgualdadesTerceira <- ContadorIgualdadesTerceira + 1
        } else {
          if(is.na(aux1[ww]) || is.null(aux1[ww]) || (aux1[ww]=="NULL")){
            ## errou um buraco
              if(aux2[ww] == "gtb"){contadorErrougtbTerceira = contadorErrougtbTerceira + 1}
            else 
              if(aux2[ww] == "mq1"){contadorErroumq1Terceira = contadorErroumq1Terceira + 1}
            else 
              if(aux2[ww] == "ls"){contadorErroulsTerceira = contadorErroulsTerceira + 1}
            else 
              if(aux2[ww] == "mq2"){contadorErroumq2Terceira = contadorErroumq2Terceira + 1}
            else
              if(aux2[ww] == "las"){contadorErroulasTerceira = contadorErroulasTerceira + 1}
            else
              if(aux2[ww] == "outFog"){contadorErrououtFogTerceira = contadorErrououtFogTerceira + 1}
            else
              if(aux2[ww] == "aa1"){contadorErrouaa1Terceira = contadorErrouaa1Terceira + 1}
          }
          contadorDiferencasTerceira <- contadorDiferencasTerceira + 1
          contadorDeBuracosErradosTerceira = contadorDeBuracosErradosTerceira + 1
          #########################################################        
          #cat("Q: ",colnames(AA[ww]),"...:",as.character(AA[,ww]),"\t",
          #    "B: ",colnames(BB[ww]),"...:",as.character(BB[,ww]),"\n")
          #########################################################        
        }
      } # for ww
      
      
      #########################################################    
      #cat ("Qtde de Buracos certos  Terceira = ", (qtdBuracosLinha-contadorDiferencasTerceira), "\n")
      #cat ("Qtde de Buracos errados Terceira = ", contadorDiferencasTerceira, "\n")
      #cat ("Qtde de Buracos                  = ", qtdBuracosLinha, "\n")
      #########################################################    
      
      if(contadorDiferencasTerceira==0)
        qtdAcertouTudoTerceira = qtdAcertouTudoTerceira + 1    
      
    } # if(terceiraMaiorProbabilidade>-1)
    
  } #  i1
  ##########################################
  # fim do Laço principal
  ##########################################
  
  # atualizando o dataframe com base no ArquivoResultado
  df_resultado <- as.data.frame(fromJSON(ArquivoResultado))
  
  #variaveis para guardar no dataframe de resultado de processamento
  df_resultado$totBuracos[linhaResultado] = contadorDeBuracos
  
  df_resultado$totBuracosAcertouPrimeira[linhaResultado] = (contadorDeBuracos-contadorDeBuracosErradosPrimeira)
  df_resultado$totBuracosAcertouSegunda[linhaResultado] = (contadorDeBuracos-contadorDeBuracosErradosSegunda)
  df_resultado$totBuracosAcertouTerceira[linhaResultado] = (contadorDeBuracos-contadorDeBuracosErradosTerceira)
  
  df_resultado$totBuracosErrouPrimeira[linhaResultado] = contadorDeBuracosErradosPrimeira
  df_resultado$totBuracosErrouSegunda[linhaResultado] = contadorDeBuracosErradosSegunda
  df_resultado$totBuracosErrouTerceira[linhaResultado] = contadorDeBuracosErradosTerceira
  
  df_resultado$predicoes100CorretasPrimeira[linhaResultado] = qtdAcertouTudoPrimeira
  df_resultado$predicoes100CorretasSegunda[linhaResultado] = qtdAcertouTudoSegunda
  df_resultado$predicoes100CorretasTerceira[linhaResultado] = qtdAcertouTudoTerceira
  
  df_resultado$acertosgtbPrimeira[linhaResultado] = contadorAcertougtbPrimeira    
  df_resultado$acertosgtbSegunda[linhaResultado]  = contadorAcertougtbSegunda     
  df_resultado$acertosgtbTerceira[linhaResultado] = contadorAcertougtbTerceira    
  
  df_resultado$acertosmq1Primeira[linhaResultado] = contadorAcertoumq1Primeira    
  df_resultado$acertosmq1Segunda[linhaResultado]  = contadorAcertoumq1Segunda     
  df_resultado$acertosmq1Terceira[linhaResultado] = contadorAcertoumq1Terceira    
  
  df_resultado$acertoslsPrimeira[linhaResultado] =  contadorAcertoulsPrimeira     
  df_resultado$acertoslsSegunda[linhaResultado]  =  contadorAcertoulsSegunda      
  df_resultado$acertoslsTerceira[linhaResultado] =  contadorAcertoulsTerceira     
  
  df_resultado$acertosmq2Primeira[linhaResultado] = contadorAcertoumq2Primeira    
  df_resultado$acertosmq2Segunda[linhaResultado]  = contadorAcertoumq2Segunda     
  df_resultado$acertosmq2Terceira[linhaResultado] = contadorAcertoumq2Terceira    
  
  df_resultado$acertoslasPrimeira[linhaResultado] = contadorAcertoulasPrimeira    
  df_resultado$acertoslasSegunda[linhaResultado]  = contadorAcertoulasSegunda     
  df_resultado$acertoslasTerceira[linhaResultado] = contadorAcertoulasTerceira    
  
  df_resultado$acertosoutFogPrimeira[linhaResultado] = contadorAcertououtFogPrimeira    
  df_resultado$acertosoutFogSegunda[linhaResultado]  = contadorAcertououtFogSegunda     
  df_resultado$acertosoutFogTerceira[linhaResultado] = contadorAcertououtFogTerceira    
  
  df_resultado$acertosaa1Primeira[linhaResultado] = contadorAcertouaa1Primeira    
  df_resultado$acertosaa1Segunda[linhaResultado]  = contadorAcertouaa1Segunda     
  df_resultado$acertosaa1Terceira[linhaResultado] = contadorAcertouaa1Terceira    

  df_resultado$errosgtbPrimeira[linhaResultado] = contadorErrougtbPrimeira    
  df_resultado$errosgtbSegunda[linhaResultado]  = contadorErrougtbSegunda     
  df_resultado$errosgtbTerceira[linhaResultado] = contadorErrougtbTerceira    
  
  df_resultado$errosmq1Primeira[linhaResultado] = contadorErroumq1Primeira    
  df_resultado$errosmq1Segunda[linhaResultado]  = contadorErroumq1Segunda     
  df_resultado$errosmq1Terceira[linhaResultado] = contadorErroumq1Terceira    
  
  df_resultado$erroslsPrimeira[linhaResultado] =  contadorErroulsPrimeira     
  df_resultado$erroslsSegunda[linhaResultado]  =  contadorErroulsSegunda      
  df_resultado$erroslsTerceira[linhaResultado] =  contadorErroulsTerceira     
  
  df_resultado$errosmq2Primeira[linhaResultado] = contadorErroumq2Primeira    
  df_resultado$errosmq2Segunda[linhaResultado]  = contadorErroumq2Segunda     
  df_resultado$errosmq2Terceira[linhaResultado] = contadorErroumq2Terceira    
  
  df_resultado$erroslasPrimeira[linhaResultado] = contadorErroulasPrimeira    
  df_resultado$erroslasSegunda[linhaResultado]  = contadorErroulasSegunda     
  df_resultado$erroslasTerceira[linhaResultado] = contadorErroulasTerceira    
  
  df_resultado$errosoutFogPrimeira[linhaResultado] = contadorErrououtFogPrimeira    
  df_resultado$errosoutFogSegunda[linhaResultado]  = contadorErrououtFogSegunda     
  df_resultado$errosoutFogTerceira[linhaResultado] = contadorErrououtFogTerceira    
  
  df_resultado$errosaa1Primeira[linhaResultado] = contadorErrouaa1Primeira    
  df_resultado$errosaa1Segunda[linhaResultado]  = contadorErrouaa1Segunda     
  df_resultado$errosaa1Terceira[linhaResultado] = contadorErrouaa1Terceira    

  # o calculo abaixo pode ter denominador zero e gerar NaN - colocar em um if
  # gateway bridge => gtb
  if((contadorAcertougtbPrimeira + contadorErrougtbPrimeira)>0){
    df_resultado$porcAcertogtbPrimeira[linhaResultado] = (contadorAcertougtbPrimeira/(contadorAcertougtbPrimeira + contadorErrougtbPrimeira))*100.0    
  } else {
    df_resultado$porcAcertogtbPrimeira[linhaResultado] = 0
  }
  
  if((contadorAcertougtbSegunda + contadorErrougtbSegunda)>0){
    df_resultado$porcAcertogtbSegunda[linhaResultado] = (contadorAcertougtbSegunda/(contadorAcertougtbSegunda + contadorErrougtbSegunda))*100.0    
  } else {
    df_resultado$porcAcertogtbSegunda[linhaResultado] = 0
  }
  
  if((contadorAcertougtbTerceira + contadorErrougtbTerceira)>0){
    df_resultado$porcAcertogtbTerceira[linhaResultado] = (contadorAcertougtbTerceira/(contadorAcertougtbTerceira + contadorErrougtbTerceira))*100.0    
  } else {
    df_resultado$porcAcertogtbTerceira[linhaResultado] = 0
  }
  
  # mqqt1 => mq1
  if((contadorAcertoumq1Primeira + contadorErroumq1Primeira)>0){
    df_resultado$porcAcertomq1Primeira[linhaResultado] = (contadorAcertoumq1Primeira/(contadorAcertoumq1Primeira + contadorErroumq1Primeira))*100.0    
  } else {
    df_resultado$porcAcertomq1Primeira[linhaResultado] = 0
  }
  
  if((contadorAcertoumq1Segunda + contadorErroumq1Segunda)>0){
    df_resultado$porcAcertomq1Segunda[linhaResultado] = (contadorAcertoumq1Segunda/(contadorAcertoumq1Segunda + contadorErroumq1Segunda))*100.0    
  } else {
    df_resultado$porcAcertomq1Segunda[linhaResultado] = 0
  }
  
  if((contadorAcertoumq1Terceira + contadorErroumq1Terceira)>0){
    df_resultado$porcAcertomq1Terceira[linhaResultado] = (contadorAcertoumq1Terceira/(contadorAcertoumq1Terceira + contadorErroumq1Terceira))*100.0    
  } else {
    df_resultado$porcAcertomq1Terceira[linhaResultado] = 0
  }
  
  # Lora Server => ls
  if((contadorAcertoulsPrimeira + contadorErroulsPrimeira)>0){
    df_resultado$porcAcertolsPrimeira[linhaResultado] = (contadorAcertoulsPrimeira/(contadorAcertoulsPrimeira + contadorErroulsPrimeira))*100.0    
  } else {
    df_resultado$porcAcertolsPrimeira[linhaResultado] = 0
  }
  
  if((contadorAcertoulsSegunda + contadorErroulsSegunda)>0){
    df_resultado$porcAcertolsSegunda[linhaResultado] = (contadorAcertoulsSegunda/(contadorAcertoulsSegunda + contadorErroulsSegunda))*100.0    
  } else {
    df_resultado$porcAcertolsSegunda[linhaResultado] = 0
  }
  
  if((contadorAcertoulsTerceira + contadorErroulsTerceira)>0){
    df_resultado$porcAcertolsTerceira[linhaResultado] = (contadorAcertoulsTerceira/(contadorAcertoulsTerceira + contadorErroulsTerceira))*100.0    
  } else {
    df_resultado$porcAcertolsTerceira[linhaResultado] = 0
  }
  
  # mqtt2 => mq2
  if((contadorAcertoumq2Primeira + contadorErroumq2Primeira)>0){
    df_resultado$porcAcertomq2Primeira[linhaResultado] = (contadorAcertoumq2Primeira/(contadorAcertoumq2Primeira + contadorErroumq2Primeira))*100.0    
  } else {
    df_resultado$porcAcertomq2Primeira[linhaResultado] = 0
  }
  
  if((contadorAcertoumq2Segunda + contadorErroumq2Segunda)>0){
    df_resultado$porcAcertomq2Segunda[linhaResultado] = (contadorAcertoumq2Segunda/(contadorAcertoumq2Segunda + contadorErroumq2Segunda))*100.0    
  } else {
    df_resultado$porcAcertomq2Segunda[linhaResultado] = 0
  }
  
  if((contadorAcertoumq2Terceira + contadorErroumq2Terceira)>0){
    df_resultado$porcAcertomq2Terceira[linhaResultado] = (contadorAcertoumq2Terceira/(contadorAcertoumq2Terceira + contadorErroumq2Terceira))*100.0    
  } else {
    df_resultado$porcAcertomq2Terceira[linhaResultado] = 0
  }
  
  # lora as => las
  if((contadorAcertoulasPrimeira + contadorErroulasPrimeira)>0){
    df_resultado$porcAcertolasPrimeira[linhaResultado] = (contadorAcertoulasPrimeira/(contadorAcertoulasPrimeira + contadorErroulasPrimeira))*100.0    
  } else {
    df_resultado$porcAcertolasPrimeira[linhaResultado] = 0
  }
  
  if((contadorAcertoulasSegunda + contadorErroulasSegunda)>0){
    df_resultado$porcAcertolasSegunda[linhaResultado] = (contadorAcertoulasSegunda/(contadorAcertoulasSegunda + contadorErroulasSegunda))*100.0    
  } else {
    df_resultado$porcAcertolasSegunda[linhaResultado] = 0
  }
  
  if((contadorAcertoulasTerceira + contadorErroulasTerceira)>0){
    df_resultado$porcAcertolasTerceira[linhaResultado] = (contadorAcertoulasTerceira/(contadorAcertoulasTerceira + contadorErroulasTerceira))*100.0    
  } else {
    df_resultado$porcAcertolasTerceira[linhaResultado] = 0
  }
  
  # saida da Fog => outFog
  if((contadorAcertououtFogPrimeira + contadorErrououtFogPrimeira)>0){
    df_resultado$porcAcertooutFogPrimeira[linhaResultado] = (contadorAcertououtFogPrimeira/(contadorAcertououtFogPrimeira + contadorErrououtFogPrimeira))*100.0    
  } else {
    df_resultado$porcAcertooutFogPrimeira[linhaResultado] = 0
  }
  
  if((contadorAcertououtFogSegunda + contadorErrououtFogSegunda)>0){
    df_resultado$porcAcertooutFogSegunda[linhaResultado] = (contadorAcertououtFogSegunda/(contadorAcertououtFogSegunda + contadorErrououtFogSegunda))*100.0    
  } else {
    df_resultado$porcAcertooutFogSegunda[linhaResultado] = 0
  }
  
  if((contadorAcertououtFogTerceira + contadorErrououtFogTerceira)>0){
    df_resultado$porcAcertooutFogTerceira[linhaResultado] = (contadorAcertououtFogTerceira/(contadorAcertououtFogTerceira + contadorErrououtFogTerceira))*100.0    
  } else {
    df_resultado$porcAcertooutFogTerceira[linhaResultado] = 0
  }
  
  # aa1 => aa1
  if((contadorAcertouaa1Primeira + contadorErrouaa1Primeira)>0){
    df_resultado$porcAcertoaa1Primeira[linhaResultado] = (contadorAcertouaa1Primeira/(contadorAcertouaa1Primeira + contadorErrouaa1Primeira))*100.0    
  } else {
    df_resultado$porcAcertoaa1Primeira[linhaResultado] = 0
  }
  
  if((contadorAcertouaa1Segunda + contadorErrouaa1Segunda)>0){
    df_resultado$porcAcertoaa1Segunda[linhaResultado] = (contadorAcertouaa1Segunda/(contadorAcertouaa1Segunda + contadorErrouaa1Segunda))*100.0    
  } else {
    df_resultado$porcAcertoaa1Segunda[linhaResultado] = 0
  }
  
  if((contadorAcertouaa1Terceira + contadorErrouaa1Terceira)>0){
    df_resultado$porcAcertoaa1Terceira[linhaResultado] = (contadorAcertouaa1Terceira/(contadorAcertouaa1Terceira + contadorErrouaa1Terceira))*100.0    
  } else {
    df_resultado$porcAcertoaa1Terceira[linhaResultado] = 0
  }
  ##################################################################
  # gravação do arquivo de resultado do processamento
  write_json(df_resultado, ArquivoResultado)
  
} # fim da função processaUmArquivoCenario4


##################################################################################
# FUNCTION : processaUmArquivoCenario5
#
# processamento de cada arquivo para cada percentual de buracos para o cenario 5
#
##################################################################################

processaUmArquivoCenario5 <- function (vez,
                                       linhaResultado,
                                       registros,
                                       furos,
                                       df_white,
                                       df_black) {
  
  ###################################################################################
  # carrega a base na variável bn_df como um dataframe
  ###################################################################################
  #
  nomeArquivo = paste(NomeExperimento,"_",registros,".csv",sep="",collapse="")
  bn_df <- read.csv(nomeArquivo)
  
  ###################################################################################
  # Converter as colunas para Factor para a rede bayesiana - função hc()
  ###################################################################################
  # cloud
  bn_df$iot = as.factor(bn_df$iot)
  bn_df$ori = as.factor(bn_df$ori)
  bn_df$mo  = as.factor(bn_df$mo)
  bn_df$ql  = as.factor(bn_df$ql)
  bn_df$cdb = as.factor(bn_df$cdb)
  bn_df$outCloud = as.factor(bn_df$outCloud)
  bn_df$aa2 = as.factor(bn_df$aa2)
  
  ###################################################################################
  # dividir a amostra em duas partes: treino e teste
  ###################################################################################
  #
  divisao = sample.split(bn_df$iot,SplitRatio = 0.5)
  
  base_treinamento      = subset(bn_df, divisao == TRUE)
  base_teste            = subset(bn_df, divisao == FALSE)
  base_teste_sem_buraco = subset(bn_df, divisao == FALSE)
  
  ###################################################################################
  # aplica a função hc na variável bn_df e guarda o resultado na variável res
  # Sobre a função: Agrupamento hierárquico aglomerativo baseado em critérios de 
  # máxima verossimilhança para modelos de mistura gaussiana parametrizados por 
  # decomposição de autovalores.
  ###################################################################################
  # O que eu entendi: está criando a RB com o algoritmo "max-min hill climbing", 
  # que é um algoritmo guloso.
  ###################################################################################
  #
  res = hc(base_treinamento, whitelist = df_white, blacklist = df_black)
  
  ###################################################################################
  # Grava o Plot das dependências funcionais entre as variáveis
  ###################################################################################
  #
  
  nomeArquivoImagem = paste(NomeExperimento,"_imagem.png",sep="",collapse="")
  if(!file.exists(nomeArquivoImagem)){
    png(file = nomeArquivoImagem)
    plot(res)
    dev.off()
  }
  
  ###################################################################################
  # treinar a rede
  # Depois de aprender a estrutura, é preciso descobrir as tabelas de probabilidade
  # condicional (CPTs) em cada nó. A função bn.fit executa o algoritmo EM para 
  # aprender CPT para diferentes nós no gráfico acima.
  ###################################################################################
  #
  #cat("treinar a rede\n")
  fittedbn <- bn.fit(res, data = base_treinamento)
  
  
  ###################################################################################
  # Explorando um dos cálculos
  # Por exemplo, vamos ver o que está dentro do nó s1
  ###################################################################################
  #
  #cat("explorando um dos cálculos\n")
  # fittedbn é um objeto da classe list
  #print(fittedbn)
  # s1 
  #print(fittedbn$s1)
  
  ###################################################################################
  # insere nulos
  ###################################################################################
  #
  porcentagem = furos/100
  
  #cat("Porcentagem=",porcentagem,"\n")
  
  nlin <- nrow(base_teste)
  ncol <- ncol(base_teste)
  totalelementos <- nlin * ncol
  porc_aleatorio <- totalelementos * porcentagem
  
  if(porc_aleatorio<1){
    porc_aleatorio=1
  }
  
  aleatorios <- sort(sample(1:totalelementos, porc_aleatorio))
  
  for (i in 1:porc_aleatorio){
    substituir <- aleatorios[i]
    linsub     <- (substituir %/% ncol)
    colsub     <- (substituir %% ncol)
    if(colsub==0){
      colsub <- ncol
    }
    base_teste[linsub,colsub] <- 'NULL'
  }
  
  #cat("Inseriu nulos\n")  
  
  
  ###################################################################################
  # Verifica se deu linha 100% nula
  ###################################################################################
  #
  contaNulo <- 0
  for (i in 1:nlin){
    for(j in 1:ncol){
      if (is.na  (base_teste[i,j]) || 
          is.null(base_teste[i,j]) ||
          (base_teste[i,j]=="NULL")){
        contaNulo = contaNulo + 1
      }
    }
    if(contaNulo==ncol){
      print("tudo nulo, não da pra fazer a predição")
      contaNulo <- 0
    #stop("tudo nulo, não da pra fazer a predição")
	}
    else
      contaNulo <- 0
  }
  #cat("Verificou linhas todas de buracos")
  
  ###################################################################################
  # Conta total de buracos na base
  # Conta total de linhas sem buracos
  # Conta total de linhas com buracos
  ###################################################################################
  #
  contadorDeBuracos    <- 0
  qtdlinhasSemBuracos  <- 0
  qtdlinhasComBuracos  <- 0
  
  for (i in 1:nlin){
    contAux=0
    for(j in 1:ncol){
      if (is.na  (base_teste[i,j]) || 
          is.null(base_teste[i,j]) ||
          (base_teste[i,j]=="NULL")){
        contAux=contAux+1
        contadorDeBuracos = contadorDeBuracos + 1
      }
    }
    if(contAux==0)
      qtdlinhasSemBuracos = qtdlinhasSemBuracos + 1
    else
      qtdlinhasComBuracos = qtdlinhasComBuracos + 1
  }
  
  #cat("Contou linhas e buracos\n")  
  
  ###################################################################################
  # informa valores para as colunas e suas categorias
  # O ideal seria vir de algum arquivo...
  ###################################################################################
  #
  valores <- list(c('0', '1'),
                  c('0', '1'),
                  c('0', '1'),
                  c('0', '1'),
                  c('0', '1'),
                  c('0', '1'),
                  c('0', '1'))
  
  categorias <- list(c('iot','mo','ori','ql','cdb','aa2','outCloud'))
  
  ###################################################################################
  # inicialização dos contadores gerais do processamento
  ###################################################################################
  #
  
  contadorDeBuracosErradosPrimeira  <- 0
  contadorDeBuracosErradosSegunda   <- 0
  contadorDeBuracosErradosTerceira  <- 0
  qtdAcertouTudoPrimeira            <- 0
  qtdAcertouTudoSegunda             <- 0
  qtdAcertouTudoTerceira            <- 0
  
  contadorAcertouiotPrimeira        <- 0
  contadorAcertouiotSegunda         <- 0
  contadorAcertouiotTerceira        <- 0
  
  contadorAcertoumoPrimeira         <- 0
  contadorAcertoumoSegunda          <- 0
  contadorAcertoumoTerceira         <- 0
  
  contadorAcertouoriPrimeira        <- 0
  contadorAcertouoriSegunda         <- 0
  contadorAcertouoriTerceira        <- 0
  
  contadorAcertouqlPrimeira         <- 0
  contadorAcertouqlSegunda          <- 0
  contadorAcertouqlTerceira         <- 0
  
  contadorAcertoucdbPrimeira        <- 0
  contadorAcertoucdbSegunda         <- 0
  contadorAcertoucdbTerceira        <- 0
  
  contadorAcertouaa2Primeira        <- 0
  contadorAcertouaa2Segunda         <- 0
  contadorAcertouaa2Terceira        <- 0
  
  contadorAcertououtCloudPrimeira        <- 0
  contadorAcertououtCloudSegunda         <- 0
  contadorAcertououtCloudTerceira        <- 0
  
  contadorErrouiotPrimeira          <- 0
  contadorErrouiotSegunda           <- 0
  contadorErrouiotTerceira          <- 0
  
  contadorErroumoPrimeira           <- 0
  contadorErroumoSegunda            <- 0
  contadorErroumoTerceira           <- 0
  
  contadorErrouoriPrimeira          <- 0
  contadorErrouoriSegunda           <- 0
  contadorErrouoriTerceira          <- 0
  
  contadorErrouqlPrimeira           <- 0
  contadorErrouqlSegunda            <- 0
  contadorErrouqlTerceira           <- 0
  
  contadorErroucdbPrimeira          <- 0
  contadorErroucdbSegunda           <- 0
  contadorErroucdbTerceira          <- 0
  
  contadorErrouaa2Primeira          <- 0
  contadorErrouaa2Segunda           <- 0
  contadorErrouaa2Terceira          <- 0
  
  contadorErrououtCloudPrimeira          <- 0
  contadorErrououtCloudSegunda           <- 0
  contadorErrououtCloudTerceira          <- 0
  
  ###################################################################################
  # Laço principal para todas as instâncias
  # do arquivo
  ###################################################################################
  #
  for(i1 in 1:nlin){
    #cat("***Laco Principal - processando linha ", i1, "\n")
    dado <- base_teste[i1,]
    
    #####################################################
    # Verificando se a linha tem buracos
    # se não tiver então pular para a próxima linha
    #####################################################
    qtdBuracosLinha = 0
    
    for (i2 in 1:length(dado)){
      if (is.na(dado[i2]) ||
          is.null(dado[i2]) ||
          (dado[i2]=="NULL"))
        qtdBuracosLinha = qtdBuracosLinha + 1        
    }
    
    
    if(qtdBuracosLinha==0 || 
       qtdBuracosLinha==length(dado) ||
       qtdBuracosLinha/length(dado) > 0.75){ # 24/07/2020
      #não precisa fazer queries ... então ir para o próximo
      #print ("Linha sem buracos... pulada")
      next
    }
    
    
    ##########################################
    # monta as queries
    ##########################################
    
    # limpando as variáveis
    dfv1 <- NULL
    dfv2 <- NULL
    dadoCopia <- NULL
    
    # fazendo uma cópia do dado (dataframe)
    dadoCopia <- dado
    
    # guardando o nome das colunas do dado (dataframe)
    nomeColunas <- colnames(dado)
    
    flag <- TRUE
    for (i2 in length(dado):1)
      if (is.na(dado[i2]) ||
          is.null(dado[i2]) ||
          (dado[i2]=="NULL"))
      {
        dadoCopia <- dadoCopia[,-c(i2)]
        if(flag)
        {
          flag <- FALSE
          dfV1 <- data.frame(valores[i2])
          names(dfV1) <- nomeColunas[i2]
        } else {
          dfV2 <- data.frame(valores[i2])
          names(dfV2) <- nomeColunas[i2]
          dfV1 <- cartesianExpand(dfV1, dfV2)
        }
      }
    
    queries <- as.data.frame(cartesianExpand(dadoCopia, dfV1))
    
    # obs: valores nulos vão no evento e os não nulos nas evidencias
    strEventoColuna    <- NULL
    strEvidenciaColuna <- NULL
    strEventoValor     <- NULL
    strEvidenciaValor  <- NULL
    
    contadorNull = 0
    for (i3 in length(dado):1)
      if (is.na(dado[i3]) ||
          is.null(dado[i3]) ||
          (dado[i3]=="NULL")){
        contadorNull = contadorNull + 1
        strEventoColuna <- c(nomeColunas[i3],strEventoColuna)
      } else {
        strEvidenciaColuna <- c(nomeColunas[i3],strEvidenciaColuna)
        strEvidenciaValor <- c((as.character(dado[1,i3])),strEvidenciaValor)
      }
    
    # devo pegar as três maiores probabilidades
    maiorProbabilidade = -1
    segundaMaiorProbabilidade = -1
    terceiraMaiorProbabilidade = -1
    
    queryMaiorProbabilidade = NULL
    querySegundaMaiorProbabilidade = NULL
    queryTerceiraMaiorProbabilidade = NULL
    
    indiceMaior = -1
    indiceSegundoMaior = -1
    indiceTerceiroMaior = -1
    
    #cat("nrow(queries) ===> ", nrow(queries), "\n")
    
    
    # percorrendo cada linha das queries
    for(i4 in 1:nrow(queries)){
      for(i5 in (length(dado)-contadorNull+1):(length(dado))){
        strEventoValor <- c((as.character(queries[i4,i5])),strEventoValor)
      }
      
      str1 = paste("(", strEventoColuna   , " == \"",strEventoValor   , "\")", sep = "", collapse = " & ")
      str2 = paste("(", strEvidenciaColuna, " == \"",strEvidenciaValor, "\")", sep = "")
      
      cmd = paste("cpquery(fittedbn, event = ", str1, ", evidence = ", str2, ")", sep = "")
      probabilidade = eval(parse(text = cmd))
      
      #cat("probabilidade===>", probabilidade, "\n")
      
      
      #pegando as três maiores probabilidades
      if(probabilidade > maiorProbabilidade){
        # a segunda passa a ser a terceira
        terceiraMaiorProbabilidade = segundaMaiorProbabilidade
        queryTerceiraMaiorProbabilidade = querySegundaMaiorProbabilidade
        indiceTerceiroMaior = indiceSegundoMaior
        
        # a primeira passa a ser a segunda
        segundaMaiorProbabilidade = maiorProbabilidade
        querySegundaMaiorProbabilidade = queryMaiorProbabilidade
        indiceSegundoMaior = indiceMaior
        
        # a nova passa a ser a primeira
        maiorProbabilidade = probabilidade
        queryMaiorProbabilidade = queries[i4,]
        indiceMaior = i4
      } else {
        if(probabilidade > segundaMaiorProbabilidade){
          # a segunda passa a ser a terceira
          terceiraMaiorProbabilidade = segundaMaiorProbabilidade
          queryTerceiraMaiorProbabilidade = querySegundaMaiorProbabilidade
          indiceTerceiroMaior = indiceSegundoMaior
          
          # a nova passa a ser a segunda
          segundaMaiorProbabilidade = probabilidade
          querySegundaMaiorProbabilidade = queries[i4,]
          indiceSegundoMaior = i4
        } else {
          if(probabilidade > terceiraMaiorProbabilidade){
            # a nova passa a ser a terceira
            terceiraMaiorProbabilidade = probabilidade
            queryTerceiraMaiorProbabilidade = queries[i4,]
            indiceTerceiraMaior = i4
          }
        }
      }
      
      strEventoValor <- NULL
    } # for i4
    
    
    ###################################################################################
    #  
    #  __   
    # /_ |  
    #  | |              Analisando os acertos e erros de primeira
    #  | | 
    #  | |
    #  |_|
    #
    ###################################################################################
    if(maiorProbabilidade>-1){
      #########################################################
      #cat ("Maior Probabiliade = ", maiorProbabilidade,"\n")
      #print ("Linha com buracos  = ")
      #print (dado[,order(names(dado))])
      #########################################################    
      
      ## auxiliar para identificar os buracos originais
      aux1 <- dado[,order(names(dado))]
      aux2 <- colnames(aux1)
      
      #########################################################
      #print ("Query selecionada  = ")
      #print (queryMaiorProbabilidade[,order(names(queryMaiorProbabilidade))])
      #print ("Linha sem buracos  = ")
      #print (base_teste_sem_buraco[i1,order(names(bn_df))])
      #########################################################
      contadorDiferencasPrimeira = 0
      ContadorIgualdadesPrimeira = 0
      
      # AA guarda a query com os buracos preenchidos com os valores de maior probabilidade
      AA <- queryMaiorProbabilidade[,order(names(queryMaiorProbabilidade))]
      # BB guarda a linha com os valores originais para verificar se acertou ou não
      BB <- base_teste_sem_buraco[i1,order(names(bn_df))]
      
      for(ww in 1:length(dado)){
        if(as.character(AA[,ww]) == as.character(BB[,ww])){
          if(is.na(aux1[ww]) || is.null(aux1[ww]) || (aux1[ww]=="NULL")){
            ## acertou um buraco
              if(aux2[ww] == "iot"){contadorAcertouiotPrimeira = contadorAcertouiotPrimeira + 1}
            else
              if(aux2[ww] == "mo"){contadorAcertoumoPrimeira = contadorAcertoumoPrimeira + 1}
            else
              if(aux2[ww] == "ori"){contadorAcertouoriPrimeira = contadorAcertouoriPrimeira + 1}
            else
              if(aux2[ww] == "ql"){contadorAcertouqlPrimeira = contadorAcertouqlPrimeira + 1}
            else
              if(aux2[ww] == "cdb"){contadorAcertoucdbPrimeira = contadorAcertoucdbPrimeira + 1}
            else
              if(aux2[ww] == "aa2"){contadorAcertouaa2Primeira = contadorAcertouaa2Primeira + 1}
            else
              if(aux2[ww] == "outCloud"){contadorAcertououtCloudPrimeira = contadorAcertououtCloudPrimeira + 1}
          }
          
          ContadorIgualdadesPrimeira <- ContadorIgualdadesPrimeira + 1
        } else {
          if(is.na(aux1[ww]) || is.null(aux1[ww]) || (aux1[ww]=="NULL")){
            ## errou um buraco
              if(aux2[ww] == "iot"){contadorErrouiotPrimeira = contadorErrouiotPrimeira + 1}
            else
              if(aux2[ww] == "mo"){contadorErroumoPrimeira = contadorErroumoPrimeira + 1}
            else
              if(aux2[ww] == "ori"){contadorErrouoriPrimeira = contadorErrouoriPrimeira + 1}
            else
              if(aux2[ww] == "ql"){contadorErrouqlPrimeira = contadorErrouqlPrimeira + 1}
            else
              if(aux2[ww] == "cdb"){contadorErroucdbPrimeira = contadorErroucdbPrimeira + 1}
            else
              if(aux2[ww] == "aa2"){contadorErrouaa2Primeira = contadorErrouaa2Primeira + 1}
            else
              if(aux2[ww] == "outCloud"){contadorErrououtCloudPrimeira = contadorErrououtCloudPrimeira + 1}
            
          }
          contadorDiferencasPrimeira <- contadorDiferencasPrimeira + 1
          contadorDeBuracosErradosPrimeira = contadorDeBuracosErradosPrimeira + 1
          #########################################################        
          #cat("Q: ",colnames(AA[ww]),"...:",as.character(AA[,ww]),"\t",
          #    "B: ",colnames(BB[ww]),"...:",as.character(BB[,ww]),"\n")
          #########################################################        
        }
      } # for ww
      
      #########################################################    
      #cat ("Qtde de Buracos certos  Primeira  = ", (qtdBuracosLinha-contadorDiferencasPrimeira), "\n")
      #cat ("Qtde de Buracos errados Primeira  = ", contadorDiferencasPrimeira, "\n")
      #cat ("Qtde de Buracos                   = ", qtdBuracosLinha, "\n")
      #########################################################    
      
      if(contadorDiferencasPrimeira==0)
        qtdAcertouTudoPrimeira = qtdAcertouTudoPrimeira + 1    
      
    } # if(maiorProbabilidade>-1)
    
    
    ###################################################################################
    #  
    #  ___  
    # |__ \
    #    ) |            Analisando os acertos e erros de Segunda
    #   / / 
    #  / /_ 
    # |____|
    #
    ###################################################################################
    if(segundaMaiorProbabilidade>-1){
      #########################################################
      #cat ("Segunda Maior Probabiliade = ", segundaMaiorProbabilidade,"\n")
      #print ("Linha com buracos  = ")
      #print (dado[,order(names(dado))])
      #########################################################    
      
      ## auxiliar para identificar os buracos originais
      aux1 <- dado[,order(names(dado))]
      aux2 <- colnames(aux1)
      
      #########################################################
      #print ("Query selecionada  = ")
      #print (querySegundaMaiorProbabilidade[,order(names(querySegundaMaiorProbabilidade))])
      #print ("Linha sem buracos  = ")
      #print (base_teste_sem_buraco[i1,order(names(bn_df))])
      #########################################################
      contadorDiferencasSegunda = 0
      ContadorIgualdadesSegunda = 0
      
      # AA guarda a query com os buracos preenchidos com os valores de maior probabilidade
      AA <- querySegundaMaiorProbabilidade[,order(names(querySegundaMaiorProbabilidade))]
      # BB guarda a linha com os valores originais para verificar se acertou ou não
      BB <- base_teste_sem_buraco[i1,order(names(bn_df))]
      
      for(ww in 1:length(dado)){
        if(as.character(AA[,ww]) == as.character(BB[,ww])){
          if(is.na(aux1[ww]) || is.null(aux1[ww]) || (aux1[ww]=="NULL")){
            ## acertou um buraco
              if(aux2[ww] == "iot"){contadorAcertouiotSegunda = contadorAcertouiotSegunda + 1}
            else
              if(aux2[ww] == "mo"){contadorAcertoumoSegunda = contadorAcertoumoSegunda + 1}
            else
              if(aux2[ww] == "ori"){contadorAcertouoriSegunda = contadorAcertouoriSegunda + 1}
            else
              if(aux2[ww] == "ql"){contadorAcertouqlSegunda = contadorAcertouqlSegunda + 1}
            else
              if(aux2[ww] == "cdb"){contadorAcertoucdbSegunda = contadorAcertoucdbSegunda + 1}
            else
              if(aux2[ww] == "aa2"){contadorAcertouaa2Segunda = contadorAcertouaa2Segunda + 1}
            else
              if(aux2[ww] == "outCloud"){contadorAcertououtCloudSegunda = contadorAcertououtCloudSegunda + 1}
          }
          
          ContadorIgualdadesSegunda <- ContadorIgualdadesSegunda + 1
        } else {
          if(is.na(aux1[ww]) || is.null(aux1[ww]) || (aux1[ww]=="NULL")){
            ## errou um buraco
              if(aux2[ww] == "iot"){contadorErrouiotSegunda = contadorErrouiotSegunda + 1}
            else
              if(aux2[ww] == "mo"){contadorErroumoSegunda = contadorErroumoSegunda + 1}
            else
              if(aux2[ww] == "ori"){contadorErrouoriSegunda = contadorErrouoriSegunda + 1}
            else
              if(aux2[ww] == "ql"){contadorErrouqlSegunda = contadorErrouqlSegunda + 1}
            else
              if(aux2[ww] == "cdb"){contadorErroucdbSegunda = contadorErroucdbSegunda + 1}
            else
              if(aux2[ww] == "aa2"){contadorErrouaa2Segunda = contadorErrouaa2Segunda + 1}
            else
              if(aux2[ww] == "outCloud"){contadorErrououtCloudSegunda = contadorErrououtCloudSegunda + 1}
          }
          contadorDiferencasSegunda <- contadorDiferencasSegunda + 1
          contadorDeBuracosErradosSegunda = contadorDeBuracosErradosSegunda + 1
          #########################################################        
          #cat("Q: ",colnames(AA[ww]),"...:",as.character(AA[,ww]),"\t",
          #    "B: ",colnames(BB[ww]),"...:",as.character(BB[,ww]),"\n")
          #########################################################        
        }
      } # for ww
      
      
      #########################################################    
      #cat ("Qtde de Buracos certos  Segunda = ", (qtdBuracosLinha-contadorDiferencasSegunda), "\n")
      #cat ("Qtde de Buracos errados Segunda = ", contadorDiferencasSegunda, "\n")
      #cat ("Qtde de Buracos                 = ", qtdBuracosLinha, "\n")
      #########################################################    
      
      if(contadorDiferencasSegunda==0)
        qtdAcertouTudoSegunda = qtdAcertouTudoSegunda + 1    
      
    } # if(segundaMaiorProbabilidade>-1) - cenario 5
    
    ###################################################################################
    #  
    #  ____  
    # |___ \   
    #   __) |           Analisando os acertos e erros de Terceira
    #  |__ < 
    #  ___) |
    # |____/
    #
    ###################################################################################
    if(terceiraMaiorProbabilidade>-1){
      #########################################################
      #cat ("Terceira Maior Probabiliade = ", terceiraMaiorProbabilidade,"\n")
      #print ("Linha com buracos  = ")
      #print (dado[,order(names(dado))])
      #########################################################    
      
      ## auxiliar para identificar os buracos originais
      aux1 <- dado[,order(names(dado))]
      aux2 <- colnames(aux1)
      
      #########################################################
      #print ("Query selecionada  = ")
      #print (queryTerceiraMaiorProbabilidade[,order(names(queryTerceiraMaiorProbabilidade))])
      #print ("Linha sem buracos  = ")
      #print (base_teste_sem_buraco[i1,order(names(bn_df))])
      #########################################################
      contadorDiferencasTerceira = 0
      ContadorIgualdadesTerceira = 0
      
      # AA guarda a query com os buracos preenchidos com os valores de maior probabilidade
      AA <- queryTerceiraMaiorProbabilidade[,order(names(queryTerceiraMaiorProbabilidade))]
      # BB guarda a linha com os valores originais para verificar se acertou ou não
      BB <- base_teste_sem_buraco[i1,order(names(bn_df))]
      
      for(ww in 1:length(dado)){
        if(as.character(AA[,ww]) == as.character(BB[,ww])){
          if(is.na(aux1[ww]) || is.null(aux1[ww]) || (aux1[ww]=="NULL")){
            ## acertou um buraco
              if(aux2[ww] == "iot"){contadorAcertouiotTerceira = contadorAcertouiotTerceira + 1}
            else
              if(aux2[ww] == "mo"){contadorAcertoumoTerceira = contadorAcertoumoTerceira + 1}
            else
              if(aux2[ww] == "ori"){contadorAcertouoriTerceira = contadorAcertouoriTerceira + 1}
            else
              if(aux2[ww] == "ql"){contadorAcertouqlTerceira = contadorAcertouqlTerceira + 1}
            else
              if(aux2[ww] == "cdb"){contadorAcertoucdbTerceira = contadorAcertoucdbTerceira + 1}
            else
              if(aux2[ww] == "aa2"){contadorAcertouaa2Terceira = contadorAcertouaa2Terceira + 1}
            else
              if(aux2[ww] == "outCloud"){contadorAcertououtCloudTerceira = contadorAcertououtCloudTerceira + 1}
          }
          
          ContadorIgualdadesTerceira <- ContadorIgualdadesTerceira + 1
        } else {
          if(is.na(aux1[ww]) || is.null(aux1[ww]) || (aux1[ww]=="NULL")){
            ## errou um buraco
              if(aux2[ww] == "iot"){contadorErrouiotTerceira = contadorErrouiotTerceira + 1}
            else
              if(aux2[ww] == "mo"){contadorErroumoTerceira = contadorErroumoTerceira + 1}
            else
              if(aux2[ww] == "ori"){contadorErrouoriTerceira = contadorErrouoriTerceira + 1}
            else
              if(aux2[ww] == "ql"){contadorErrouqlTerceira = contadorErrouqlTerceira + 1}
            else
              if(aux2[ww] == "cdb"){contadorErroucdbTerceira = contadorErroucdbTerceira + 1}
            else
              if(aux2[ww] == "aa2"){contadorErrouaa2Terceira = contadorErrouaa2Terceira + 1}
            else
              if(aux2[ww] == "outCloud"){contadorErrououtCloudTerceira = contadorErrououtCloudTerceira + 1}
          }
          contadorDiferencasTerceira <- contadorDiferencasTerceira + 1
          contadorDeBuracosErradosTerceira = contadorDeBuracosErradosTerceira + 1
          #########################################################        
          #cat("Q: ",colnames(AA[ww]),"...:",as.character(AA[,ww]),"\t",
          #    "B: ",colnames(BB[ww]),"...:",as.character(BB[,ww]),"\n")
          #########################################################        
        }
      } # for ww
      
      
      #########################################################    
      #cat ("Qtde de Buracos certos  Terceira = ", (qtdBuracosLinha-contadorDiferencasTerceira), "\n")
      #cat ("Qtde de Buracos errados Terceira = ", contadorDiferencasTerceira, "\n")
      #cat ("Qtde de Buracos                  = ", qtdBuracosLinha, "\n")
      #########################################################    
      
      if(contadorDiferencasTerceira==0)
        qtdAcertouTudoTerceira = qtdAcertouTudoTerceira + 1    
      
    } # if(terceiraMaiorProbabilidade>-1)
    
  } #  i1
  ##########################################
  # fim do Laço principal
  ##########################################
  
  # atualizando o dataframe com base no ArquivoResultado
  df_resultado <- as.data.frame(fromJSON(ArquivoResultado))
  
  #variaveis para guardar no dataframe de resultado de processamento
  df_resultado$totBuracos[linhaResultado] = contadorDeBuracos
  
  df_resultado$totBuracosAcertouPrimeira[linhaResultado] = (contadorDeBuracos-contadorDeBuracosErradosPrimeira)
  df_resultado$totBuracosAcertouSegunda[linhaResultado] = (contadorDeBuracos-contadorDeBuracosErradosSegunda)
  df_resultado$totBuracosAcertouTerceira[linhaResultado] = (contadorDeBuracos-contadorDeBuracosErradosTerceira)
  
  df_resultado$totBuracosErrouPrimeira[linhaResultado] = contadorDeBuracosErradosPrimeira
  df_resultado$totBuracosErrouSegunda[linhaResultado] = contadorDeBuracosErradosSegunda
  df_resultado$totBuracosErrouTerceira[linhaResultado] = contadorDeBuracosErradosTerceira
  
  df_resultado$predicoes100CorretasPrimeira[linhaResultado] = qtdAcertouTudoPrimeira
  df_resultado$predicoes100CorretasSegunda[linhaResultado] = qtdAcertouTudoSegunda
  df_resultado$predicoes100CorretasTerceira[linhaResultado] = qtdAcertouTudoTerceira

  df_resultado$acertosiotPrimeira[linhaResultado] = contadorAcertouiotPrimeira    
  df_resultado$acertosiotSegunda[linhaResultado]  = contadorAcertouiotSegunda     
  df_resultado$acertosiotTerceira[linhaResultado] = contadorAcertouiotTerceira    
  
  df_resultado$acertosmoPrimeira[linhaResultado] = contadorAcertoumoPrimeira     
  df_resultado$acertosmoSegunda[linhaResultado]  = contadorAcertoumoSegunda      
  df_resultado$acertosmoTerceira[linhaResultado] = contadorAcertoumoTerceira     
  
  df_resultado$acertosoriPrimeira[linhaResultado] = contadorAcertouoriPrimeira    
  df_resultado$acertosoriSegunda[linhaResultado]  = contadorAcertouoriSegunda     
  df_resultado$acertosoriTerceira[linhaResultado] = contadorAcertouoriTerceira    
  
  df_resultado$acertosqlPrimeira[linhaResultado] =  contadorAcertouqlPrimeira     
  df_resultado$acertosqlSegunda[linhaResultado]  =  contadorAcertouqlSegunda      
  df_resultado$acertosqlTerceira[linhaResultado] =  contadorAcertouqlTerceira     
  
  df_resultado$acertoscdbPrimeira[linhaResultado] = contadorAcertoucdbPrimeira    
  df_resultado$acertoscdbSegunda[linhaResultado]  = contadorAcertoucdbSegunda     
  df_resultado$acertoscdbTerceira[linhaResultado] = contadorAcertoucdbTerceira    
  
  df_resultado$acertosaa2Primeira[linhaResultado] = contadorAcertouaa2Primeira    
  df_resultado$acertosaa2Segunda[linhaResultado]  = contadorAcertouaa2Segunda     
  df_resultado$acertosaa2Terceira[linhaResultado] = contadorAcertouaa2Terceira
  
  df_resultado$acertosoutCloudPrimeira[linhaResultado] = contadorAcertououtCloudPrimeira    
  df_resultado$acertosoutCloudSegunda[linhaResultado]  = contadorAcertououtCloudSegunda     
  df_resultado$acertosoutCloudTerceira[linhaResultado] = contadorAcertououtCloudTerceira
  
  df_resultado$errosiotPrimeira[linhaResultado] = contadorErrouiotPrimeira    
  df_resultado$errosiotSegunda[linhaResultado]  = contadorErrouiotSegunda     
  df_resultado$errosiotTerceira[linhaResultado] = contadorErrouiotTerceira    
  
  df_resultado$errosmoPrimeira[linhaResultado] = contadorErroumoPrimeira     
  df_resultado$errosmoSegunda[linhaResultado]  = contadorErroumoSegunda      
  df_resultado$errosmoTerceira[linhaResultado] = contadorErroumoTerceira     
  
  df_resultado$errosoriPrimeira[linhaResultado] = contadorErrouoriPrimeira    
  df_resultado$errosoriSegunda[linhaResultado]  = contadorErrouoriSegunda     
  df_resultado$errosoriTerceira[linhaResultado] = contadorErrouoriTerceira    
  
  df_resultado$errosqlPrimeira[linhaResultado] =  contadorErrouqlPrimeira     
  df_resultado$errosqlSegunda[linhaResultado]  =  contadorErrouqlSegunda      
  df_resultado$errosqlTerceira[linhaResultado] =  contadorErrouqlTerceira     
  
  df_resultado$erroscdbPrimeira[linhaResultado] = contadorErroucdbPrimeira    
  df_resultado$erroscdbSegunda[linhaResultado]  = contadorErroucdbSegunda     
  df_resultado$erroscdbTerceira[linhaResultado] = contadorErroucdbTerceira    
  
  df_resultado$errosaa2Primeira[linhaResultado] = contadorErrouaa2Primeira    
  df_resultado$errosaa2Segunda[linhaResultado]  = contadorErrouaa2Segunda     
  df_resultado$errosaa2Terceira[linhaResultado] = contadorErrouaa2Terceira
  
  df_resultado$errosoutCloudPrimeira[linhaResultado] = contadorErrououtCloudPrimeira    
  df_resultado$errosoutCloudSegunda[linhaResultado]  = contadorErrououtCloudSegunda     
  df_resultado$errosoutCloudTerceira[linhaResultado] = contadorErrououtCloudTerceira
  
  
  # o calculo abaixo pode ter denominador zero e gerar NaN - colocar em um if

  # iot Agent => iot
  if((contadorAcertouiotPrimeira + contadorErrouiotPrimeira)>0){
    df_resultado$porcAcertoiotPrimeira[linhaResultado] = (contadorAcertouiotPrimeira/(contadorAcertouiotPrimeira + contadorErrouiotPrimeira))*100.0    
  } else {
    df_resultado$porcAcertoiotPrimeira[linhaResultado] = 0
  }
  
  if((contadorAcertouiotSegunda + contadorErrouiotSegunda)>0){
    df_resultado$porcAcertoiotSegunda[linhaResultado] = (contadorAcertouiotSegunda/(contadorAcertouiotSegunda + contadorErrouiotSegunda))*100.0    
  } else {
    df_resultado$porcAcertoiotSegunda[linhaResultado] = 0
  }
  
  if((contadorAcertouiotTerceira + contadorErrouiotTerceira)>0){
    df_resultado$porcAcertoiotTerceira[linhaResultado] = (contadorAcertouiotTerceira/(contadorAcertouiotTerceira + contadorErrouiotTerceira))*100.0    
  } else {
    df_resultado$porcAcertoiotTerceira[linhaResultado] = 0
  }
  
  # mongo => mo
  if((contadorAcertoumoPrimeira + contadorErroumoPrimeira)>0){
    df_resultado$porcAcertomoPrimeira[linhaResultado] = (contadorAcertoumoPrimeira/(contadorAcertoumoPrimeira + contadorErroumoPrimeira))*100.0    
  } else {
    df_resultado$porcAcertomoPrimeira[linhaResultado] = 0
  }
  
  if((contadorAcertoumoSegunda + contadorErroumoSegunda)>0){
    df_resultado$porcAcertomoSegunda[linhaResultado] = (contadorAcertoumoSegunda/(contadorAcertoumoSegunda + contadorErroumoSegunda))*100.0    
  } else {
    df_resultado$porcAcertomoSegunda[linhaResultado] = 0
  }
  
  if((contadorAcertoumoTerceira + contadorErroumoTerceira)>0){
    df_resultado$porcAcertomoTerceira[linhaResultado] = (contadorAcertoumoTerceira/(contadorAcertoumoTerceira + contadorErroumoTerceira))*100.0    
  } else {
    df_resultado$porcAcertomoTerceira[linhaResultado] = 0
  }
  
  # orion => ori
  if((contadorAcertouoriPrimeira + contadorErrouoriPrimeira)>0){
    df_resultado$porcAcertooriPrimeira[linhaResultado] = (contadorAcertouoriPrimeira/(contadorAcertouoriPrimeira + contadorErrouoriPrimeira))*100.0    
  } else {
    df_resultado$porcAcertooriPrimeira[linhaResultado] = 0
  }
  
  if((contadorAcertouoriSegunda + contadorErrouoriSegunda)>0){
    df_resultado$porcAcertooriSegunda[linhaResultado] = (contadorAcertouoriSegunda/(contadorAcertouoriSegunda + contadorErrouoriSegunda))*100.0    
  } else {
    df_resultado$porcAcertooriSegunda[linhaResultado] = 0
  }
  
  if((contadorAcertouoriTerceira + contadorErrouoriTerceira)>0){
    df_resultado$porcAcertooriTerceira[linhaResultado] = (contadorAcertouoriTerceira/(contadorAcertouoriTerceira + contadorErrouoriTerceira))*100.0    
  } else {
    df_resultado$porcAcertooriTerceira[linhaResultado] = 0
  }
  
  # ql => ql
  if((contadorAcertouqlPrimeira + contadorErrouqlPrimeira)>0){
    df_resultado$porcAcertoqlPrimeira[linhaResultado] = (contadorAcertouqlPrimeira/(contadorAcertouqlPrimeira + contadorErrouqlPrimeira))*100.0    
  } else {
    df_resultado$porcAcertoqlPrimeira[linhaResultado] = 0
  }
  
  if((contadorAcertouqlSegunda + contadorErrouqlSegunda)>0){
    df_resultado$porcAcertoqlSegunda[linhaResultado] = (contadorAcertouqlSegunda/(contadorAcertouqlSegunda + contadorErrouqlSegunda))*100.0    
  } else {
    df_resultado$porcAcertoqlSegunda[linhaResultado] = 0
  }
  
  if((contadorAcertouqlTerceira + contadorErrouqlTerceira)>0){
    df_resultado$porcAcertoqlTerceira[linhaResultado] = (contadorAcertouqlTerceira/(contadorAcertouqlTerceira + contadorErrouqlTerceira))*100.0    
  } else {
    df_resultado$porcAcertoqlTerceira[linhaResultado] = 0
  }
  
  # Crate DB => cdb
  if((contadorAcertoucdbPrimeira + contadorErroucdbPrimeira)>0){
    df_resultado$porcAcertocdbPrimeira[linhaResultado] = (contadorAcertoucdbPrimeira/(contadorAcertoucdbPrimeira + contadorErroucdbPrimeira))*100.0    
  } else {
    df_resultado$porcAcertocdbPrimeira[linhaResultado] = 0
  }
  
  if((contadorAcertoucdbSegunda + contadorErroucdbSegunda)>0){
    df_resultado$porcAcertocdbSegunda[linhaResultado] = (contadorAcertoucdbSegunda/(contadorAcertoucdbSegunda + contadorErroucdbSegunda))*100.0    
  } else {
    df_resultado$porcAcertocdbSegunda[linhaResultado] = 0
  }
  
  if((contadorAcertoucdbTerceira + contadorErroucdbTerceira)>0){
    df_resultado$porcAcertocdbTerceira[linhaResultado] = (contadorAcertoucdbTerceira/(contadorAcertoucdbTerceira + contadorErroucdbTerceira))*100.0    
  } else {
    df_resultado$porcAcertocdbTerceira[linhaResultado] = 0
  }
  
  # aa2 => aa2
  if((contadorAcertouaa2Primeira + contadorErrouaa2Primeira)>0){
    df_resultado$porcAcertoaa2Primeira[linhaResultado] = (contadorAcertouaa2Primeira/(contadorAcertouaa2Primeira + contadorErrouaa2Primeira))*100.0    
  } else {
    df_resultado$porcAcertoaa2Primeira[linhaResultado] = 0
  }
  
  if((contadorAcertouaa2Segunda + contadorErrouaa2Segunda)>0){
    df_resultado$porcAcertoaa2Segunda[linhaResultado] = (contadorAcertouaa2Segunda/(contadorAcertouaa2Segunda + contadorErrouaa2Segunda))*100.0    
  } else {
    df_resultado$porcAcertoaa2Segunda[linhaResultado] = 0
  }
  
  if((contadorAcertouaa2Terceira + contadorErrouaa2Terceira)>0){
    df_resultado$porcAcertoaa2Terceira[linhaResultado] = (contadorAcertouaa2Terceira/(contadorAcertouaa2Terceira + contadorErrouaa2Terceira))*100.0    
  } else {
    df_resultado$porcAcertoaa2Terceira[linhaResultado] = 0
  }
  
  # saída da Cloud => outCloud
  if((contadorAcertououtCloudPrimeira + contadorErrououtCloudPrimeira)>0){
    df_resultado$porcAcertooutCloudPrimeira[linhaResultado] = (contadorAcertououtCloudPrimeira/(contadorAcertououtCloudPrimeira + contadorErrououtCloudPrimeira))*100.0    
  } else {
    df_resultado$porcAcertooutCloudPrimeira[linhaResultado] = 0
  }
  
  if((contadorAcertououtCloudSegunda + contadorErrououtCloudSegunda)>0){
    df_resultado$porcAcertooutCloudSegunda[linhaResultado] = (contadorAcertououtCloudSegunda/(contadorAcertououtCloudSegunda + contadorErrououtCloudSegunda))*100.0    
  } else {
    df_resultado$porcAcertooutCloudSegunda[linhaResultado] = 0
  }
  
  if((contadorAcertououtCloudTerceira + contadorErrououtCloudTerceira)>0){
    df_resultado$porcAcertooutCloudTerceira[linhaResultado] = (contadorAcertououtCloudTerceira/(contadorAcertououtCloudTerceira + contadorErrououtCloudTerceira))*100.0    
  } else {
    df_resultado$porcAcertooutCloudTerceira[linhaResultado] = 0
  }
  ##################################################################
  # gravação do arquivo de resultado do processamento
  write_json(df_resultado, ArquivoResultado)
  
} # fim da função processaUmArquivoCenario5

##################################################################################
# FUNCTION : geraBlackList
#
# com base na combinação 2 a 2 dos elementos (matriz_combinatoria)
# elimina os elementos da White List, gravando o que sobra na Black List
##################################################################################
geraBlackList <- function(matriz_arranjo, df_white, df_black){
  i = 0
  for(j in 1:nrow(matriz_arranjo)){
     achou = FALSE
      
     for(k in 1:nrow(df_white)){
        if(matriz_arranjo[j,1] == df_white[k,1] &&
           matriz_arranjo[j,2] == df_white[k,2] ){
          achou = TRUE
          break
        }
     } # for k
      
     if(achou==FALSE){
        i <- i + 1
        df_black[i,1] <- matriz_arranjo[j,1]
        df_black[i,2] <- matriz_arranjo[j,2]
     }
  } # for J

  return (df_black)

}


##################################################################################
# FUNCTION : processaUmArquivoCenario6
#
# processamento de cada arquivo para cada percentual de buracos para o cenario 6
#
##################################################################################

processaUmArquivoCenario6 <- function (vez,
                                       linhaResultado,
                                       registros,
                                       furos,
                                       df_white,
                                       df_black) {
  
  ###################################################################################
  # carrega a base na variável bn_df como um dataframe
  ###################################################################################
  # 
  nomeArquivo = paste(NomeExperimento,"_",registros,".csv",sep="",collapse="")
  bn_df <- read.csv(nomeArquivo)  
  
  ###################################################################################
  # Converter as colunas para Factor para a rede bayesiana - função hc()
  ###################################################################################
  #
  bn_df$ss1       = as.factor(bn_df$ss1)
  bn_df$ss2       = as.factor(bn_df$ss2)
  bn_df$somafusao = as.factor(bn_df$somafusao)
  bn_df$acao      = as.factor(bn_df$acao)
  bn_df$a1        = as.factor(bn_df$a1)
  bn_df$a2        = as.factor(bn_df$a2)

  ###################################################################################
  # dividir a amostra em duas partes: treino e teste
  ###################################################################################
  #
  divisao = sample.split(bn_df$ss1,SplitRatio = 0.5)
  
  
  base_treinamento      = subset(bn_df, divisao == TRUE)
  base_teste            = subset(bn_df, divisao == FALSE)
  base_teste_sem_buraco = subset(bn_df, divisao == FALSE)
  
  ###################################################################################
  # aplica a função hc na variável bn_df e guarda o resultado na variável res
  # Sobre a função: Agrupamento hierárquico aglomerativo baseado em critérios de 
  # máxima verossimilhança para modelos de mistura gaussiana parametrizados por 
  # decomposição de autovalores.
  ###################################################################################
  # O que eu entendi: está criando a RB com o algoritmo "max-min hill climbing", 
  # que é um algoritmo guloso.
  ###################################################################################
  #
  res = hc(base_treinamento, whitelist = df_white, blacklist = df_black)
  
  ###################################################################################
  # Grava o Plot das dependências funcionais entre as variáveis
  ###################################################################################
  #
  nomeArquivoImagem = paste(NomeExperimento,"_imagem.png",sep="",collapse="")
  if(!file.exists(nomeArquivoImagem)){
    png(file = nomeArquivoImagem)
    plot(res)
    dev.off()
  }
  
  
  ###################################################################################
  # treinar a rede
  # Depois de aprender a estrutura, é preciso descobrir as tabelas de probabilidade
  # condicional (CPTs) em cada nó. A função bn.fit executa o algoritmo EM para 
  # aprender CPT para diferentes nós no gráfico acima.
  ###################################################################################
  #
  #cat("treinar a rede\n")
  fittedbn <- bn.fit(res, data = base_treinamento)
  
  ###################################################################################
  # Explorando um dos cálculos
  # Por exemplo, vamos ver o que está dentro do nó ss1
  ###################################################################################
  #
  #cat("explorando um dos cálculos\n")
  # fittedbn é um objeto da classe list
  #print(fittedbn)
  # ss1 
  #print(fittedbn$ss1)
  
  
  ###################################################################################
  # insere nulos
  ###################################################################################
  #
  porcentagem = furos/100
  nlin <- nrow(base_teste)
  ncol <- ncol(base_teste)
  totalelementos <- nlin * ncol
  porc_aleatorio <- totalelementos * porcentagem
  
  if(porc_aleatorio<1){
    porc_aleatorio=1
  }
  
  aleatorios <- sort(sample(1:totalelementos, porc_aleatorio))
  
  for (i in 1:porc_aleatorio){
    substituir <- aleatorios[i]
    linsub     <- (substituir %/% ncol)
    colsub     <- (substituir %% ncol)
    if(colsub==0){
      colsub <- ncol
    }
    base_teste[linsub,colsub] <- 'NULL'
  }
  
  #cat("Inseriu nulos\n")  
  ###################################################################################
  # Verifica se deu linha 100% nula
  ###################################################################################
  #
  contaNulo <- 0
  for (i in 1:nlin){
    for(j in 1:ncol){
      if (is.na  (base_teste[i,j]) || 
          is.null(base_teste[i,j]) ||
          (base_teste[i,j]=="NULL")){
        contaNulo = contaNulo + 1
      }
    }
    if(contaNulo==ncol){
      contaNulo <- 0 # 24/07/2020 
      print("tudo nulo, não da pra fazer a predição")
      #stop("tudo nulo, não da pra fazer a predição")
    }
    else
      contaNulo <- 0
  }
  
  #cat("Verificou linhas todas de buracos")
  ###################################################################################
  # Conta total de buracos na base
  # Conta total de linhas sem buracos
  # Conta total de linhas com buracos
  ###################################################################################
  #
  contadorDeBuracos    <- 0
  qtdlinhasSemBuracos  <- 0
  qtdlinhasComBuracos  <- 0
  
  for (i in 1:nlin){
    contAux=0
    for(j in 1:ncol){
      if (is.na  (base_teste[i,j]) || 
          is.null(base_teste[i,j]) ||
          (base_teste[i,j]=="NULL")){
        contAux=contAux+1
        contadorDeBuracos = contadorDeBuracos + 1
      }
    }
    if(contAux==0)
      qtdlinhasSemBuracos = qtdlinhasSemBuracos + 1
    else
      qtdlinhasComBuracos = qtdlinhasComBuracos + 1
  }
  
  #cat("Contou linhas e buracos\n")  
  
  ###################################################################################
  # informa valores para as colunas e suas categorias
  # O ideal seria vir de algum arquivo...
  ###################################################################################
  #
  valores <- list(c('0', '1', 'D'),
                  c('0', '1', 'D'),
                  c('0','1', '2', '3'),
                  c('00', '01', '10', '11'),
                  c('0', '1', 'D'),
                  c('0', '1', 'D'))
  
  categorias <- list(c('ss1','ss2'),
                     c('somafusao'),
                     c('acao'),
                     c('a1','a2'))
  
  ###################################################################################
  # inicialização dos contadores gerais do processamento
  ###################################################################################
  #
  contadorDeBuracosErradosPrimeira  <- 0
  contadorDeBuracosErradosSegunda   <- 0
  contadorDeBuracosErradosTerceira  <- 0
  qtdAcertouTudoPrimeira            <- 0
  qtdAcertouTudoSegunda             <- 0
  qtdAcertouTudoTerceira            <- 0
  
  contadorAcertouAtuador1Primeira   <- 0
  contadorAcertouAtuador1Segunda    <- 0
  contadorAcertouAtuador1Terceira   <- 0

  contadorAcertouAtuador2Primeira   <- 0
  contadorAcertouAtuador2Segunda    <- 0
  contadorAcertouAtuador2Terceira   <- 0
  
  contadorAcertouAcaoPrimeira       <- 0
  contadorAcertouAcaoSegunda        <- 0
  contadorAcertouAcaoTerceira       <- 0
  
  contadorAcertouSomafusaoPrimeira  <- 0
  contadorAcertouSomafusaoSegunda   <- 0
  contadorAcertouSomafusaoTerceira  <- 0
  
  contadorAcertouSensor1Primeira    <- 0
  contadorAcertouSensor1Segunda     <- 0
  contadorAcertouSensor1Terceira    <- 0
  
  contadorAcertouSensor2Primeira    <- 0
  contadorAcertouSensor2Segunda     <- 0
  contadorAcertouSensor2Terceira    <- 0
  
  contadorErrouAtuador1Primeira     <- 0
  contadorErrouAtuador1Segunda      <- 0
  contadorErrouAtuador1Terceira     <- 0
  
  contadorErrouAtuador2Primeira     <- 0
  contadorErrouAtuador2Segunda      <- 0
  contadorErrouAtuador2Terceira     <- 0
  
  contadorErrouAcaoPrimeira         <- 0
  contadorErrouAcaoSegunda          <- 0
  contadorErrouAcaoTerceira         <- 0
  
  contadorErrouSomafusaoPrimeira    <- 0
  contadorErrouSomafusaoSegunda     <- 0
  contadorErrouSomafusaoTerceira    <- 0
  
  contadorErrouSensor1Primeira      <- 0
  contadorErrouSensor1Segunda       <- 0
  contadorErrouSensor1Terceira      <- 0
  
  contadorErrouSensor2Primeira      <- 0
  contadorErrouSensor2Segunda       <- 0
  contadorErrouSensor2Terceira      <- 0
  
  ###################################################################################
  # Laço principal para todas as instâncias
  # do arquivo
  ###################################################################################
  #
  for(i1 in 1:nlin){
    #cat("***Laco Principal - processando linha ", i1, "\n")
    dado <- base_teste[i1,]
    
    #####################################################
    # Verificando se a linha tem buracos
    # se não tiver então pular para a próxima linha
    #####################################################
    qtdBuracosLinha = 0
    
    for (i2 in 1:length(dado)){
      if (is.na(dado[i2]) ||
          is.null(dado[i2]) ||
          (dado[i2]=="NULL"))
        qtdBuracosLinha = qtdBuracosLinha + 1        
    }
    
    
    if(qtdBuracosLinha==0 || 
       qtdBuracosLinha==length(dado) ||
       qtdBuracosLinha/length(dado) > 0.75){ # 24/07/2020 - se não tem buraco ou só tem buraco pular
      #não precisa fazer queries ... então ir para o próximo
      #print ("Linha sem buracos... pulada")
      next
    }
    
    ##########################################
    # monta as queries
    ##########################################
    
    # limpando as variáveis
    dfv1 <- NULL
    dfv2 <- NULL
    dadoCopia <- NULL
    
    # fazendo uma cópia do dado (dataframe)
    dadoCopia <- dado
    
    # guardando o nome das colunas do dado (dataframe)
    nomeColunas <- colnames(dado)
    
    flag <- TRUE
    for (i2 in length(dado):1)
      if (is.na(dado[i2]) ||
          is.null(dado[i2]) ||
          (dado[i2]=="NULL"))
      {
        dadoCopia <- dadoCopia[,-c(i2)]
        if(flag)
        {
          flag <- FALSE
          dfV1 <- data.frame(valores[i2])
          names(dfV1) <- nomeColunas[i2]
        } else {
          dfV2 <- data.frame(valores[i2])
          names(dfV2) <- nomeColunas[i2]
          dfV1 <- cartesianExpand(dfV1, dfV2)
        }
      }
    
    queries <- as.data.frame(cartesianExpand(dadoCopia, dfV1))
    
    # obs: valores nulos vão no evento e os não nulos nas evidencias
    strEventoColuna    <- NULL
    strEvidenciaColuna <- NULL
    strEventoValor     <- NULL
    strEvidenciaValor  <- NULL
    
    contadorNull = 0
    for (i3 in length(dado):1)
      if (is.na(dado[i3]) ||
          is.null(dado[i3]) ||
          (dado[i3]=="NULL")){
        contadorNull = contadorNull + 1
        strEventoColuna <- c(nomeColunas[i3],strEventoColuna)
      } else {
        strEvidenciaColuna <- c(nomeColunas[i3],strEvidenciaColuna)
        strEvidenciaValor <- c((as.character(dado[1,i3])),strEvidenciaValor)
      }
    
    # devo pegar as três maiores probabilidades
    maiorProbabilidade = -1
    segundaMaiorProbabilidade = -1
    terceiraMaiorProbabilidade = -1
    
    queryMaiorProbabilidade = NULL
    querySegundaMaiorProbabilidade = NULL
    queryTerceiraMaiorProbabilidade = NULL
    
    indiceMaior = -1
    indiceSegundoMaior = -1
    indiceTerceiroMaior = -1
    
    #cat("nrow(queries) ===> ", nrow(queries), "\n")
    
    # percorrendo cada linha das queries
    for(i4 in 1:nrow(queries)){
      for(i5 in (length(dado)-contadorNull+1):(length(dado))){
        strEventoValor <- c((as.character(queries[i4,i5])),strEventoValor)
      }
      
      str1 = paste("(", strEventoColuna   , " == \"",strEventoValor   , "\")", sep = "", collapse = " & ")
      str2 = paste("(", strEvidenciaColuna, " == \"",strEvidenciaValor, "\")", sep = "")
      
      cmd = paste("cpquery(fittedbn, event = ", str1, ", evidence = ", str2, ")", sep = "")
      probabilidade = eval(parse(text = cmd))
      
      #cat("probabilidade===>", probabilidade, "\n")
      
      
      #pegando as três maiores probabilidades
      if(probabilidade > maiorProbabilidade){
        # a segunda passa a ser a terceira
        terceiraMaiorProbabilidade = segundaMaiorProbabilidade
        queryTerceiraMaiorProbabilidade = querySegundaMaiorProbabilidade
        indiceTerceiroMaior = indiceSegundoMaior
        
        # a primeira passa a ser a segunda
        segundaMaiorProbabilidade = maiorProbabilidade
        querySegundaMaiorProbabilidade = queryMaiorProbabilidade
        indiceSegundoMaior = indiceMaior
        
        # a nova passa a ser a primeira
        maiorProbabilidade = probabilidade
        queryMaiorProbabilidade = queries[i4,]
        indiceMaior = i4
      } else {
        if(probabilidade > segundaMaiorProbabilidade){
          # a segunda passa a ser a terceira
          terceiraMaiorProbabilidade = segundaMaiorProbabilidade
          queryTerceiraMaiorProbabilidade = querySegundaMaiorProbabilidade
          indiceTerceiroMaior = indiceSegundoMaior
          
          # a nova passa a ser a segunda
          segundaMaiorProbabilidade = probabilidade
          querySegundaMaiorProbabilidade = queries[i4,]
          indiceSegundoMaior = i4
        } else {
          if(probabilidade > terceiraMaiorProbabilidade){
            # a nova passa a ser a terceira
            terceiraMaiorProbabilidade = probabilidade
            queryTerceiraMaiorProbabilidade = queries[i4,]
            indiceTerceiraMaior = i4
          }
        }
      }
      
      strEventoValor <- NULL
    } # for i4
    
    
    ###################################################################################
    #  
    #  __   
    # /_ |  
    #  | |              Analisando os acertos e erros de primeira
    #  | | 
    #  | |
    #  |_|
    #
    ###################################################################################
    if(maiorProbabilidade>-1){
      #########################################################
      #cat ("Maior Probabiliade = ", maiorProbabilidade,"\n")
      #print ("Linha com buracos  = ")
      #print (dado[,order(names(dado))])
      #########################################################    
      
      ## auxiliar para identificar os buracos originais
      aux1 <- dado[,order(names(dado))]
      aux2 <- colnames(aux1)
      
      #########################################################
      #print ("Query selecionada  = ")
      #print (queryMaiorProbabilidade[,order(names(queryMaiorProbabilidade))])
      #print ("Linha sem buracos  = ")
      #print (base_teste_sem_buraco[i1,order(names(bn_df))])
      #########################################################
      contadorDiferencasPrimeira = 0
      ContadorIgualdadesPrimeira = 0
      
      # AA guarda a query com os buracos preenchidos com os valores de maior probabilidade
      AA <- queryMaiorProbabilidade[,order(names(queryMaiorProbabilidade))]
      # BB guarda a linha com os valores originais para verificar se acertou ou não
      BB <- base_teste_sem_buraco[i1,order(names(bn_df))]
      
      for(ww in 1:length(dado)){
        if(as.character(AA[,ww]) == as.character(BB[,ww])){
          if(is.na(aux1[ww]) || is.null(aux1[ww]) || (aux1[ww]=="NULL")){
            ## acertou um buraco
            if (aux2[ww] == "a1"){contadorAcertouAtuador1Primeira = contadorAcertouAtuador1Primeira + 1} 
            else
              if (aux2[ww] == "a2"){contadorAcertouAtuador2Primeira = contadorAcertouAtuador2Primeira + 1}
            else
              if(aux2[ww] == "acao"){contadorAcertouAcaoPrimeira = contadorAcertouAcaoPrimeira + 1}
            else 
              if(aux2[ww] == "somafusao"){contadorAcertouSomafusaoPrimeira = contadorAcertouSomafusaoPrimeira + 1}
            else 
              if(aux2[ww] == "ss1"){contadorAcertouSensor1Primeira = contadorAcertouSensor1Primeira + 1}
            else
              if(aux2[ww] == "ss2"){contadorAcertouSensor2Primeira = contadorAcertouSensor2Primeira + 1}
          }
          
          ContadorIgualdadesPrimeira <- ContadorIgualdadesPrimeira + 1
        } else {
          if(is.na(aux1[ww]) || is.null(aux1[ww]) || (aux1[ww]=="NULL")){
            ## errou um buraco
            if (aux2[ww] == "a1"){contadorErrouAtuador1Primeira = contadorErrouAtuador1Primeira + 1} 
            else
              if (aux2[ww] == "a2"){contadorErrouAtuador2Primeira = contadorErrouAtuador2Primeira + 1}
            else
              if(aux2[ww] == "acao"){contadorErrouAcaoPrimeira = contadorErrouAcaoPrimeira + 1}
            else 
              if(aux2[ww] == "somafusao"){contadorErrouSomafusaoPrimeira = contadorErrouSomafusaoPrimeira + 1}
            else 
              if(aux2[ww] == "ss1"){contadorErrouSensor1Primeira = contadorErrouSensor1Primeira + 1}
            else
              if(aux2[ww] == "ss2"){contadorErrouSensor2Primeira = contadorErrouSensor2Primeira + 1}
          }
          contadorDiferencasPrimeira <- contadorDiferencasPrimeira + 1
          contadorDeBuracosErradosPrimeira = contadorDeBuracosErradosPrimeira + 1
          #########################################################        
          #cat("Q: ",colnames(AA[ww]),"...:",as.character(AA[,ww]),"\t",
          #    "B: ",colnames(BB[ww]),"...:",as.character(BB[,ww]),"\n")
          #########################################################        
        }
      } # for ww
      
      #########################################################    
      #cat ("Qtde de Buracos certos  Primeira  = ", (qtdBuracosLinha-contadorDiferencasPrimeira), "\n")
      #cat ("Qtde de Buracos errados Primeira  = ", contadorDiferencasPrimeira, "\n")
      #cat ("Qtde de Buracos                   = ", qtdBuracosLinha, "\n")
      #########################################################    
      
      if(contadorDiferencasPrimeira==0)
        qtdAcertouTudoPrimeira = qtdAcertouTudoPrimeira + 1    
      
    } # if(maiorProbabilidade>-1)

    
    ###################################################################################
    #  
    #  ___  
    # |__ \
    #    ) |            Analisando os acertos e erros de Segunda
    #   / / 
    #  / /_ 
    # |____|
    #
    ###################################################################################
    if(segundaMaiorProbabilidade>-1){
      #########################################################
      #cat ("Segunda Maior Probabiliade = ", segundaMaiorProbabilidade,"\n")
      #print ("Linha com buracos  = ")
      #print (dado[,order(names(dado))])
      #########################################################    
      
      ## auxiliar para identificar os buracos originais
      aux1 <- dado[,order(names(dado))]
      aux2 <- colnames(aux1)
      
      #########################################################
      #print ("Query selecionada  = ")
      #print (querySegundaMaiorProbabilidade[,order(names(querySegundaMaiorProbabilidade))])
      #print ("Linha sem buracos  = ")
      #print (base_teste_sem_buraco[i1,order(names(bn_df))])
      #########################################################
      contadorDiferencasSegunda = 0
      ContadorIgualdadesSegunda = 0
      
      # AA guarda a query com os buracos preenchidos com os valores de maior probabilidade
      AA <- querySegundaMaiorProbabilidade[,order(names(querySegundaMaiorProbabilidade))]
      # BB guarda a linha com os valores originais para verificar se acertou ou não
      BB <- base_teste_sem_buraco[i1,order(names(bn_df))]
      
      for(ww in 1:length(dado)){
        if(as.character(AA[,ww]) == as.character(BB[,ww])){
          if(is.na(aux1[ww]) || is.null(aux1[ww]) || (aux1[ww]=="NULL")){
            ## acertou um buraco
            if (aux2[ww] == "a1"){contadorAcertouAtuador1Segunda = contadorAcertouAtuador1Segunda + 1} 
            else
              if (aux2[ww] == "a2"){contadorAcertouAtuador2Segunda = contadorAcertouAtuador2Segunda + 1}
            else
              if(aux2[ww] == "acao"){contadorAcertouAcaoSegunda = contadorAcertouAcaoSegunda + 1}
            else 
              if(aux2[ww] == "somafusao"){contadorAcertouSomafusaoSegunda = contadorAcertouSomafusaoSegunda + 1}
            else 
              if(aux2[ww] == "ss1"){contadorAcertouSensor1Segunda = contadorAcertouSensor1Segunda + 1}
            else
              if(aux2[ww] == "ss2"){contadorAcertouSensor2Segunda = contadorAcertouSensor2Segunda + 1}
          }
          
          ContadorIgualdadesSegunda <- ContadorIgualdadesSegunda + 1
        } else {
          if(is.na(aux1[ww]) || is.null(aux1[ww]) || (aux1[ww]=="NULL")){
            ## errou um buraco
            if (aux2[ww] == "a1"){contadorErrouAtuador1Segunda = contadorErrouAtuador1Segunda + 1} 
            else
              if (aux2[ww] == "a2"){contadorErrouAtuador2Segunda = contadorErrouAtuador2Segunda + 1}
            else
              if(aux2[ww] == "acao"){contadorErrouAcaoSegunda = contadorErrouAcaoSegunda + 1}
            else 
              if(aux2[ww] == "somafusao"){contadorErrouSomafusaoSegunda = contadorErrouSomafusaoSegunda + 1}
            else 
              if(aux2[ww] == "ss1"){contadorErrouSensor1Segunda = contadorErrouSensor1Segunda + 1}
            else
              if(aux2[ww] == "ss2"){contadorErrouSensor2Segunda = contadorErrouSensor2Segunda + 1}
          }
          contadorDiferencasSegunda <- contadorDiferencasSegunda + 1
          contadorDeBuracosErradosSegunda = contadorDeBuracosErradosSegunda + 1
          #########################################################        
          #cat("Q: ",colnames(AA[ww]),"...:",as.character(AA[,ww]),"\t",
          #    "B: ",colnames(BB[ww]),"...:",as.character(BB[,ww]),"\n")
          #########################################################        
        }
      } # for ww
      
      
      #########################################################    
      #cat ("Qtde de Buracos certos  Segunda = ", (qtdBuracosLinha-contadorDiferencasSegunda), "\n")
      #cat ("Qtde de Buracos errados Segunda = ", contadorDiferencasSegunda, "\n")
      #cat ("Qtde de Buracos                 = ", qtdBuracosLinha, "\n")
      #########################################################    
      
      if(contadorDiferencasSegunda==0)
        qtdAcertouTudoSegunda = qtdAcertouTudoSegunda + 1    
      
    } # if(segundaMaiorProbabilidade>-1)
    
    ###################################################################################
    #  
    #  ____  
    # |___ \   
    #   __) |           Analisando os acertos e erros de Terceira
    #  |__ < 
    #  ___) |
    # |____/
    #
    ###################################################################################
    if(terceiraMaiorProbabilidade>-1){
      #########################################################
      #cat ("Terceira Maior Probabiliade = ", terceiraMaiorProbabilidade,"\n")
      #print ("Linha com buracos  = ")
      #print (dado[,order(names(dado))])
      #########################################################    
      
      ## auxiliar para identificar os buracos originais
      aux1 <- dado[,order(names(dado))]
      aux2 <- colnames(aux1)
      
      #########################################################
      #print ("Query selecionada  = ")
      #print (queryTerceiraMaiorProbabilidade[,order(names(queryTerceiraMaiorProbabilidade))])
      #print ("Linha sem buracos  = ")
      #print (base_teste_sem_buraco[i1,order(names(bn_df))])
      #########################################################
      contadorDiferencasTerceira = 0
      ContadorIgualdadesTerceira = 0
      
      # AA guarda a query com os buracos preenchidos com os valores de maior probabilidade
      AA <- queryTerceiraMaiorProbabilidade[,order(names(queryTerceiraMaiorProbabilidade))]
      # BB guarda a linha com os valores originais para verificar se acertou ou não
      BB <- base_teste_sem_buraco[i1,order(names(bn_df))]
      
      for(ww in 1:length(dado)){
        if(as.character(AA[,ww]) == as.character(BB[,ww])){
          if(is.na(aux1[ww]) || is.null(aux1[ww]) || (aux1[ww]=="NULL")){
            ## acertou um buraco
            if (aux2[ww] == "a1"){contadorAcertouAtuador1Terceira = contadorAcertouAtuador1Terceira + 1} 
            else
              if (aux2[ww] == "a2"){contadorAcertouAtuador2Terceira = contadorAcertouAtuador2Terceira + 1}
            else
              if(aux2[ww] == "acao"){contadorAcertouAcaoTerceira = contadorAcertouAcaoTerceira + 1}
            else 
              if(aux2[ww] == "somafusao"){contadorAcertouSomafusaoTerceira = contadorAcertouSomafusaoTerceira + 1}
            else 
              if(aux2[ww] == "ss1"){contadorAcertouSensor1Terceira = contadorAcertouSensor1Terceira + 1}
            else
              if(aux2[ww] == "ss2"){contadorAcertouSensor2Terceira = contadorAcertouSensor2Terceira + 1}
          }
          
          ContadorIgualdadesTerceira <- ContadorIgualdadesTerceira + 1
        } else {
          if(is.na(aux1[ww]) || is.null(aux1[ww]) || (aux1[ww]=="NULL")){
            ## errou um buraco
            if (aux2[ww] == "a1"){contadorErrouAtuador1Terceira = contadorErrouAtuador1Terceira + 1} 
            else
              if (aux2[ww] == "a2"){contadorErrouAtuador2Terceira = contadorErrouAtuador2Terceira + 1}
            else
              if(aux2[ww] == "acao"){contadorErrouAcaoTerceira = contadorErrouAcaoTerceira + 1}
            else 
              if(aux2[ww] == "somafusao"){contadorErrouSomafusaoTerceira = contadorErrouSomafusaoTerceira + 1}
            else 
              if(aux2[ww] == "ss1"){contadorErrouSensor1Terceira = contadorErrouSensor1Terceira + 1}
            else
              if(aux2[ww] == "ss2"){contadorErrouSensor2Terceira = contadorErrouSensor2Terceira + 1}
          }
          contadorDiferencasTerceira <- contadorDiferencasTerceira + 1
          contadorDeBuracosErradosTerceira = contadorDeBuracosErradosTerceira + 1
          #########################################################        
          #cat("Q: ",colnames(AA[ww]),"...:",as.character(AA[,ww]),"\t",
          #    "B: ",colnames(BB[ww]),"...:",as.character(BB[,ww]),"\n")
          #########################################################        
        }
      } # for ww
      
      
      #########################################################    
      #cat ("Qtde de Buracos certos  Terceira = ", (qtdBuracosLinha-contadorDiferencasTerceira), "\n")
      #cat ("Qtde de Buracos errados Terceira = ", contadorDiferencasTerceira, "\n")
      #cat ("Qtde de Buracos                  = ", qtdBuracosLinha, "\n")
      #########################################################    
      
      if(contadorDiferencasTerceira==0)
        qtdAcertouTudoTerceira = qtdAcertouTudoTerceira + 1    
      
    } # if(terceiraMaiorProbabilidade>-1)
    
  } #  i1
  ##########################################
  # fim do Laço principal
  ##########################################
  
  contadorAcertouSensorPrimeira <- contadorAcertouSensor1Primeira +
    contadorAcertouSensor2Primeira
  
  contadorAcertouSensorSegunda <- contadorAcertouSensor1Segunda +
    contadorAcertouSensor2Segunda
  
  contadorAcertouSensorTerceira <- contadorAcertouSensor1Terceira +
    contadorAcertouSensor2Terceira
  
  
  contadorErrouSensorPrimeira   <- contadorErrouSensor1Primeira   +
    contadorErrouSensor2Primeira
  
  contadorErrouSensorSegunda   <-  contadorErrouSensor1Segunda   +
    contadorErrouSensor2Segunda
  
  contadorErrouSensorTerceira   <- contadorErrouSensor1Terceira   +
    contadorErrouSensor2Terceira
  
  contadorAcertouAtuadorPrimeira <- contadorAcertouAtuador1Primeira +
    contadorAcertouAtuador2Primeira
  
  contadorAcertouAtuadorSegunda <-  contadorAcertouAtuador1Segunda +
    contadorAcertouAtuador2Segunda
  
  contadorAcertouAtuadorTerceira <- contadorAcertouAtuador1Terceira +
    contadorAcertouAtuador2Terceira
  
  contadorErrouAtuadorPrimeira   <- contadorErrouAtuador1Primeira   +
    contadorErrouAtuador2Primeira
  
  contadorErrouAtuadorSegunda   <- contadorErrouAtuador1Segunda   +
    contadorErrouAtuador2Segunda 
  
  contadorErrouAtuadorTerceira  <- contadorErrouAtuador1Terceira   +
    contadorErrouAtuador2Terceira 
  
  # atualizando o dataframe com base no ArquivoResultado
  df_resultado <- as.data.frame(fromJSON(ArquivoResultado))
  
  #variaveis para guardar no dataframe de resultado de processamento
  df_resultado$totBuracos[linhaResultado] = contadorDeBuracos
  
  df_resultado$totBuracosAcertouPrimeira[linhaResultado] = (contadorDeBuracos-contadorDeBuracosErradosPrimeira)
  df_resultado$totBuracosAcertouSegunda[linhaResultado] = (contadorDeBuracos-contadorDeBuracosErradosSegunda)
  df_resultado$totBuracosAcertouTerceira[linhaResultado] = (contadorDeBuracos-contadorDeBuracosErradosTerceira)
  
  df_resultado$totBuracosErrouPrimeira[linhaResultado] = contadorDeBuracosErradosPrimeira
  df_resultado$totBuracosErrouSegunda[linhaResultado] = contadorDeBuracosErradosSegunda
  df_resultado$totBuracosErrouTerceira[linhaResultado] = contadorDeBuracosErradosTerceira
  
  df_resultado$predicoes100CorretasPrimeira[linhaResultado] = qtdAcertouTudoPrimeira
  df_resultado$predicoes100CorretasSegunda[linhaResultado] = qtdAcertouTudoSegunda
  df_resultado$predicoes100CorretasTerceira[linhaResultado] = qtdAcertouTudoTerceira
  
  # acho melhor pegar a Primeira
  df_resultado$totBuracosSensores[linhaResultado] = contadorAcertouSensorPrimeira+contadorErrouSensorPrimeira
  
  df_resultado$acertosSensor1Primeira[linhaResultado] = contadorAcertouSensor1Primeira
  df_resultado$acertosSensor1Segunda[linhaResultado] = contadorAcertouSensor1Segunda
  df_resultado$acertosSensor1Terceira[linhaResultado] = contadorAcertouSensor1Terceira
  
  df_resultado$acertosSensor2Primeira[linhaResultado] = contadorAcertouSensor2Primeira
  df_resultado$acertosSensor2Segunda[linhaResultado] = contadorAcertouSensor2Segunda
  df_resultado$acertosSensor2Terceira[linhaResultado] = contadorAcertouSensor2Terceira
  
  df_resultado$errosSensor1Primeira[linhaResultado] = contadorErrouSensor1Primeira
  df_resultado$errosSensor1Segunda[linhaResultado] = contadorErrouSensor1Segunda
  df_resultado$errosSensor1Terceira[linhaResultado] = contadorErrouSensor1Terceira
  
  df_resultado$errosSensor2Primeira[linhaResultado] = contadorErrouSensor2Primeira
  df_resultado$errosSensor2Segunda[linhaResultado] = contadorErrouSensor2Segunda
  df_resultado$errosSensor2Terceira[linhaResultado] = contadorErrouSensor2Terceira
  
  # o calculo abaixo pode ter denominador zero e gerar NaN - colocar em um if
  if((contadorAcertouSensorPrimeira + contadorErrouSensorPrimeira)>0){
    df_resultado$porcAcertoSensorPrimeira[linhaResultado] = (contadorAcertouSensorPrimeira/(contadorAcertouSensorPrimeira + contadorErrouSensorPrimeira))*100.0    
  } else {
    df_resultado$porcAcertoSensorPrimeira[linhaResultado] = 0
  }
  
  if((contadorAcertouSensorSegunda + contadorErrouSensorSegunda)>0){
    df_resultado$porcAcertoSensorSegunda[linhaResultado] = (contadorAcertouSensorSegunda/(contadorAcertouSensorSegunda + contadorErrouSensorSegunda))*100.0    
  } else {
    df_resultado$porcAcertoSensorSegunda[linhaResultado] = 0
  }
  
  if((contadorAcertouSensorTerceira + contadorErrouSensorTerceira)>0){
    df_resultado$porcAcertoSensorTerceira[linhaResultado] = (contadorAcertouSensorTerceira/(contadorAcertouSensorTerceira + contadorErrouSensorTerceira))*100.0    
  } else {
    df_resultado$porcAcertoSensorTerceira[linhaResultado] = 0
  }
  
  # acho melhor pegar a Primeira
  df_resultado$totBuracosAcao[linhaResultado] = contadorAcertouAcaoPrimeira + contadorErrouAcaoPrimeira
  
  df_resultado$acertosAcaoPrimeira[linhaResultado] = contadorAcertouAcaoPrimeira
  df_resultado$acertosAcaoSegunda[linhaResultado] = contadorAcertouAcaoSegunda
  df_resultado$acertosAcaoTerceira[linhaResultado] = contadorAcertouAcaoTerceira
  
  df_resultado$errosAcaoPrimeira[linhaResultado] = contadorErrouAcaoPrimeira
  df_resultado$errosAcaoSegunda[linhaResultado] = contadorErrouAcaoSegunda
  df_resultado$errosAcaoTerceira[linhaResultado] = contadorErrouAcaoTerceira
  
  # o calculo abaixo pode ter denominador zero e gerar NaN - colocar em um if
  if((contadorAcertouAcaoPrimeira + contadorErrouAcaoPrimeira)>0){
    df_resultado$porcAcertoAcaoPrimeira[linhaResultado] = (contadorAcertouAcaoPrimeira/(contadorAcertouAcaoPrimeira+contadorErrouAcaoPrimeira))*100.0    
  } else {
    df_resultado$porcAcertoAcaoPrimeira[linhaResultado] = 0    
  }
  
  if((contadorAcertouAcaoSegunda + contadorErrouAcaoSegunda)>0){
    df_resultado$porcAcertoAcaoSegunda[linhaResultado] = (contadorAcertouAcaoSegunda/(contadorAcertouAcaoSegunda+contadorErrouAcaoSegunda))*100.0    
  } else {
    df_resultado$porcAcertoAcaoSegunda[linhaResultado] = 0    
  }
  
  if((contadorAcertouAcaoTerceira + contadorErrouAcaoTerceira)>0){
    df_resultado$porcAcertoAcaoTerceira[linhaResultado] = (contadorAcertouAcaoTerceira/(contadorAcertouAcaoTerceira+contadorErrouAcaoTerceira))*100.0    
  } else {
    df_resultado$porcAcertoAcaoTerceira[linhaResultado] = 0    
  }
  
  # acho melhor pegar a Primeira  
  df_resultado$totBuracosSomafusao[linhaResultado] = contadorAcertouSomafusaoPrimeira + contadorErrouSomafusaoPrimeira
  
  df_resultado$acertosSomafusaoPrimeira[linhaResultado] = contadorAcertouSomafusaoPrimeira
  df_resultado$acertosSomafusaoSegunda[linhaResultado] = contadorAcertouSomafusaoSegunda
  df_resultado$acertosSomafusaoTerceira[linhaResultado] = contadorAcertouSomafusaoTerceira
  
  df_resultado$errosSomafusaoPrimeira[linhaResultado] = contadorErrouSomafusaoPrimeira
  df_resultado$errosSomafusaoSegunda[linhaResultado] = contadorErrouSomafusaoSegunda
  df_resultado$errosSomafusaoTerceira[linhaResultado] = contadorErrouSomafusaoTerceira
  
  # o calculo abaixo pode ter denominador zero e gerar NaN - colocar em um if
  if((contadorAcertouSomafusaoPrimeira + contadorErrouSomafusaoPrimeira)>0){
    df_resultado$porcAcertoSomafusaoPrimeira[linhaResultado] = (contadorAcertouSomafusaoPrimeira/(contadorAcertouSomafusaoPrimeira+contadorErrouSomafusaoPrimeira))*100.0    
  } else {
    df_resultado$porcAcertoSomafusaoPrimeira[linhaResultado] = 0
  }
  
  if((contadorAcertouSomafusaoSegunda + contadorErrouSomafusaoSegunda)>0){
    df_resultado$porcAcertoSomafusaoSegunda[linhaResultado] = (contadorAcertouSomafusaoSegunda/(contadorAcertouSomafusaoSegunda+contadorErrouSomafusaoSegunda))*100.0    
  } else {
    df_resultado$porcAcertoSomafusaoSegunda[linhaResultado] = 0
  }
  
  if((contadorAcertouSomafusaoTerceira + contadorErrouSomafusaoTerceira)>0){
    df_resultado$porcAcertoSomafusaoTerceira[linhaResultado] = (contadorAcertouSomafusaoTerceira/(contadorAcertouSomafusaoTerceira+contadorErrouSomafusaoTerceira))*100.0    
  } else {
    df_resultado$porcAcertoSomafusaoTerceira[linhaResultado] = 0
  }
  
  # acho melhor pegar a Primeira  
  df_resultado$totBuracosAtuador[linhaResultado] = contadorAcertouAtuadorPrimeira + contadorErrouAtuadorPrimeira
  
  df_resultado$acertosAtuador1Primeira[linhaResultado] = contadorAcertouAtuador1Primeira
  df_resultado$acertosAtuador1Segunda[linhaResultado] = contadorAcertouAtuador1Segunda
  df_resultado$acertosAtuador1Terceira[linhaResultado] = contadorAcertouAtuador1Terceira
  
  df_resultado$acertosAtuador2Primeira[linhaResultado] = contadorAcertouAtuador2Primeira
  df_resultado$acertosAtuador2Segunda[linhaResultado] = contadorAcertouAtuador2Segunda
  df_resultado$acertosAtuador2Terceira[linhaResultado] = contadorAcertouAtuador2Terceira
  
  df_resultado$errosAtuador1Primeira[linhaResultado] = contadorErrouAtuador1Primeira
  df_resultado$errosAtuador1Segunda[linhaResultado] = contadorErrouAtuador1Segunda
  df_resultado$errosAtuador1Terceira[linhaResultado] = contadorErrouAtuador1Terceira
  
  df_resultado$errosAtuador2Primeira[linhaResultado] = contadorErrouAtuador2Primeira
  df_resultado$errosAtuador2Segunda[linhaResultado] = contadorErrouAtuador2Segunda
  df_resultado$errosAtuador2Terceira[linhaResultado] = contadorErrouAtuador2Terceira
  
  # o calculo abaixo pode ter denominador zero e gerar NaN - colocar em um if
  if((contadorAcertouAtuadorPrimeira + contadorErrouAtuadorPrimeira)>0){
    df_resultado$porcAcertosAtuadorPrimeira[linhaResultado] = (contadorAcertouAtuadorPrimeira/(contadorAcertouAtuadorPrimeira + contadorErrouAtuadorPrimeira))*100.0    
  } else {
    df_resultado$porcAcertosAtuadorPrimeira[linhaResultado] = 0    
  }
  
  if((contadorAcertouAtuadorSegunda + contadorErrouAtuadorSegunda)){
    df_resultado$porcAcertosAtuadorSegunda[linhaResultado] = (contadorAcertouAtuadorSegunda/(contadorAcertouAtuadorSegunda + contadorErrouAtuadorSegunda))*100.0    
  } else {
    df_resultado$porcAcertosAtuadorSegunda[linhaResultado] = 0    
  }
  
  if((contadorAcertouAtuadorTerceira + contadorErrouAtuadorTerceira)){
    df_resultado$porcAcertosAtuadorTerceira[linhaResultado] = (contadorAcertouAtuadorTerceira/(contadorAcertouAtuadorTerceira + contadorErrouAtuadorTerceira))*100.0    
  } else {
    df_resultado$porcAcertosAtuadorTerceira[linhaResultado] = 0    
  }
  
  
  ##################################################################
  # gravação do arquivo de resultado do processamento
  write_json(df_resultado, ArquivoResultado)
  
} # fim da função processaUmArquivoCenario6


##################################################################################
# FUNCTION : processaUmArquivoCenario7
#
# processamento de cada arquivo para cada percentual de buracos para o cenario 7
#
##################################################################################

processaUmArquivoCenario7 <- function (vez,
                                       linhaResultado,
                                       registros,
                                       furos,
                                       df_white,
                                       df_black) {
  
  ###################################################################################
  # carrega a base na variável bn_df como um dataframe
  ###################################################################################
  #
  nomeArquivo = paste(NomeExperimento,"_",registros,".csv",sep="",collapse="")
  bn_df <- read.csv(nomeArquivo)
  
  ###################################################################################
  # Converter as colunas para Factor para a rede bayesiana - função hc()
  ###################################################################################
  #
  bn_df$c   = as.factor(bn_df$c)
  bn_df$s1  = as.factor(bn_df$s1)
  bn_df$s2  = as.factor(bn_df$s2)
  bn_df$f1  = as.factor(bn_df$f1)
  bn_df$a1  = as.factor(bn_df$a1)
  bn_df$r   = as.factor(bn_df$r)
  
  ###################################################################################
  # dividir a amostra em duas partes: treino e teste
  ###################################################################################
  #
  divisao = sample.split(bn_df$a1,SplitRatio = 0.5)
  
  base_treinamento      = subset(bn_df, divisao == TRUE)
  base_teste            = subset(bn_df, divisao == FALSE)
  base_teste_sem_buraco = subset(bn_df, divisao == FALSE)
  
  ###################################################################################
  # aplica a função hc na variável bn_df e guarda o resultado na variável res
  # Sobre a função: Agrupamento hierárquico aglomerativo baseado em critérios de 
  # máxima verossimilhança para modelos de mistura gaussiana parametrizados por 
  # decomposição de autovalores.
  ###################################################################################
  # O que eu entendi: está criando a RB com o algoritmo "max-min hill climbing", 
  # que é um algoritmo guloso.
  ###################################################################################
  #
  res = hc(base_treinamento, whitelist = df_white, blacklist = df_black)
  
  ###################################################################################
  # Grava o Plot das dependências funcionais entre as variáveis
  ###################################################################################
  #
  nomeArquivoImagem = paste(NomeExperimento,"_imagem.png",sep="",collapse="")
  if(!file.exists(nomeArquivoImagem)){
    png(file = nomeArquivoImagem)
    plot(res)
    dev.off()
  }
  ###################################################################################
  # treinar a rede
  # Depois de aprender a estrutura, é preciso descobrir as tabelas de probabilidade
  # condicional (CPTs) em cada nó. A função bn.fit executa o algoritmo EM para 
  # aprender CPT para diferentes nós no gráfico acima.
  ###################################################################################
  #
  #cat("treinar a rede\n")
  fittedbn <- bn.fit(res, data = base_treinamento)
  
  ###################################################################################
  # Explorando um dos cálculos
  # Por exemplo, vamos ver o que está dentro do nó s1
  ###################################################################################
  #
  #cat("explorando um dos cálculos\n")
  # fittedbn é um objeto da classe list
  #print(fittedbn)
  # s1 
  #print(fittedbn$s1)
  
  
  ###################################################################################
  # insere nulos
  ###################################################################################
  #
  porcentagem = furos/100
  
  #cat("Porcentagem=",porcentagem,"\n")
  
  nlin <- nrow(base_teste)
  ncol <- ncol(base_teste)
  totalelementos <- nlin * ncol
  porc_aleatorio <- totalelementos * porcentagem
  
  if(porc_aleatorio<1){
    porc_aleatorio=1
  }
  
  aleatorios <- sort(sample(1:totalelementos, porc_aleatorio))
  
  for (i in 1:porc_aleatorio){
    substituir <- aleatorios[i]
    linsub     <- (substituir %/% ncol)
    colsub     <- (substituir %% ncol)
    if(colsub==0){
      colsub <- ncol
    }
    base_teste[linsub,colsub] <- 'NULL'
  }
  
  #cat("Inseriu nulos\n")  
  ###################################################################################
  # Verifica se deu linha 100% nula
  ###################################################################################
  #
  contaNulo <- 0
  for (i in 1:nlin){
    for(j in 1:ncol){
      if (is.na  (base_teste[i,j]) || 
          is.null(base_teste[i,j]) ||
          (base_teste[i,j]=="NULL")){
        contaNulo = contaNulo + 1
      }
    }
    if(contaNulo==ncol){
      print("tudo nulo, não da pra fazer a predição")
      contaNulo <- 0 #24/07/2020
      #stop("tudo nulo, não da pra fazer a predição")
    }
    else
      contaNulo <- 0
  }
  #cat("Verificou linhas todas de buracos")
  ###################################################################################
  # Conta total de buracos na base
  # Conta total de linhas sem buracos
  # Conta total de linhas com buracos
  ###################################################################################
  #
  contadorDeBuracos    <- 0
  qtdlinhasSemBuracos  <- 0
  qtdlinhasComBuracos  <- 0
  
  for (i in 1:nlin){
    contAux=0
    for(j in 1:ncol){
      if (is.na  (base_teste[i,j]) || 
          is.null(base_teste[i,j]) ||
          (base_teste[i,j]=="NULL")){
        contAux=contAux+1
        contadorDeBuracos = contadorDeBuracos + 1
      }
    }
    if(contAux==0)
      qtdlinhasSemBuracos = qtdlinhasSemBuracos + 1
    else
      qtdlinhasComBuracos = qtdlinhasComBuracos + 1
  }
  
  #cat("Contou linhas e buracos\n")  
  
  ###################################################################################
  # informa valores para as colunas e suas categorias
  # O ideal seria vir de algum arquivo...
  ###################################################################################
  #
  valores <- list(c('0', '1'),
                  c('0', '1'),
                  c('0', '1'),
                  c('0', '1'),
                  c('0', '1'),
                  c('0', '1'))
  
  categorias <- list(c('c','s1','s2'),
                     c('f1'),
                     c('r'),
                     c('a1'))
  
  ###################################################################################
  # inicialização dos contadores gerais do processamento
  ###################################################################################
  #
  
  contadorDeBuracosErradosPrimeira  <- 0
  contadorDeBuracosErradosSegunda   <- 0
  contadorDeBuracosErradosTerceira  <- 0
  qtdAcertouTudoPrimeira            <- 0
  qtdAcertouTudoSegunda             <- 0
  qtdAcertouTudoTerceira            <- 0
  
  contadorAcertouAtuador1Primeira   <- 0
  contadorAcertouAtuador1Segunda    <- 0
  contadorAcertouAtuador1Terceira   <- 0

  contadorAcertouF1Primeira         <- 0  
  contadorAcertouF1Segunda          <- 0 
  contadorAcertouF1Terceira         <- 0  
  
  contadorAcertouRPrimeira          <- 0  
  contadorAcertouRSegunda           <- 0 
  contadorAcertouRTerceira          <- 0  
  
  contadorAcertouSensor1Primeira    <- 0    
  contadorAcertouSensor1Segunda     <- 0  
  contadorAcertouSensor1Terceira    <- 0    
  
  contadorAcertouSensor2Primeira    <- 0  
  contadorAcertouSensor2Segunda     <- 0 
  contadorAcertouSensor2Terceira    <- 0  
  
  contadorAcertouSensorCPrimeira    <- 0    
  contadorAcertouSensorCSegunda     <- 0  
  contadorAcertouSensorCTerceira    <- 0    
  
  contadorErrouAtuador1Primeira     <- 0
  contadorErrouAtuador1Segunda      <- 0
  contadorErrouAtuador1Terceira     <- 0
  
  contadorErrouF1Primeira           <- 0
  contadorErrouF1Segunda            <- 0
  contadorErrouF1Terceira           <- 0
  
  contadorErrouRPrimeira            <- 0
  contadorErrouRSegunda             <- 0
  contadorErrouRTerceira            <- 0
  
  contadorErrouSensor1Primeira      <- 0
  contadorErrouSensor1Segunda       <- 0
  contadorErrouSensor1Terceira      <- 0
  
  contadorErrouSensor2Primeira      <- 0
  contadorErrouSensor2Segunda       <- 0
  contadorErrouSensor2Terceira      <- 0
  
  contadorErrouSensorCPrimeira      <- 0
  contadorErrouSensorCSegunda       <- 0
  contadorErrouSensorCTerceira      <- 0
  
  ###################################################################################
  # Laço principal para todas as instâncias
  # do arquivo
  ###################################################################################
  #
  for(i1 in 1:nlin){
    #cat("***Laco Principal - processando linha ", i1, "\n")
    dado <- base_teste[i1,]
    
    #####################################################
    # Verificando se a linha tem buracos
    # se não tiver então pular para a próxima linha
    #####################################################
    qtdBuracosLinha = 0
    
    for (i2 in 1:length(dado)){
      if (is.na(dado[i2]) ||
          is.null(dado[i2]) ||
          (dado[i2]=="NULL"))
        qtdBuracosLinha = qtdBuracosLinha + 1        
    }
    
    
    if(qtdBuracosLinha==0 ||
       qtdBuracosLinha==length(dado) ||
       qtdBuracosLinha/length(dado) > 0.75){ # 24/07/2020 se não tem buraco ou só tem buraco, pular.
      #não precisa fazer queries ... então ir para o próximo
      #print ("Linha sem buracos... pulada")
      next
    }
    
    
    ##########################################
    # monta as queries
    ##########################################
    
    # limpando as variáveis
    dfv1 <- NULL
    dfv2 <- NULL
    dadoCopia <- NULL
    
    # fazendo uma cópia do dado (dataframe)
    dadoCopia <- dado
    
    # guardando o nome das colunas do dado (dataframe)
    nomeColunas <- colnames(dado)
    
    flag <- TRUE
    for (i2 in length(dado):1)
      if (is.na(dado[i2]) ||
          is.null(dado[i2]) ||
          (dado[i2]=="NULL"))
      {
        dadoCopia <- dadoCopia[,-c(i2)]
        if(flag)
        {
          flag <- FALSE
          dfV1 <- data.frame(valores[i2])
          names(dfV1) <- nomeColunas[i2]
        } else {
          dfV2 <- data.frame(valores[i2])
          names(dfV2) <- nomeColunas[i2]
          dfV1 <- cartesianExpand(dfV1, dfV2)
        }
      }
    
    
    queries <- as.data.frame(cartesianExpand(dadoCopia, dfV1))
    
    # obs: valores nulos vão no evento e os não nulos nas evidencias
    strEventoColuna    <- NULL
    strEvidenciaColuna <- NULL
    strEventoValor     <- NULL
    strEvidenciaValor  <- NULL
    
    contadorNull = 0
    for (i3 in length(dado):1)
      if (is.na(dado[i3]) ||
          is.null(dado[i3]) ||
          (dado[i3]=="NULL")){
        contadorNull = contadorNull + 1
        strEventoColuna <- c(nomeColunas[i3],strEventoColuna)
      } else {
        strEvidenciaColuna <- c(nomeColunas[i3],strEvidenciaColuna)
        strEvidenciaValor <- c((as.character(dado[1,i3])),strEvidenciaValor)
      }
    
    # devo pegar as três maiores probabilidades
    maiorProbabilidade = -1
    segundaMaiorProbabilidade = -1
    terceiraMaiorProbabilidade = -1
    
    queryMaiorProbabilidade = NULL
    querySegundaMaiorProbabilidade = NULL
    queryTerceiraMaiorProbabilidade = NULL
    
    indiceMaior = -1
    indiceSegundoMaior = -1
    indiceTerceiroMaior = -1
    
    #cat("nrow(queries) ===> ", nrow(queries), "\n")
    
    
    # percorrendo cada linha das queries
    for(i4 in 1:nrow(queries)){
      for(i5 in (length(dado)-contadorNull+1):(length(dado))){
        strEventoValor <- c((as.character(queries[i4,i5])),strEventoValor)
      }
      
      str1 = paste("(", strEventoColuna   , " == \"",strEventoValor   , "\")", sep = "", collapse = " & ")
      str2 = paste("(", strEvidenciaColuna, " == \"",strEvidenciaValor, "\")", sep = "")
      
      cmd = paste("cpquery(fittedbn, event = ", str1, ", evidence = ", str2, ")", sep = "")
      probabilidade = eval(parse(text = cmd))
      
      #cat("probabilidade===>", probabilidade, "\n")
      
      
      #pegando as três maiores probabilidades
      if(probabilidade > maiorProbabilidade){
        # a segunda passa a ser a terceira
        terceiraMaiorProbabilidade = segundaMaiorProbabilidade
        queryTerceiraMaiorProbabilidade = querySegundaMaiorProbabilidade
        indiceTerceiroMaior = indiceSegundoMaior
        
        # a primeira passa a ser a segunda
        segundaMaiorProbabilidade = maiorProbabilidade
        querySegundaMaiorProbabilidade = queryMaiorProbabilidade
        indiceSegundoMaior = indiceMaior
        
        # a nova passa a ser a primeira
        maiorProbabilidade = probabilidade
        queryMaiorProbabilidade = queries[i4,]
        indiceMaior = i4
      } else {
        if(probabilidade > segundaMaiorProbabilidade){
          # a segunda passa a ser a terceira
          terceiraMaiorProbabilidade = segundaMaiorProbabilidade
          queryTerceiraMaiorProbabilidade = querySegundaMaiorProbabilidade
          indiceTerceiroMaior = indiceSegundoMaior
          
          # a nova passa a ser a segunda
          segundaMaiorProbabilidade = probabilidade
          querySegundaMaiorProbabilidade = queries[i4,]
          indiceSegundoMaior = i4
        } else {
          if(probabilidade > terceiraMaiorProbabilidade){
            # a nova passa a ser a terceira
            terceiraMaiorProbabilidade = probabilidade
            queryTerceiraMaiorProbabilidade = queries[i4,]
            indiceTerceiraMaior = i4
          }
        }
      }
      
      strEventoValor <- NULL
    } # for i4
    
    
    ###################################################################################
    #  
    #  __   
    # /_ |  
    #  | |              Analisando os acertos e erros de primeira
    #  | | 
    #  | |
    #  |_|
    #
    ###################################################################################
    if(maiorProbabilidade>-1){
      #########################################################
      #cat ("Maior Probabiliade = ", maiorProbabilidade,"\n")
      #print ("Linha com buracos  = ")
      #print (dado[,order(names(dado))])
      #########################################################    
      
      ## auxiliar para identificar os buracos originais
      aux1 <- dado[,order(names(dado))]
      aux2 <- colnames(aux1)
      
      #########################################################
      #print ("Query selecionada  = ")
      #print (queryMaiorProbabilidade[,order(names(queryMaiorProbabilidade))])
      #print ("Linha sem buracos  = ")
      #print (base_teste_sem_buraco[i1,order(names(bn_df))])
      #########################################################
      contadorDiferencasPrimeira = 0
      ContadorIgualdadesPrimeira = 0
      
      # AA guarda a query com os buracos preenchidos com os valores de maior probabilidade
      AA <- queryMaiorProbabilidade[,order(names(queryMaiorProbabilidade))]
      # BB guarda a linha com os valores originais para verificar se acertou ou não
      BB <- base_teste_sem_buraco[i1,order(names(bn_df))]
      
      for(ww in 1:length(dado)){
        if(as.character(AA[,ww]) == as.character(BB[,ww])){
          if(is.na(aux1[ww]) || is.null(aux1[ww]) || (aux1[ww]=="NULL")){
            ## acertou um buraco
            if (aux2[ww] == "a1"){contadorAcertouAtuador1Primeira = contadorAcertouAtuador1Primeira + 1} 
            else
              if(aux2[ww] == "r"){contadorAcertouRPrimeira = contadorAcertouRPrimeira + 1}
            else 
              if(aux2[ww] == "f1"){contadorAcertouF1Primeira = contadorAcertouF1Primeira + 1}
            else 
              if(aux2[ww] == "s1"){contadorAcertouSensor1Primeira = contadorAcertouSensor1Primeira + 1}
            else
              if(aux2[ww] == "s2"){contadorAcertouSensor2Primeira = contadorAcertouSensor2Primeira + 1}
            else
              if(aux2[ww] == "c"){contadorAcertouSensorcPrimeira = contadorAcertouSensorCPrimeira + 1}
          }
          
          ContadorIgualdadesPrimeira <- ContadorIgualdadesPrimeira + 1
        } else {
          if(is.na(aux1[ww]) || is.null(aux1[ww]) || (aux1[ww]=="NULL")){
            ## errou um buraco
            if (aux2[ww] == "a1"){contadorErrouAtuador1Primeira = contadorErrouAtuador1Primeira + 1} 
            else
              if(aux2[ww] == "r"){contadorErrouRPrimeira = contadorErrouRPrimeira + 1}
            else 
              if(aux2[ww] == "f1"){contadorErrouF1Primeira = contadorErrouF1Primeira + 1}
            else 
              if(aux2[ww] == "s1"){contadorErrouSensor1Primeira = contadorErrouSensor1Primeira + 1}
            else
              if(aux2[ww] == "s2"){contadorErrouSensor2Primeira = contadorErrouSensor2Primeira + 1}
            else
              if(aux2[ww] == "c"){contadorErrouSensorCPrimeira = contadorErrouSensorCPrimeira + 1}
          }
          contadorDiferencasPrimeira <- contadorDiferencasPrimeira + 1
          contadorDeBuracosErradosPrimeira = contadorDeBuracosErradosPrimeira + 1
          #########################################################        
          #cat("Q: ",colnames(AA[ww]),"...:",as.character(AA[,ww]),"\t",
          #    "B: ",colnames(BB[ww]),"...:",as.character(BB[,ww]),"\n")
          #########################################################        
        }
      } # for ww
      
      #########################################################    
      #cat ("Qtde de Buracos certos  Primeira  = ", (qtdBuracosLinha-contadorDiferencasPrimeira), "\n")
      #cat ("Qtde de Buracos errados Primeira  = ", contadorDiferencasPrimeira, "\n")
      #cat ("Qtde de Buracos                   = ", qtdBuracosLinha, "\n")
      #########################################################    
      
      if(contadorDiferencasPrimeira==0)
        qtdAcertouTudoPrimeira = qtdAcertouTudoPrimeira + 1    
      
    } # if(maiorProbabilidade>-1)
    
    
    ###################################################################################
    #  
    #  ___  
    # |__ \
    #    ) |            Analisando os acertos e erros de Segunda
    #   / / 
    #  / /_ 
    # |____|
    #
    ###################################################################################
    if(segundaMaiorProbabilidade>-1){
      #########################################################
      #cat ("Segunda Maior Probabiliade = ", segundaMaiorProbabilidade,"\n")
      #print ("Linha com buracos  = ")
      #print (dado[,order(names(dado))])
      #########################################################    
      
      ## auxiliar para identificar os buracos originais
      aux1 <- dado[,order(names(dado))]
      aux2 <- colnames(aux1)
      
      #########################################################
      #print ("Query selecionada  = ")
      #print (querySegundaMaiorProbabilidade[,order(names(querySegundaMaiorProbabilidade))])
      #print ("Linha sem buracos  = ")
      #print (base_teste_sem_buraco[i1,order(names(bn_df))])
      #########################################################
      contadorDiferencasSegunda = 0
      ContadorIgualdadesSegunda = 0
      
      # AA guarda a query com os buracos preenchidos com os valores de maior probabilidade
      AA <- querySegundaMaiorProbabilidade[,order(names(querySegundaMaiorProbabilidade))]
      # BB guarda a linha com os valores originais para verificar se acertou ou não
      BB <- base_teste_sem_buraco[i1,order(names(bn_df))]
      
      for(ww in 1:length(dado)){
        if(as.character(AA[,ww]) == as.character(BB[,ww])){
          if(is.na(aux1[ww]) || is.null(aux1[ww]) || (aux1[ww]=="NULL")){
            ## acertou um buraco
            if (aux2[ww] == "a1"){contadorAcertouAtuador1Segunda = contadorAcertouAtuador1Segunda + 1} 
            else
              if(aux2[ww] == "r"){contadorAcertouRSegunda = contadorAcertouRSegunda + 1}
            else 
              if(aux2[ww] == "f1"){contadorAcertouF1Segunda = contadorAcertouF1Segunda + 1}
            else 
              if(aux2[ww] == "s1"){contadorAcertouSensor1Segunda = contadorAcertouSensor1Segunda + 1}
            else
              if(aux2[ww] == "s2"){contadorAcertouSensor2Segunda = contadorAcertouSensor2Segunda + 1}
            else
              if(aux2[ww] == "c"){contadorAcertouSensorcSegunda = contadorAcertouSensorCSegunda + 1}
          }
          
          ContadorIgualdadesSegunda <- ContadorIgualdadesSegunda + 1
        } else {
          if(is.na(aux1[ww]) || is.null(aux1[ww]) || (aux1[ww]=="NULL")){
            ## errou um buraco
            if (aux2[ww] == "a1"){contadorErrouAtuador1Segunda = contadorErrouAtuador1Segunda + 1} 
            else
              if(aux2[ww] == "r"){contadorErrouRSegunda = contadorErrouRSegunda + 1}
            else 
              if(aux2[ww] == "f1"){contadorErrouF1Segunda = contadorErrouF1Segunda + 1}
            else 
              if(aux2[ww] == "s1"){contadorErrouSensor1Segunda = contadorErrouSensor1Segunda + 1}
            else
              if(aux2[ww] == "s2"){contadorErrouSensor2Segunda = contadorErrouSensor2Segunda + 1}
            else
              if(aux2[ww] == "c"){contadorErrouSensorCSegunda = contadorErrouSensorCSegunda + 1}
          }
          contadorDiferencasSegunda <- contadorDiferencasSegunda + 1
          contadorDeBuracosErradosSegunda = contadorDeBuracosErradosSegunda + 1
          #########################################################        
          #cat("Q: ",colnames(AA[ww]),"...:",as.character(AA[,ww]),"\t",
          #    "B: ",colnames(BB[ww]),"...:",as.character(BB[,ww]),"\n")
          #########################################################        
        }
      } # for ww
      
      
      #########################################################    
      #cat ("Qtde de Buracos certos  Segunda = ", (qtdBuracosLinha-contadorDiferencasSegunda), "\n")
      #cat ("Qtde de Buracos errados Segunda = ", contadorDiferencasSegunda, "\n")
      #cat ("Qtde de Buracos                 = ", qtdBuracosLinha, "\n")
      #########################################################    
      
      if(contadorDiferencasSegunda==0)
        qtdAcertouTudoSegunda = qtdAcertouTudoSegunda + 1    
      
    } # if(segundaMaiorProbabilidade>-1)
    
    ###################################################################################
    #  
    #  ____  
    # |___ \   
    #   __) |           Analisando os acertos e erros de Terceira
    #  |__ < 
    #  ___) |
    # |____/
    #
    ###################################################################################
    if(terceiraMaiorProbabilidade>-1){
      #########################################################
      #cat ("Terceira Maior Probabiliade = ", terceiraMaiorProbabilidade,"\n")
      #print ("Linha com buracos  = ")
      #print (dado[,order(names(dado))])
      #########################################################    
      
      ## auxiliar para identificar os buracos originais
      aux1 <- dado[,order(names(dado))]
      aux2 <- colnames(aux1)
      
      #########################################################
      #print ("Query selecionada  = ")
      #print (queryTerceiraMaiorProbabilidade[,order(names(queryTerceiraMaiorProbabilidade))])
      #print ("Linha sem buracos  = ")
      #print (base_teste_sem_buraco[i1,order(names(bn_df))])
      #########################################################
      contadorDiferencasTerceira = 0
      ContadorIgualdadesTerceira = 0
      
      # AA guarda a query com os buracos preenchidos com os valores de maior probabilidade
      AA <- queryTerceiraMaiorProbabilidade[,order(names(queryTerceiraMaiorProbabilidade))]
      # BB guarda a linha com os valores originais para verificar se acertou ou não
      BB <- base_teste_sem_buraco[i1,order(names(bn_df))]
      
      for(ww in 1:length(dado)){
        if(as.character(AA[,ww]) == as.character(BB[,ww])){
          if(is.na(aux1[ww]) || is.null(aux1[ww]) || (aux1[ww]=="NULL")){
            ## acertou um buraco
            if (aux2[ww] == "a1"){contadorAcertouAtuador1Terceira = contadorAcertouAtuador1Terceira + 1} 
            else
              if(aux2[ww] == "r"){contadorAcertouRTerceira = contadorAcertouRTerceira + 1}
            else 
              if(aux2[ww] == "f1"){contadorAcertouF1Terceira = contadorAcertouF1Terceira + 1}
            else 
              if(aux2[ww] == "s1"){contadorAcertouSensor1Terceira = contadorAcertouSensor1Terceira + 1}
            else
              if(aux2[ww] == "s2"){contadorAcertouSensor2Terceira = contadorAcertouSensor2Terceira + 1}
            else
              if(aux2[ww] == "c"){contadorAcertouSensorcTerceira = contadorAcertouSensorCTerceira + 1}
          }
          
          ContadorIgualdadesTerceira <- ContadorIgualdadesTerceira + 1
        } else {
          if(is.na(aux1[ww]) || is.null(aux1[ww]) || (aux1[ww]=="NULL")){
            ## errou um buraco
            if (aux2[ww] == "a1"){contadorErrouAtuador1Terceira = contadorErrouAtuador1Terceira + 1} 
            else
              if(aux2[ww] == "r"){contadorErrouRTerceira = contadorErrouRTerceira + 1}
            else 
              if(aux2[ww] == "f1"){contadorErrouF1Terceira = contadorErrouF1Terceira + 1}
            else 
              if(aux2[ww] == "s1"){contadorErrouSensor1Terceira = contadorErrouSensor1Terceira + 1}
            else
              if(aux2[ww] == "s2"){contadorErrouSensor2Terceira = contadorErrouSensor2Terceira + 1}
            else
              if(aux2[ww] == "c"){contadorErrouSensorCTerceira = contadorErrouSensorCTerceira + 1}
          }
          contadorDiferencasTerceira <- contadorDiferencasTerceira + 1
          contadorDeBuracosErradosTerceira = contadorDeBuracosErradosTerceira + 1
          #########################################################        
          #cat("Q: ",colnames(AA[ww]),"...:",as.character(AA[,ww]),"\t",
          #    "B: ",colnames(BB[ww]),"...:",as.character(BB[,ww]),"\n")
          #########################################################        
        }
      } # for ww
      
      
      #########################################################    
      #cat ("Qtde de Buracos certos  Terceira = ", (qtdBuracosLinha-contadorDiferencasTerceira), "\n")
      #cat ("Qtde de Buracos errados Terceira = ", contadorDiferencasTerceira, "\n")
      #cat ("Qtde de Buracos                  = ", qtdBuracosLinha, "\n")
      #########################################################    
      
      if(contadorDiferencasTerceira==0)
        qtdAcertouTudoTerceira = qtdAcertouTudoTerceira + 1    
      
    } # if(terceiraMaiorProbabilidade>-1)
    
  } #  i1
  ##########################################
  # fim do Laço principal
  ##########################################
  contadorAcertouSensorPrimeira <- contadorAcertouSensor1Primeira +
    contadorAcertouSensor2Primeira +
    contadorAcertouSensorCPrimeira
  
  contadorAcertouSensorSegunda  <- contadorAcertouSensor1Segunda  +
    contadorAcertouSensor2Segunda  +
    contadorAcertouSensorCSegunda
  
  contadorAcertouSensorTerceira <- contadorAcertouSensor1Terceira +
    contadorAcertouSensor2Terceira +
    contadorAcertouSensorCTerceira
  
  contadorErrouSensorPrimeira   <- contadorErrouSensor1Primeira   +
    contadorErrouSensor2Primeira   +
    contadorErrouSensorCPrimeira
  
  contadorErrouSensorSegunda    <- contadorErrouSensor1Segunda    +
    contadorErrouSensor2Segunda    +
    contadorErrouSensorCSegunda
  
  contadorErrouSensorTerceira   <- contadorErrouSensor1Terceira   +
    contadorErrouSensor2Terceira   +
    contadorErrouSensorCTerceira
  
  
  contadorAcertouAtuadorPrimeira <- contadorAcertouAtuador1Primeira
  
  contadorAcertouAtuadorSegunda  <- contadorAcertouAtuador1Segunda
  
  contadorAcertouAtuadorTerceira <- contadorAcertouAtuador1Terceira
  
  contadorErrouAtuadorPrimeira   <- contadorErrouAtuador1Primeira
  
  contadorErrouAtuadorSegunda    <- contadorErrouAtuador1Segunda
  
  contadorErrouAtuadorTerceira   <- contadorErrouAtuador1Terceira
  
  contadorAcertouFusaoPrimeira   <- contadorAcertouF1Primeira
  
  contadorAcertouFusaoSegunda    <- contadorAcertouF1Segunda
  
  contadorAcertouFusaoTerceira   <- contadorAcertouF1Terceira
  
  contadorErrouFusaoPrimeira     <- contadorErrouF1Primeira
  
  contadorErrouFusaoSegunda      <- contadorErrouF1Segunda
  
  contadorErrouFusaoTerceira     <- contadorErrouF1Terceira
  
  
  # atualizando o dataframe com base no ArquivoResultado
  df_resultado <- as.data.frame(fromJSON(ArquivoResultado))
  
  #variaveis para guardar no dataframe de resultado de processamento
  df_resultado$totBuracos[linhaResultado] = contadorDeBuracos
  
  df_resultado$totBuracosAcertouPrimeira[linhaResultado] = (contadorDeBuracos-contadorDeBuracosErradosPrimeira)
  df_resultado$totBuracosAcertouSegunda[linhaResultado] = (contadorDeBuracos-contadorDeBuracosErradosSegunda)
  df_resultado$totBuracosAcertouTerceira[linhaResultado] = (contadorDeBuracos-contadorDeBuracosErradosTerceira)
  
  df_resultado$totBuracosErrouPrimeira[linhaResultado] = contadorDeBuracosErradosPrimeira
  df_resultado$totBuracosErrouSegunda[linhaResultado] = contadorDeBuracosErradosSegunda
  df_resultado$totBuracosErrouTerceira[linhaResultado] = contadorDeBuracosErradosTerceira
  
  df_resultado$predicoes100CorretasPrimeira[linhaResultado] = qtdAcertouTudoPrimeira
  df_resultado$predicoes100CorretasSegunda[linhaResultado] = qtdAcertouTudoSegunda
  df_resultado$predicoes100CorretasTerceira[linhaResultado] = qtdAcertouTudoTerceira
  
  # acho melhor pegar a Primeira
  df_resultado$totBuracosSensores[linhaResultado] = contadorAcertouSensorPrimeira+contadorErrouSensorPrimeira
  
  df_resultado$acertosSensor1Primeira[linhaResultado] = contadorAcertouSensor1Primeira
  df_resultado$acertosSensor1Segunda[linhaResultado] = contadorAcertouSensor1Segunda
  df_resultado$acertosSensor1Terceira[linhaResultado] = contadorAcertouSensor1Terceira
  
  df_resultado$acertosSensor2Primeira[linhaResultado] = contadorAcertouSensor2Primeira
  df_resultado$acertosSensor2Segunda[linhaResultado] = contadorAcertouSensor2Segunda
  df_resultado$acertosSensor2Terceira[linhaResultado] = contadorAcertouSensor2Terceira
  
  df_resultado$acertosSensorCPrimeira[linhaResultado] = contadorAcertouSensorCPrimeira
  df_resultado$acertosSensorCSegunda[linhaResultado] = contadorAcertouSensorCSegunda
  df_resultado$acertosSensorCTerceira[linhaResultado] = contadorAcertouSensorCTerceira
  
  df_resultado$errosSensor1Primeira[linhaResultado] = contadorErrouSensor1Primeira
  df_resultado$errosSensor1Segunda[linhaResultado] = contadorErrouSensor1Segunda
  df_resultado$errosSensor1Terceira[linhaResultado] = contadorErrouSensor1Terceira
  
  df_resultado$errosSensor2Primeira[linhaResultado] = contadorErrouSensor2Primeira
  df_resultado$errosSensor2Segunda[linhaResultado] = contadorErrouSensor2Segunda
  df_resultado$errosSensor2Terceira[linhaResultado] = contadorErrouSensor2Terceira
  
  df_resultado$errosSensorCPrimeira[linhaResultado] = contadorErrouSensorCPrimeira
  df_resultado$errosSensorCSegunda[linhaResultado] = contadorErrouSensorCSegunda
  df_resultado$errosSensorCTerceira[linhaResultado] = contadorErrouSensorCTerceira
  
  # o calculo abaixo pode ter denominador zero e gerar NaN - colocar em um if
  if((contadorAcertouSensorPrimeira + contadorErrouSensorPrimeira)>0){
    df_resultado$porcAcertoSensorPrimeira[linhaResultado] = (contadorAcertouSensorPrimeira/(contadorAcertouSensorPrimeira + contadorErrouSensorPrimeira))*100.0    
  } else {
    df_resultado$porcAcertoSensorPrimeira[linhaResultado] = 0
  }
  
  if((contadorAcertouSensorSegunda + contadorErrouSensorSegunda)>0){
    df_resultado$porcAcertoSensorSegunda[linhaResultado] = (contadorAcertouSensorSegunda/(contadorAcertouSensorSegunda + contadorErrouSensorSegunda))*100.0    
  } else {
    df_resultado$porcAcertoSensorSegunda[linhaResultado] = 0
  }
  
  if((contadorAcertouSensorTerceira + contadorErrouSensorTerceira)>0){
    df_resultado$porcAcertoSensorTerceira[linhaResultado] = (contadorAcertouSensorTerceira/(contadorAcertouSensorTerceira + contadorErrouSensorTerceira))*100.0    
  } else {
    df_resultado$porcAcertoSensorTerceira[linhaResultado] = 0
  }
  
  # acho melhor pegar a Primeira
  df_resultado$totBuracosAcao[linhaResultado] = contadorAcertouRPrimeira + contadorErrouRPrimeira
  
  df_resultado$acertosAcaoPrimeira[linhaResultado] = contadorAcertouRPrimeira
  df_resultado$acertosAcaoSegunda[linhaResultado] = contadorAcertouRSegunda
  df_resultado$acertosAcaoTerceira[linhaResultado] = contadorAcertouRTerceira
  
  df_resultado$errosAcaoPrimeira[linhaResultado] = contadorErrouRPrimeira
  df_resultado$errosAcaoSegunda[linhaResultado] = contadorErrouRSegunda
  df_resultado$errosAcaoTerceira[linhaResultado] = contadorErrouRTerceira
  
  # o calculo abaixo pode ter denominador zero e gerar NaN - colocar em um if
  if((contadorAcertouRPrimeira + contadorErrouRPrimeira)>0){
    df_resultado$porcAcertoAcaoPrimeira[linhaResultado] = (contadorAcertouRPrimeira/(contadorAcertouRPrimeira+contadorErrouRPrimeira))*100.0    
  } else {
    df_resultado$porcAcertoAcaoPrimeira[linhaResultado] = 0    
  }
  
  if((contadorAcertouRSegunda + contadorErrouRSegunda)>0){
    df_resultado$porcAcertoAcaoSegunda[linhaResultado] = (contadorAcertouRSegunda/(contadorAcertouRSegunda+contadorErrouRSegunda))*100.0    
  } else {
    df_resultado$porcAcertoAcaoSegunda[linhaResultado] = 0    
  }
  
  if((contadorAcertouRTerceira + contadorErrouRTerceira)>0){
    df_resultado$porcAcertoAcaoTerceira[linhaResultado] = (contadorAcertouRTerceira/(contadorAcertouRTerceira+contadorErrouRTerceira))*100.0    
  } else {
    df_resultado$porcAcertoAcaoTerceira[linhaResultado] = 0    
  }
  
  # acho melhor pegar a Primeira  
  df_resultado$totBuracosSomafusao[linhaResultado] = contadorAcertouFusaoPrimeira + contadorErrouFusaoPrimeira
  
  df_resultado$acertosSomafusao1Primeira[linhaResultado] = contadorAcertouF1Primeira
  df_resultado$acertosSomafusao1Segunda[linhaResultado] = contadorAcertouF1Segunda
  df_resultado$acertosSomafusao1Terceira[linhaResultado] = contadorAcertouF1Terceira
  
  df_resultado$errosSomafusao1Primeira[linhaResultado] = contadorErrouF1Primeira
  df_resultado$errosSomafusao1Segunda[linhaResultado] = contadorErrouF1Segunda
  df_resultado$errosSomafusao1Terceira[linhaResultado] = contadorErrouF1Terceira
  
  # o calculo abaixo pode ter denominador zero e gerar NaN - colocar em um if
  if((contadorAcertouFusaoPrimeira + contadorErrouFusaoPrimeira)>0){
    df_resultado$porcAcertoSomafusaoPrimeira[linhaResultado] = (contadorAcertouFusaoPrimeira/(contadorAcertouFusaoPrimeira+contadorErrouFusaoPrimeira))*100.0    
  } else {
    df_resultado$porcAcertoSomafusaoPrimeira[linhaResultado] = 0
  }
  
  if((contadorAcertouFusaoSegunda + contadorErrouFusaoSegunda)>0){
    df_resultado$porcAcertoSomafusaoSegunda[linhaResultado] = (contadorAcertouFusaoSegunda/(contadorAcertouFusaoSegunda+contadorErrouFusaoSegunda))*100.0    
  } else {
    df_resultado$porcAcertoSomafusaoSegunda[linhaResultado] = 0
  }
  
  if((contadorAcertouFusaoTerceira + contadorErrouFusaoTerceira)>0){
    df_resultado$porcAcertoSomafusaoTerceira[linhaResultado] = (contadorAcertouFusaoTerceira/(contadorAcertouFusaoTerceira+contadorErrouFusaoTerceira))*100.0    
  } else {
    df_resultado$porcAcertoSomafusaoTerceira[linhaResultado] = 0
  }
  
  # acho melhor pegar a Primeira  
  df_resultado$totBuracosAtuador[linhaResultado] = contadorAcertouAtuadorPrimeira + contadorErrouAtuadorPrimeira
  
  df_resultado$acertosAtuador1Primeira[linhaResultado] = contadorAcertouAtuador1Primeira
  df_resultado$acertosAtuador1Segunda[linhaResultado] = contadorAcertouAtuador1Segunda
  df_resultado$acertosAtuador1Terceira[linhaResultado] = contadorAcertouAtuador1Terceira
  
  df_resultado$errosAtuador1Primeira[linhaResultado] = contadorErrouAtuador1Primeira
  df_resultado$errosAtuador1Segunda[linhaResultado] = contadorErrouAtuador1Segunda
  df_resultado$errosAtuador1Terceira[linhaResultado] = contadorErrouAtuador1Terceira
  
  # o calculo abaixo pode ter denominador zero e gerar NaN - colocar em um if
  if((contadorAcertouAtuadorPrimeira + contadorErrouAtuadorPrimeira)>0){
    df_resultado$porcAcertosAtuadorPrimeira[linhaResultado] = (contadorAcertouAtuadorPrimeira/(contadorAcertouAtuadorPrimeira + contadorErrouAtuadorPrimeira))*100.0    
  } else {
    df_resultado$porcAcertosAtuadorPrimeira[linhaResultado] = 0    
  }
  
  if((contadorAcertouAtuadorSegunda + contadorErrouAtuadorSegunda)){
    df_resultado$porcAcertosAtuadorSegunda[linhaResultado] = (contadorAcertouAtuadorSegunda/(contadorAcertouAtuadorSegunda + contadorErrouAtuadorSegunda))*100.0    
  } else {
    df_resultado$porcAcertosAtuadorSegunda[linhaResultado] = 0    
  }
  
  if((contadorAcertouAtuadorTerceira + contadorErrouAtuadorTerceira)){
    df_resultado$porcAcertosAtuadorTerceira[linhaResultado] = (contadorAcertouAtuadorTerceira/(contadorAcertouAtuadorTerceira + contadorErrouAtuadorTerceira))*100.0    
  } else {
    df_resultado$porcAcertosAtuadorTerceira[linhaResultado] = 0    
  }
  
  
  ##################################################################
  # gravação do arquivo de resultado do processamento
  write_json(df_resultado, ArquivoResultado)
  
} # fim da função processaUmArquivoCenario7


calcularEstatisticas <- function(){
  
  # fazer as estatísticas com os dados do resultado
  
  #        _                                 
  #       | |                                
  #     __| |_   _ _ __ ___  _ __ ___  _   _ 
  #    / _` | | | | '_ ` _ \| '_ ` _ \| | | |
  #   | (_| | |_| | | | | | | | | | | | |_| |
  #    \__,_|\__,_|_| |_| |_|_| |_| |_|\__, |
  #                                     __/ |
  #                                    |___/   


}

##################################################################################
# MAIN PROGRAM
#
#
#
##################################################################################
# Início do Processamento
inicioProcessamento = proc.time()

# set seed em função do horario (EPOCH TIME)
set.seed(as.numeric(Sys.time()))

# lendo os dados para processar o experimento
ArquivoExperimento    <- paste(NomeExperimento,".json",sep="",collapse="")
lista_experimento     <- fromJSON(ArquivoExperimento)

# verificando o que já foi processado do experimento
ArquivoControle   <- paste(NomeExperimento,"_controle",".json",sep="",collapse="")

if(file.exists(ArquivoControle)){
  lista_controle <- fromJSON(ArquivoControle)
} else  {
  # gerando uma lista de controle do processamento do experimento
  lista_controle <- list(rep(0, length(lista_experimento$registros)),
                         rep(0, lista_experimento$bateria * 
                                length(lista_experimento$registros) * 
                                length(lista_experimento$furos))
                         )
  names(lista_controle) <- c("arquivos","processados")

  # geração dos arquivos com dados
  write_json(lista_controle, ArquivoControle)
}

# carregar arquivo JSON com o resultado do processamento (se existir)
ArquivoResultado <- paste(NomeExperimento,"_resultado",".json",sep="",collapse="")

if(file.exists(ArquivoResultado)){
  df_resultado <- as.data.frame(fromJSON(ArquivoResultado))
} else {
  if(lista_experimento$cenario=="1"){
    # gerando um dataframe para o resultado do processamento (90 variáveis)
    print("gerando um dataframe para o resultado do processamento (90 variáveis)")
    df_resultado <- data.frame(matrix(ncol = 90, nrow = 0))
    nomesColunas <- c("nomeExperimento",
                      "vezExecucao",
                      "qtdeRegistros",
                      "porcBuracos",
                      "totBuracos",
                      "totBuracosAcertouPrimeira",
                      "totBuracosAcertouSegunda",
                      "totBuracosAcertouTerceira",
                      "totBuracosErrouPrimeira",
                      "totBuracosErrouSegunda",
                      "totBuracosErrouTerceira",
                      "predicoes100CorretasPrimeira",
                      "predicoes100CorretasSegunda",
                      "predicoes100CorretasTerceira",
                      "totBuracosSensores",
                      "acertosSensor1Primeira",
                      "acertosSensor1Segunda",
                      "acertosSensor1Terceira",
                      "acertosSensor2Primeira",
                      "acertosSensor2Segunda",
                      "acertosSensor2Terceira",
                      "acertosSensor3Primeira",
                      "acertosSensor3Segunda",
                      "acertosSensor3Terceira",
                      "acertosSensor4Primeira",
                      "acertosSensor4Segunda",
                      "acertosSensor4Terceira",
                      "errosSensor1Primeira",
                      "errosSensor1Segunda",
                      "errosSensor1Terceira",
                      "errosSensor2Primeira",
                      "errosSensor2Segunda",
                      "errosSensor2Terceira",
                      "errosSensor3Primeira",
                      "errosSensor3Segunda",
                      "errosSensor3Terceira",
                      "errosSensor4Primeira",
                      "errosSensor4Segunda",
                      "errosSensor4Terceira",
                      "porcAcertoSensorPrimeira",
                      "porcAcertoSensorSegunda",
                      "porcAcertoSensorTerceira",
                      "totBuracosAcao",
                      "acertosAcaoPrimeira",
                      "acertosAcaoSegunda",
                      "acertosAcaoTerceira",
                      "errosAcaoPrimeira",
                      "errosAcaoSegunda",
                      "errosAcaoTerceira",
                      "porcAcertoAcaoPrimeira",
                      "porcAcertoAcaoSegunda",
                      "porcAcertoAcaoTerceira",
                      "totBuracosSomafusao",
                      "acertosSomafusao1Primeira",
                      "acertosSomafusao1Segunda",
                      "acertosSomafusao1Terceira",
                      "errosSomafusao1Primeira",
                      "errosSomafusao1Segunda",
                      "errosSomafusao1Terceira",
                      "porcAcertoSomafusaoPrimeira",
                      "porcAcertoSomafusaoSegunda",
                      "porcAcertoSomafusaoTerceira",
                      "totBuracosAtuador",
                      "acertosAtuador1Primeira",
                      "acertosAtuador1Segunda",
                      "acertosAtuador1Terceira",
                      "acertosAtuador2Primeira",
                      "acertosAtuador2Segunda",
                      "acertosAtuador2Terceira",
                      "acertosAtuador3Primeira",
                      "acertosAtuador3Segunda",
                      "acertosAtuador3Terceira",
                      "acertosAtuador4Primeira",
                      "acertosAtuador4Segunda",
                      "acertosAtuador4Terceira",
                      "errosAtuador1Primeira",
                      "errosAtuador1Segunda",
                      "errosAtuador1Terceira",
                      "errosAtuador2Primeira",
                      "errosAtuador2Segunda",
                      "errosAtuador2Terceira",
                      "errosAtuador3Primeira",
                      "errosAtuador3Segunda",
                      "errosAtuador3Terceira",
                      "errosAtuador4Primeira",
                      "errosAtuador4Segunda",
                      "errosAtuador4Terceira",
                      "porcAcertosAtuadorPrimeira",
                      "porcAcertosAtuadorSegunda",
                      "porcAcertosAtuadorTerceira")
    
    # carregando o dataFrame com as linhas
    for(i in 1:lista_experimento$bateria){
      for(j in 1:length(lista_experimento$registros)){
        for(k in 1:length(lista_experimento$furos)){
          #cat("i=",i," j=",j," k=",k,"\n")
          df_resultado <-rbind(df_resultado,list(
            NomeExperimento,                # nomeExperimento
            i,                              # vezExecucao
            lista_experimento$registros[j], # qtdeRegistros
            lista_experimento$furos[k],     # porcBuracos
            -1, # totBuracos
            -1, # totBuracosAcertouPrimeira
            -1, # totBuracosAcertouSegunda
            -1, # totBuracosAcertouTerceira
            -1, # totBuracosErrouPrimeira
            -1, # totBuracosErrouSegunda
            -1, # totBuracosErrouTerceira
            -1, # predicoes100CorretasPrimeira
            -1, # predicoes100CorretasSegunda
            -1, # predicoes100CorretasTerceira
            -1, # totBuracosSensores
            -1, # acertosSensor1Primeira
            -1, # acertosSensor1Segunda
            -1, # acertosSensor1Terceira
            -1, # acertosSensor2Primeira
            -1, # acertosSensor2Segunda
            -1, # acertosSensor2Terceira
            -1, # acertosSensor3Primeira
            -1, # acertosSensor3Segunda
            -1, # acertosSensor3Terceira
            -1, # acertosSensor4Primeira
            -1, # acertosSensor4Segunda
            -1, # acertosSensor4Terceira
            -1, # errosSensor1Primeira
            -1, # errosSensor1Segunda
            -1, # errosSensor1Terceira
            -1, # errosSensor2Primeira
            -1, # errosSensor2Segunda
            -1, # errosSensor2Terceira
            -1, # errosSensor3Primeira
            -1, # errosSensor3Segunda
            -1, # errosSensor3Terceira
            -1, # errosSensor4Primeira
            -1, # errosSensor4Segunda
            -1, # errosSensor4Terceira
            -1, # porcAcertoSensorPrimeira
            -1, # porcAcertoSensorSegunda
            -1, # porcAcertoSensorTerceira
            -1, # totBuracosAcao
            -1, # acertosAcaoPrimeira
            -1, # acertosAcaoSegunda
            -1, # acertosAcaoTerceira
            -1, # errosAcaoPrimeira
            -1, # errosAcaoSegunda
            -1, # errosAcaoTerceira
            -1, # porcAcertoAcaoPrimeira
            -1, # porcAcertoAcaoSegunda
            -1, # porcAcertoAcaoTerceira
            -1, # totBuracosSomafusao
            -1, # acertosSomafusao1Primeira
            -1, # acertosSomafusao1Segunda
            -1, # acertosSomafusao1Terceira
            -1, # errosSomafusao1Primeira
            -1, # errosSomafusao1Segunda
            -1, # errosSomafusao1Terceira
            -1, # porcAcertoSomafusaoPrimeira
            -1, # porcAcertoSomafusaoSegunda
            -1, # porcAcertoSomafusaoTerceira
            -1, # totBuracosAtuador
            -1, # acertosAtuador1Primeira
            -1, # acertosAtuador1Segunda
            -1, # acertosAtuador1Terceira
            -1, # acertosAtuador2Primeira
            -1, # acertosAtuador2Segunda
            -1, # acertosAtuador2Terceira
            -1, # acertosAtuador3Primeira
            -1, # acertosAtuador3Segunda
            -1, # acertosAtuador3Terceira
            -1, # acertosAtuador4Primeira
            -1, # acertosAtuador4Segunda
            -1, # acertosAtuador4Terceira
            -1, # errosAtuador1Primeira
            -1, # errosAtuador1Segunda
            -1, # errosAtuador1Terceira
            -1, # errosAtuador2Primeira
            -1, # errosAtuador2Segunda
            -1, # errosAtuador2Terceira
            -1, # errosAtuador3Primeira
            -1, # errosAtuador3Segunda
            -1, # errosAtuador3Terceira
            -1, # errosAtuador4Primeira
            -1, # errosAtuador4Segunda
            -1, # errosAtuador4Terceira
            -1, # porcAcertosAtuadorPrimeira
            -1, # porcAcertosAtuadorSegunda
            -1  # porcAcertosAtuadorTerceira
          ))  
        } # for k
      } # for j
    } # for i    
    #cat("saiu do laco ijk\n")
  } 
  
  if(lista_experimento$cenario=="2"){
    # gerando um dataframe para o resultado do processamento (93 variáveis)
    df_resultado <- data.frame(matrix(ncol = 93, nrow = 0))
    nomesColunas <- c("nomeExperimento",
                      "vezExecucao",
                      "qtdeRegistros",
                      "porcBuracos",
                      "totBuracos",
                      "totBuracosAcertouPrimeira",
                      "totBuracosAcertouSegunda",
                      "totBuracosAcertouTerceira",
                      "totBuracosErrouPrimeira",
                      "totBuracosErrouSegunda",
                      "totBuracosErrouTerceira",
                      "predicoes100CorretasPrimeira",
                      "predicoes100CorretasSegunda",
                      "predicoes100CorretasTerceira",
                      "totBuracosSensores",
                      "acertosSensor1Primeira",
                      "acertosSensor1Segunda",
                      "acertosSensor1Terceira",
                      "acertosSensor2Primeira",
                      "acertosSensor2Segunda",
                      "acertosSensor2Terceira",
                      "acertosSensor3Primeira",
                      "acertosSensor3Segunda",
                      "acertosSensor3Terceira",
                      "acertosSensor4Primeira",
                      "acertosSensor4Segunda",
                      "acertosSensor4Terceira",
                      "acertosSensorCPrimeira",
                      "acertosSensorCSegunda",
                      "acertosSensorCTerceira",
                      "errosSensor1Primeira",
                      "errosSensor1Segunda",
                      "errosSensor1Terceira",
                      "errosSensor2Primeira",
                      "errosSensor2Segunda",
                      "errosSensor2Terceira",
                      "errosSensor3Primeira",
                      "errosSensor3Segunda",
                      "errosSensor3Terceira",
                      "errosSensor4Primeira",
                      "errosSensor4Segunda",
                      "errosSensor4Terceira",
                      "errosSensorCPrimeira",
                      "errosSensorCSegunda",
                      "errosSensorCTerceira",
                      "porcAcertoSensorPrimeira",
                      "porcAcertoSensorSegunda",
                      "porcAcertoSensorTerceira",
                      "totBuracosAcao",
                      "acertosAcaoPrimeira",
                      "acertosAcaoSegunda",
                      "acertosAcaoTerceira",
                      "errosAcaoPrimeira",
                      "errosAcaoSegunda",
                      "errosAcaoTerceira",
                      "porcAcertoAcaoPrimeira",
                      "porcAcertoAcaoSegunda",
                      "porcAcertoAcaoTerceira",
                      "totBuracosSomafusao",
                      "acertosSomafusao1Primeira",
                      "acertosSomafusao1Segunda",
                      "acertosSomafusao1Terceira",
                      "acertosSomafusao2Primeira",
                      "acertosSomafusao2Segunda",
                      "acertosSomafusao2Terceira",
                      "errosSomafusao1Primeira",
                      "errosSomafusao1Segunda",
                      "errosSomafusao1Terceira",
                      "errosSomafusao2Primeira",
                      "errosSomafusao2Segunda",
                      "errosSomafusao2Terceira",
                      "porcAcertoSomafusao1Primeira",
                      "porcAcertoSomafusao1Segunda",
                      "porcAcertoSomafusao1Terceira",
                      "porcAcertoSomafusao2Primeira",
                      "porcAcertoSomafusao2Segunda",
                      "porcAcertoSomafusao2Terceira",
                      "totBuracosAtuador",
                      "acertosAtuador1Primeira",
                      "acertosAtuador1Segunda",
                      "acertosAtuador1Terceira",
                      "acertosAtuador2Primeira",
                      "acertosAtuador2Segunda",
                      "acertosAtuador2Terceira",
                      "errosAtuador1Primeira",
                      "errosAtuador1Segunda",
                      "errosAtuador1Terceira",
                      "errosAtuador2Primeira",
                      "errosAtuador2Segunda",
                      "errosAtuador2Terceira",
                      "porcAcertosAtuadorPrimeira",
                      "porcAcertosAtuadorSegunda",
                      "porcAcertosAtuadorTerceira")
    
    # carregando o dataFrame com as linhas
    for(i in 1:lista_experimento$bateria){
      for(j in 1:length(lista_experimento$registros)){
        for(k in 1:length(lista_experimento$furos)){
          df_resultado <-rbind(df_resultado,list(
            NomeExperimento,                # nomeExperimento
            i,                              # vezExecucao
            lista_experimento$registros[j], # qtdeRegistros
            lista_experimento$furos[k],     # porcBuracos
            -1, # totBuracos
            -1, # totBuracosAcertouPrimeira
            -1, # totBuracosAcertouSegunda
            -1, # totBuracosAcertouTerceira
            -1, # totBuracosErrouPrimeira
            -1, # totBuracosErrouSegunda
            -1, # totBuracosErrouTerceira
            -1, # predicoes100CorretasPrimeira
            -1, # predicoes100CorretasSegunda
            -1, # predicoes100CorretasTerceira
            -1, # totBuracosSensores
            -1, # acertosSensor1Primeira
            -1, # acertosSensor1Segunda
            -1, # acertosSensor1Terceira
            -1, # acertosSensor2Primeira
            -1, # acertosSensor2Segunda
            -1, # acertosSensor2Terceira
            -1, # acertosSensor3Primeira
            -1, # acertosSensor3Segunda
            -1, # acertosSensor3Terceira
            -1, # acertosSensor4Primeira
            -1, # acertosSensor4Segunda
            -1, # acertosSensor4Terceira
            -1, # acertosSensorCPrimeira
            -1, # acertosSensorCSegunda
            -1, # acertosSensorCTerceira
            -1, # errosSensor1Primeira
            -1, # errosSensor1Segunda
            -1, # errosSensor1Terceira
            -1, # errosSensor2Primeira
            -1, # errosSensor2Segunda
            -1, # errosSensor2Terceira
            -1, # errosSensor3Primeira
            -1, # errosSensor3Segunda
            -1, # errosSensor3Terceira
            -1, # errosSensor4Primeira
            -1, # errosSensor4Segunda
            -1, # errosSensor4Terceira
            -1, # errosSensorCPrimeira
            -1, # errosSensorCSegunda
            -1, # errosSensorCTerceira
            -1, # porcAcertoSensorPrimeira
            -1, # porcAcertoSensorSegunda
            -1, # porcAcertoSensorTerceira
            -1, # totBuracosAcao
            -1, # acertosAcaoPrimeira
            -1, # acertosAcaoSegunda
            -1, # acertosAcaoTerceira
            -1, # errosAcaoPrimeira
            -1, # errosAcaoSegunda
            -1, # errosAcaoTerceira
            -1, # porcAcertoAcaoPrimeira
            -1, # porcAcertoAcaoSegunda
            -1, # porcAcertoAcaoTerceira
            -1, # totBuracosSomafusao
            -1, # acertosSomafusao1Primeira
            -1, # acertosSomafusao1Segunda
            -1, # acertosSomafusao1Terceira
            -1, # acertosSomafusao2Primeira
            -1, # acertosSomafusao2Segunda
            -1, # acertosSomafusao2Terceira
            -1, # errosSomafusao1Primeira
            -1, # errosSomafusao1Segunda
            -1, # errosSomafusao1Terceira
            -1, # errosSomafusao2Primeira
            -1, # errosSomafusao2Segunda
            -1, # errosSomafusao2Terceira
            -1, # porcAcertoSomafusao1Primeira
            -1, # porcAcertoSomafusao1Segunda
            -1, # porcAcertoSomafusao1Terceira
            -1, # porcAcertoSomafusao2Primeira
            -1, # porcAcertoSomafusao2Segunda
            -1, # porcAcertoSomafusao2Terceira
            -1, # totBuracosAtuador
            -1, # acertosAtuador1Primeira
            -1, # acertosAtuador1Segunda
            -1, # acertosAtuador1Terceira
            -1, # acertosAtuador2Primeira
            -1, # acertosAtuador2Segunda
            -1, # acertosAtuador2Terceira
            -1, # errosAtuador1Primeira
            -1, # errosAtuador1Segunda
            -1, # errosAtuador1Terceira
            -1, # errosAtuador2Primeira
            -1, # errosAtuador2Segunda
            -1, # errosAtuador2Terceira
            -1, # porcAcertosAtuadorPrimeira
            -1, # porcAcertosAtuadorSegunda
            -1  # porcAcertosAtuadorTerceira
          ))  
        } # for k
      } # for j
    } # for i
    
  } 
  
  if(lista_experimento$cenario=="3"){
    # gerando um dataframe para o resultado do processamento (158 variáveis)
    df_resultado <- data.frame(matrix(ncol = 158, nrow = 0))
    nomesColunas <- c("nomeExperimento",
                      "vezExecucao",
                      "qtdeRegistros",
                      "porcBuracos",
                      "totBuracos",
                      "totBuracosAcertouPrimeira",
                      "totBuracosAcertouSegunda",
                      "totBuracosAcertouTerceira",
                      "totBuracosErrouPrimeira",
                      "totBuracosErrouSegunda",
                      "totBuracosErrouTerceira",
                      "predicoes100CorretasPrimeira",
                      "predicoes100CorretasSegunda",
                      "predicoes100CorretasTerceira",
                      "acertosenPrimeira",
                      "acertosenSegunda",
                      "acertosenTerceira",
                      "acertosmiPrimeira",
                      "acertosmiSegunda",
                      "acertosmiTerceira",
                      "acertosgtbPrimeira",
                      "acertosgtbSegunda",
                      "acertosgtbTerceira",
                      "acertosmq1Primeira",
                      "acertosmq1Segunda",
                      "acertosmq1Terceira",
                      "acertoslsPrimeira",
                      "acertoslsSegunda",
                      "acertoslsTerceira",
                      "acertosmq2Primeira",
                      "acertosmq2Segunda",
                      "acertosmq2Terceira",
                      "acertoslasPrimeira",
                      "acertoslasSegunda",
                      "acertoslasTerceira",
                      "acertosoutFogPrimeira",
                      "acertosoutFogSegunda",
                      "acertosoutFogTerceira",
                      "acertosaa1Primeira",
                      "acertosaa1Segunda",
                      "acertosaa1Terceira",
                      "acertosiotPrimeira",
                      "acertosiotSegunda",
                      "acertosiotTerceira",
                      "acertosmoPrimeira",
                      "acertosmoSegunda",
                      "acertosmoTerceira",
                      "acertosoriPrimeira",
                      "acertosoriSegunda",
                      "acertosoriTerceira",
                      "acertosqlPrimeira",
                      "acertosqlSegunda",
                      "acertosqlTerceira",
                      "acertoscdbPrimeira",
                      "acertoscdbSegunda",
                      "acertoscdbTerceira",
                      "acertosaa2Primeira",
                      "acertosaa2Segunda",
                      "acertosaa2Terceira",
                      "acertosoutCloudPrimeira",
                      "acertosoutCloudSegunda",
                      "acertosoutCloudTerceira",
                      "errosenPrimeira",
                      "errosenSegunda",
                      "errosenTerceira",
                      "errosmiPrimeira",
                      "errosmiSegunda",
                      "errosmiTerceira",
                      "errosgtbPrimeira",
                      "errosgtbSegunda",
                      "errosgtbTerceira",
                      "errosmq1Primeira",
                      "errosmq1Segunda",
                      "errosmq1Terceira",
                      "erroslsPrimeira",
                      "erroslsSegunda",
                      "erroslsTerceira",
                      "errosmq2Primeira",
                      "errosmq2Segunda",
                      "errosmq2Terceira",
                      "erroslasPrimeira",
                      "erroslasSegunda",
                      "erroslasTerceira",
                      "errosoutFogPrimeira",
                      "errosoutFogSegunda",
                      "errosoutFogTerceira",
                      "errosaa1Primeira",
                      "errosaa1Segunda",
                      "errosaa1Terceira",
                      "errosiotPrimeira",
                      "errosiotSegunda",
                      "errosiotTerceira",
                      "errosmoPrimeira",
                      "errosmoSegunda",
                      "errosmoTerceira",
                      "errosoriPrimeira",
                      "errosoriSegunda",
                      "errosoriTerceira",
                      "errosqlPrimeira",
                      "errosqlSegunda",
                      "errosqlTerceira",
                      "erroscdbPrimeira",
                      "erroscdbSegunda",
                      "erroscdbTerceira",
                      "errosaa2Primeira",
                      "errosaa2Segunda",
                      "errosaa2Terceira",
                      "errosoutCloudPrimeira",
                      "errosoutCloudSegunda",
                      "errosoutCloudTerceira",
                      "porcAcertoenPrimeira",
                      "porcAcertoenSegunda",
                      "porcAcertoenTerceira",
                      "porcAcertomiPrimeira",
                      "porcAcertomiSegunda",
                      "porcAcertomiTerceira",
                      "porcAcertogtbPrimeira",
                      "porcAcertogtbSegunda",
                      "porcAcertogtbTerceira",
                      "porcAcertomq1Primeira",
                      "porcAcertomq1Segunda",
                      "porcAcertomq1Terceira",
                      "porcAcertolsPrimeira",
                      "porcAcertolsSegunda",
                      "porcAcertolsTerceira",
                      "porcAcertomq2Primeira",
                      "porcAcertomq2Segunda",
                      "porcAcertomq2Terceira",
                      "porcAcertolasPrimeira",
                      "porcAcertolasSegunda",
                      "porcAcertolasTerceira",
                      "porcAcertooutFogPrimeira",
                      "porcAcertooutFogSegunda",
                      "porcAcertooutFogTerceira",
                      "porcAcertoaa1Primeira",
                      "porcAcertoaa1Segunda",
                      "porcAcertoaa1Terceira",
                      "porcAcertoiotPrimeira",
                      "porcAcertoiotSegunda",
                      "porcAcertoiotTerceira",
                      "porcAcertomoPrimeira",
                      "porcAcertomoSegunda",
                      "porcAcertomoTerceira",
                      "porcAcertooriPrimeira",
                      "porcAcertooriSegunda",
                      "porcAcertooriTerceira",
                      "porcAcertoqlPrimeira",
                      "porcAcertoqlSegunda",
                      "porcAcertoqlTerceira",
                      "porcAcertocdbPrimeira",
                      "porcAcertocdbSegunda",
                      "porcAcertocdbTerceira",
                      "porcAcertoaa2Primeira",
                      "porcAcertoaa2Segunda",
                      "porcAcertoaa2Terceira",
                      "porcAcertooutCloudPrimeira",
                      "porcAcertooutCloudSegunda",
                      "porcAcertooutCloudTerceira")
    
    # carregando o dataFrame com as linhas
    for(i in 1:lista_experimento$bateria){
      for(j in 1:length(lista_experimento$registros)){
        for(k in 1:length(lista_experimento$furos)){
          df_resultado <-rbind(df_resultado,list(
            NomeExperimento,                # nomeExperimento
            i,                              # vezExecucao
            lista_experimento$registros[j], # qtdeRegistros
            lista_experimento$furos[k],     # porcBuracos
            -1, # totBuracos
            -1, # totBuracosAcertouPrimeira
            -1, # totBuracosAcertouSegunda
            -1, # totBuracosAcertouTerceira
            -1, # totBuracosErrouPrimeira
            -1, # totBuracosErrouSegunda
            -1, # totBuracosErrouTerceira
            -1, # predicoes100CorretasPrimeira
            -1, # predicoes100CorretasSegunda
            -1, # predicoes100CorretasTerceira
            -1, # acertosenPrimeira
            -1, # acertosenSegunda
            -1, # acertosenTerceira
            -1, # acertosmiPrimeira
            -1, # acertosmiSegunda
            -1, # acertosmiTerceira
            -1, # acertosgtbPrimeira
            -1, # acertosgtbSegunda
            -1, # acertosgtbTerceira
            -1, # acertosmq1Primeira
            -1, # acertosmq1Segunda
            -1, # acertosmq1Terceira
            -1, # acertoslsPrimeira
            -1, # acertoslsSegunda
            -1, # acertoslsTerceira
            -1, # acertosmq2Primeira
            -1, # acertosmq2Segunda
            -1, # acertosmq2Terceira
            -1, # acertoslasPrimeira
            -1, # acertoslasSegunda
            -1, # acertoslasTerceira
            -1, # acertosoutFogPrimeira
            -1, # acertosoutFogSegunda
            -1, # acertosoutFogTerceira
            -1, # acertosaa1Primeira
            -1, # acertosaa1Segunda
            -1, # acertosaa1Terceira
            -1, # acertosiotPrimeira
            -1, # acertosiotSegunda
            -1, # acertosiotTerceira
            -1, # acertosmoPrimeira
            -1, # acertosmoSegunda
            -1, # acertosmoTerceira
            -1, # acertosoriPrimeira
            -1, # acertosoriSegunda
            -1, # acertosoriTerceira
            -1, # acertosqlPrimeira
            -1, # acertosqlSegunda
            -1, # acertosqlTerceira
            -1, # acertoscdbPrimeira
            -1, # acertoscdbSegunda
            -1, # acertoscdbTerceira
            -1, # acertosaa2Primeira
            -1, # acertosaa2Segunda
            -1, # acertosaa2Terceira
            -1, # acertosoutCloudPrimeira
            -1, # acertosoutCloudSegunda
            -1, # acertosoutCloudTerceira
            -1, # errosenPrimeira
            -1, # errosenSegunda
            -1, # errosenTerceira
            -1, # errosmiPrimeira
            -1, # errosmiSegunda
            -1, # errosmiTerceira
            -1, # errosgtbPrimeira
            -1, # errosgtbSegunda
            -1, # errosgtbTerceira
            -1, # errosmq1Primeira
            -1, # errosmq1Segunda
            -1, # errosmq1Terceira
            -1, # erroslsPrimeira
            -1, # erroslsSegunda
            -1, # erroslsTerceira
            -1, # errosmq2Primeira
            -1, # errosmq2Segunda
            -1, # errosmq2Terceira
            -1, # erroslasPrimeira
            -1, # erroslasSegunda
            -1, # erroslasTerceira
            -1, # errosoutFogPrimeira
            -1, # errosoutFogSegunda
            -1, # errosoutFogTerceira
            -1, # errosaa1Primeira
            -1, # errosaa1Segunda
            -1, # errosaa1Terceira
            -1, # errosiotPrimeira
            -1, # errosiotSegunda
            -1, # errosiotTerceira
            -1, # errosmoPrimeira
            -1, # errosmoSegunda
            -1, # errosmoTerceira
            -1, # errosoriPrimeira
            -1, # errosoriSegunda
            -1, # errosoriTerceira
            -1, # errosqlPrimeira
            -1, # errosqlSegunda
            -1, # errosqlTerceira
            -1, # erroscdbPrimeira
            -1, # erroscdbSegunda
            -1, # erroscdbTerceira
            -1, # errosaa2Primeira
            -1, # errosaa2Segunda
            -1, # errosaa2Terceira
            -1, # errosoutCloudPrimeira
            -1, # errosoutCloudSegunda
            -1, # errosoutCloudTerceira
            -1, # porcAcertoenPrimeira
            -1, # porcAcertoenSegunda
            -1, # porcAcertoenTerceira
            -1, # porcAcertomiPrimeira
            -1, # porcAcertomiSegunda
            -1, # porcAcertomiTerceira
            -1, # porcAcertogtbPrimeira
            -1, # porcAcertogtbSegunda
            -1, # porcAcertogtbTerceira
            -1, # porcAcertomq1Primeira
            -1, # porcAcertomq1Segunda
            -1, # porcAcertomq1Terceira
            -1, # porcAcertolsPrimeira
            -1, # porcAcertolsSegunda
            -1, # porcAcertolsTerceira
            -1, # porcAcertomq2Primeira
            -1, # porcAcertomq2Segunda
            -1, # porcAcertomq2Terceira
            -1, # porcAcertolasPrimeira
            -1, # porcAcertolasSegunda
            -1, # porcAcertolasTerceira
            -1, # porcAcertooutFogPrimeira
            -1, # porcAcertooutFogSegunda
            -1, # porcAcertooutFogTerceira
            -1, # porcAcertoaa1Primeira
            -1, # porcAcertoaa1Segunda
            -1, # porcAcertoaa1Terceira
            -1, # porcAcertoiotPrimeira
            -1, # porcAcertoiotSegunda
            -1, # porcAcertoiotTerceira
            -1, # porcAcertomoPrimeira
            -1, # porcAcertomoSegunda
            -1, # porcAcertomoTerceira
            -1, # porcAcertooriPrimeira
            -1, # porcAcertooriSegunda
            -1, # porcAcertooriTerceira
            -1, # porcAcertoqlPrimeira
            -1, # porcAcertoqlSegunda
            -1, # porcAcertoqlTerceira
            -1, # porcAcertocdbPrimeira
            -1, # porcAcertocdbSegunda
            -1, # porcAcertocdbTerceira
            -1, # porcAcertoaa2Primeira
            -1, # porcAcertoaa2Segunda
            -1, # porcAcertoaa2Terceira
            -1, # porcAcertooutCloudPrimeira
            -1, # porcAcertooutCloudSegunda
            -1  # porcAcertooutCloudTerceira
            ))  
        } # for k
      } # for j
    } # for i
    
  } 
  
  
  if(lista_experimento$cenario=="4"){
    # gerando um dataframe para o resultado do processamento 
    df_resultado <- data.frame(matrix(ncol = 77, nrow = 0))
    nomesColunas <- c("nomeExperimento",
                      "vezExecucao",
                      "qtdeRegistros",
                      "porcBuracos",
                      "totBuracos",
                      "totBuracosAcertouPrimeira",
                      "totBuracosAcertouSegunda",
                      "totBuracosAcertouTerceira",
                      "totBuracosErrouPrimeira",
                      "totBuracosErrouSegunda",
                      "totBuracosErrouTerceira",
                      "predicoes100CorretasPrimeira",
                      "predicoes100CorretasSegunda",
                      "predicoes100CorretasTerceira",
                      "acertosgtbPrimeira",
                      "acertosgtbSegunda",
                      "acertosgtbTerceira",
                      "acertosmq1Primeira",
                      "acertosmq1Segunda",
                      "acertosmq1Terceira",
                      "acertoslsPrimeira",
                      "acertoslsSegunda",
                      "acertoslsTerceira",
                      "acertosmq2Primeira",
                      "acertosmq2Segunda",
                      "acertosmq2Terceira",
                      "acertoslasPrimeira",
                      "acertoslasSegunda",
                      "acertoslasTerceira",
                      "acertosoutFogPrimeira",
                      "acertosoutFogSegunda",
                      "acertosoutFogTerceira",
                      "acertosaa1Primeira",
                      "acertosaa1Segunda",
                      "acertosaa1Terceira",
                      "errosgtbPrimeira",
                      "errosgtbSegunda",
                      "errosgtbTerceira",
                      "errosmq1Primeira",
                      "errosmq1Segunda",
                      "errosmq1Terceira",
                      "erroslsPrimeira",
                      "erroslsSegunda",
                      "erroslsTerceira",
                      "errosmq2Primeira",
                      "errosmq2Segunda",
                      "errosmq2Terceira",
                      "erroslasPrimeira",
                      "erroslasSegunda",
                      "erroslasTerceira",
                      "errosoutFogPrimeira",
                      "errosoutFogSegunda",
                      "errosoutFogTerceira",
                      "errosaa1Primeira",
                      "errosaa1Segunda",
                      "errosaa1Terceira",
                      "porcAcertogtbPrimeira",
                      "porcAcertogtbSegunda",
                      "porcAcertogtbTerceira",
                      "porcAcertomq1Primeira",
                      "porcAcertomq1Segunda",
                      "porcAcertomq1Terceira",
                      "porcAcertolsPrimeira",
                      "porcAcertolsSegunda",
                      "porcAcertolsTerceira",
                      "porcAcertomq2Primeira",
                      "porcAcertomq2Segunda",
                      "porcAcertomq2Terceira",
                      "porcAcertolasPrimeira",
                      "porcAcertolasSegunda",
                      "porcAcertolasTerceira",
                      "porcAcertooutFogPrimeira",
                      "porcAcertooutFogSegunda",
                      "porcAcertooutFogTerceira",
                      "porcAcertoaa1Primeira",
                      "porcAcertoaa1Segunda",
                      "porcAcertoaa1Terceira")
    
    # carregando o dataFrame com as linhas
    for(i in 1:lista_experimento$bateria){
      for(j in 1:length(lista_experimento$registros)){
        for(k in 1:length(lista_experimento$furos)){
          df_resultado <-rbind(df_resultado,list(
            NomeExperimento,                # nomeExperimento
            i,                              # vezExecucao
            lista_experimento$registros[j], # qtdeRegistros
            lista_experimento$furos[k],     # porcBuracos
            -1, # totBuracos
            -1, # totBuracosAcertouPrimeira
            -1, # totBuracosAcertouSegunda
            -1, # totBuracosAcertouTerceira
            -1, # totBuracosErrouPrimeira
            -1, # totBuracosErrouSegunda
            -1, # totBuracosErrouTerceira
            -1, # predicoes100CorretasPrimeira
            -1, # predicoes100CorretasSegunda
            -1, # predicoes100CorretasTerceira
            -1, # acertosgtbPrimeira
            -1, # acertosgtbSegunda
            -1, # acertosgtbTerceira
            -1, # acertosmq1Primeira
            -1, # acertosmq1Segunda
            -1, # acertosmq1Terceira
            -1, # acertoslsPrimeira
            -1, # acertoslsSegunda
            -1, # acertoslsTerceira
            -1, # acertosmq2Primeira
            -1, # acertosmq2Segunda
            -1, # acertosmq2Terceira
            -1, # acertoslasPrimeira
            -1, # acertoslasSegunda
            -1, # acertoslasTerceira
            -1, # acertosoutFogPrimeira
            -1, # acertosoutFogSegunda
            -1, # acertosoutFogTerceira
            -1, # acertosaa1Primeira
            -1, # acertosaa1Segunda
            -1, # acertosaa1Terceira
            -1, # errosgtbPrimeira
            -1, # errosgtbSegunda
            -1, # errosgtbTerceira
            -1, # errosmq1Primeira
            -1, # errosmq1Segunda
            -1, # errosmq1Terceira
            -1, # erroslsPrimeira
            -1, # erroslsSegunda
            -1, # erroslsTerceira
            -1, # errosmq2Primeira
            -1, # errosmq2Segunda
            -1, # errosmq2Terceira
            -1, # erroslasPrimeira
            -1, # erroslasSegunda
            -1, # erroslasTerceira
            -1, # errosoutFogPrimeira
            -1, # errosoutFogSegunda
            -1, # errosoutFogTerceira
            -1, # errosaa1Primeira
            -1, # errosaa1Segunda
            -1, # errosaa1Terceira
            -1, # porcAcertogtbPrimeira
            -1, # porcAcertogtbSegunda
            -1, # porcAcertogtbTerceira
            -1, # porcAcertomq1Primeira
            -1, # porcAcertomq1Segunda
            -1, # porcAcertomq1Terceira
            -1, # porcAcertolsPrimeira
            -1, # porcAcertolsSegunda
            -1, # porcAcertolsTerceira
            -1, # porcAcertomq2Primeira
            -1, # porcAcertomq2Segunda
            -1, # porcAcertomq2Terceira
            -1, # porcAcertolasPrimeira
            -1, # porcAcertolasSegunda
            -1, # porcAcertolasTerceira
            -1, # porcAcertooutFogPrimeira
            -1, # porcAcertooutFogSegunda
            -1, # porcAcertooutFogTerceira
            -1, # porcAcertoaa1Primeira
            -1, # porcAcertoaa1Segunda
            -1  # porcAcertoaa1Terceira
          ))  
        } # for k
      } # for j
    } # for i
    
  } 
  
  if(lista_experimento$cenario=="5"){
    # gerando um dataframe para o resultado do processamento (77 variáveis)
    df_resultado <- data.frame(matrix(ncol = 77, nrow = 0))
    nomesColunas <- c("nomeExperimento",
                      "vezExecucao",
                      "qtdeRegistros",
                      "porcBuracos",
                      "totBuracos",
                      "totBuracosAcertouPrimeira",
                      "totBuracosAcertouSegunda",
                      "totBuracosAcertouTerceira",
                      "totBuracosErrouPrimeira",
                      "totBuracosErrouSegunda",
                      "totBuracosErrouTerceira",
                      "predicoes100CorretasPrimeira",
                      "predicoes100CorretasSegunda",
                      "predicoes100CorretasTerceira",
                      "acertosiotPrimeira",
                      "acertosiotSegunda",
                      "acertosiotTerceira",
                      "acertosmoPrimeira",
                      "acertosmoSegunda",
                      "acertosmoTerceira",
                      "acertosoriPrimeira",
                      "acertosoriSegunda",
                      "acertosoriTerceira",
                      "acertosqlPrimeira",
                      "acertosqlSegunda",
                      "acertosqlTerceira",
                      "acertoscdbPrimeira",
                      "acertoscdbSegunda",
                      "acertoscdbTerceira",
                      "acertosaa2Primeira",
                      "acertosaa2Segunda",
                      "acertosaa2Terceira",
                      "acertosoutCloudPrimeira",
                      "acertosoutCloudSegunda",
                      "acertosoutCloudTerceira",
                      "errosiotPrimeira",
                      "errosiotSegunda",
                      "errosiotTerceira",
                      "errosmoPrimeira",
                      "errosmoSegunda",
                      "errosmoTerceira",
                      "errosoriPrimeira",
                      "errosoriSegunda",
                      "errosoriTerceira",
                      "errosqlPrimeira",
                      "errosqlSegunda",
                      "errosqlTerceira",
                      "erroscdbPrimeira",
                      "erroscdbSegunda",
                      "erroscdbTerceira",
                      "errosaa2Primeira",
                      "errosaa2Segunda",
                      "errosaa2Terceira",
                      "errosoutCloudPrimeira",
                      "errosoutCloudSegunda",
                      "errosoutCloudTerceira",
                      "porcAcertoiotPrimeira",
                      "porcAcertoiotSegunda",
                      "porcAcertoiotTerceira",
                      "porcAcertomoPrimeira",
                      "porcAcertomoSegunda",
                      "porcAcertomoTerceira",
                      "porcAcertooriPrimeira",
                      "porcAcertooriSegunda",
                      "porcAcertooriTerceira",
                      "porcAcertoqlPrimeira",
                      "porcAcertoqlSegunda",
                      "porcAcertoqlTerceira",
                      "porcAcertocdbPrimeira",
                      "porcAcertocdbSegunda",
                      "porcAcertocdbTerceira",
                      "porcAcertoaa2Primeira",
                      "porcAcertoaa2Segunda",
                      "porcAcertoaa2Terceira",
                      "porcAcertooutCloudPrimeira",
                      "porcAcertooutCloudSegunda",
                      "porcAcertooutCloudTerceira")
    
    # carregando o dataFrame com as linhas
    for(i in 1:lista_experimento$bateria){
      for(j in 1:length(lista_experimento$registros)){
        for(k in 1:length(lista_experimento$furos)){
          df_resultado <-rbind(df_resultado,list(
            NomeExperimento,                # nomeExperimento
            i,                              # vezExecucao
            lista_experimento$registros[j], # qtdeRegistros
            lista_experimento$furos[k],     # porcBuracos
            -1, # totBuracos
            -1, # totBuracosAcertouPrimeira
            -1, # totBuracosAcertouSegunda
            -1, # totBuracosAcertouTerceira
            -1, # totBuracosErrouPrimeira
            -1, # totBuracosErrouSegunda
            -1, # totBuracosErrouTerceira
            -1, # predicoes100CorretasPrimeira
            -1, # predicoes100CorretasSegunda
            -1, # predicoes100CorretasTerceira
            -1, # acertosiotPrimeira
            -1, # acertosiotSegunda
            -1, # acertosiotTerceira
            -1, # acertosmoPrimeira
            -1, # acertosmoSegunda
            -1, # acertosmoTerceira
            -1, # acertosoriPrimeira
            -1, # acertosoriSegunda
            -1, # acertosoriTerceira
            -1, # acertosqlPrimeira
            -1, # acertosqlSegunda
            -1, # acertosqlTerceira
            -1, # acertoscdbPrimeira
            -1, # acertoscdbSegunda
            -1, # acertoscdbTerceira
            -1, # acertosaa2Primeira
            -1, # acertosaa2Segunda
            -1, # acertosaa2Terceira
            -1, # acertosoutCloudPrimeira
            -1, # acertosoutCloudSegunda
            -1, # acertosoutCloudTerceira
            -1, # errosiotPrimeira
            -1, # errosiotSegunda
            -1, # errosiotTerceira
            -1, # errosmoPrimeira
            -1, # errosmoSegunda
            -1, # errosmoTerceira
            -1, # errosoriPrimeira
            -1, # errosoriSegunda
            -1, # errosoriTerceira
            -1, # errosqlPrimeira
            -1, # errosqlSegunda
            -1, # errosqlTerceira
            -1, # erroscdbPrimeira
            -1, # erroscdbSegunda
            -1, # erroscdbTerceira
            -1, # errosaa2Primeira
            -1, # errosaa2Segunda
            -1, # errosaa2Terceira
            -1, # errosoutCloudPrimeira
            -1, # errosoutCloudSegunda
            -1, # errosoutCloudTerceira
            -1, # porcAcertoiotPrimeira
            -1, # porcAcertoiotSegunda
            -1, # porcAcertoiotTerceira
            -1, # porcAcertomoPrimeira
            -1, # porcAcertomoSegunda
            -1, # porcAcertomoTerceira
            -1, # porcAcertooriPrimeira
            -1, # porcAcertooriSegunda
            -1, # porcAcertooriTerceira
            -1, # porcAcertoqlPrimeira
            -1, # porcAcertoqlSegunda
            -1, # porcAcertoqlTerceira
            -1, # porcAcertocdbPrimeira
            -1, # porcAcertocdbSegunda
            -1, # porcAcertocdbTerceira
            -1, # porcAcertoaa2Primeira
            -1, # porcAcertoaa2Segunda
            -1, # porcAcertoaa2Terceira
            -1, # porcAcertooutCloudPrimeira
            -1, # porcAcertooutCloudSegunda
            -1  # porcAcertooutCloudTerceira
          ))  
        } # for k
      } # for j
    } # for i
    
  } 
  
  if(lista_experimento$cenario=="6"){
    # gerando um dataframe para o resultado do processamento (90 variáveis)
    print("gerando um dataframe para o resultado do processamento (90 variáveis)")
    df_resultado <- data.frame(matrix(ncol = 90, nrow = 0))
    nomesColunas <- c("nomeExperimento",
                      "vezExecucao",
                      "qtdeRegistros",
                      "porcBuracos",
                      "totBuracos",
                      "totBuracosAcertouPrimeira",
                      "totBuracosAcertouSegunda",
                      "totBuracosAcertouTerceira",
                      "totBuracosErrouPrimeira",
                      "totBuracosErrouSegunda",
                      "totBuracosErrouTerceira",
                      "predicoes100CorretasPrimeira",
                      "predicoes100CorretasSegunda",
                      "predicoes100CorretasTerceira",
                      "totBuracosSensores",
                      "acertosSensor1Primeira",
                      "acertosSensor1Segunda",
                      "acertosSensor1Terceira",
                      "acertosSensor2Primeira",
                      "acertosSensor2Segunda",
                      "acertosSensor2Terceira",
                      "errosSensor1Primeira",
                      "errosSensor1Segunda",
                      "errosSensor1Terceira",
                      "errosSensor2Primeira",
                      "errosSensor2Segunda",
                      "errosSensor2Terceira",
                      "porcAcertoSensorPrimeira",
                      "porcAcertoSensorSegunda",
                      "porcAcertoSensorTerceira",
                      "totBuracosAcao",
                      "acertosAcaoPrimeira",
                      "acertosAcaoSegunda",
                      "acertosAcaoTerceira",
                      "errosAcaoPrimeira",
                      "errosAcaoSegunda",
                      "errosAcaoTerceira",
                      "porcAcertoAcaoPrimeira",
                      "porcAcertoAcaoSegunda",
                      "porcAcertoAcaoTerceira",
                      "totBuracosSomafusao",
                      "acertosSomafusao1Primeira",
                      "acertosSomafusao1Segunda",
                      "acertosSomafusao1Terceira",
                      "errosSomafusao1Primeira",
                      "errosSomafusao1Segunda",
                      "errosSomafusao1Terceira",
                      "porcAcertoSomafusaoPrimeira",
                      "porcAcertoSomafusaoSegunda",
                      "porcAcertoSomafusaoTerceira",
                      "totBuracosAtuador",
                      "acertosAtuador1Primeira",
                      "acertosAtuador1Segunda",
                      "acertosAtuador1Terceira",
                      "acertosAtuador2Primeira",
                      "acertosAtuador2Segunda",
                      "acertosAtuador2Terceira",
                      "errosAtuador1Primeira",
                      "errosAtuador1Segunda",
                      "errosAtuador1Terceira",
                      "errosAtuador2Primeira",
                      "errosAtuador2Segunda",
                      "errosAtuador2Terceira",
                      "porcAcertosAtuadorPrimeira",
                      "porcAcertosAtuadorSegunda",
                      "porcAcertosAtuadorTerceira")
    
    # carregando o dataFrame com as linhas
    for(i in 1:lista_experimento$bateria){
      for(j in 1:length(lista_experimento$registros)){
        for(k in 1:length(lista_experimento$furos)){
          #cat("i=",i," j=",j," k=",k,"\n")
          df_resultado <-rbind(df_resultado,list(
            NomeExperimento,                # nomeExperimento
            i,                              # vezExecucao
            lista_experimento$registros[j], # qtdeRegistros
            lista_experimento$furos[k],     # porcBuracos
            -1, # totBuracos
            -1, # totBuracosAcertouPrimeira
            -1, # totBuracosAcertouSegunda
            -1, # totBuracosAcertouTerceira
            -1, # totBuracosErrouPrimeira
            -1, # totBuracosErrouSegunda
            -1, # totBuracosErrouTerceira
            -1, # predicoes100CorretasPrimeira
            -1, # predicoes100CorretasSegunda
            -1, # predicoes100CorretasTerceira
            -1, # totBuracosSensores
            -1, # acertosSensor1Primeira
            -1, # acertosSensor1Segunda
            -1, # acertosSensor1Terceira
            -1, # acertosSensor2Primeira
            -1, # acertosSensor2Segunda
            -1, # acertosSensor2Terceira
            -1, # errosSensor1Primeira
            -1, # errosSensor1Segunda
            -1, # errosSensor1Terceira
            -1, # errosSensor2Primeira
            -1, # errosSensor2Segunda
            -1, # errosSensor2Terceira
            -1, # porcAcertoSensorPrimeira
            -1, # porcAcertoSensorSegunda
            -1, # porcAcertoSensorTerceira
            -1, # totBuracosAcao
            -1, # acertosAcaoPrimeira
            -1, # acertosAcaoSegunda
            -1, # acertosAcaoTerceira
            -1, # errosAcaoPrimeira
            -1, # errosAcaoSegunda
            -1, # errosAcaoTerceira
            -1, # porcAcertoAcaoPrimeira
            -1, # porcAcertoAcaoSegunda
            -1, # porcAcertoAcaoTerceira
            -1, # totBuracosSomafusao
            -1, # acertosSomafusao1Primeira
            -1, # acertosSomafusao1Segunda
            -1, # acertosSomafusao1Terceira
            -1, # errosSomafusao1Primeira
            -1, # errosSomafusao1Segunda
            -1, # errosSomafusao1Terceira
            -1, # porcAcertoSomafusaoPrimeira
            -1, # porcAcertoSomafusaoSegunda
            -1, # porcAcertoSomafusaoTerceira
            -1, # totBuracosAtuador
            -1, # acertosAtuador1Primeira
            -1, # acertosAtuador1Segunda
            -1, # acertosAtuador1Terceira
            -1, # acertosAtuador2Primeira
            -1, # acertosAtuador2Segunda
            -1, # acertosAtuador2Terceira
            -1, # errosAtuador1Primeira
            -1, # errosAtuador1Segunda
            -1, # errosAtuador1Terceira
            -1, # errosAtuador2Primeira
            -1, # errosAtuador2Segunda
            -1, # errosAtuador2Terceira
            -1, # porcAcertosAtuadorPrimeira
            -1, # porcAcertosAtuadorSegunda
            -1  # porcAcertosAtuadorTerceira
          ))  
        } # for k
      } # for j
    } # for i    
    #cat("saiu do laco ijk\n")
  } 
  
  if(lista_experimento$cenario=="7"){
    # gerando um dataframe para o resultado do processamento (93 variáveis)
    df_resultado <- data.frame(matrix(ncol = 93, nrow = 0))
    nomesColunas <- c("nomeExperimento",
                      "vezExecucao",
                      "qtdeRegistros",
                      "porcBuracos",
                      "totBuracos",
                      "totBuracosAcertouPrimeira",
                      "totBuracosAcertouSegunda",
                      "totBuracosAcertouTerceira",
                      "totBuracosErrouPrimeira",
                      "totBuracosErrouSegunda",
                      "totBuracosErrouTerceira",
                      "predicoes100CorretasPrimeira",
                      "predicoes100CorretasSegunda",
                      "predicoes100CorretasTerceira",
                      "totBuracosSensores",
                      "acertosSensor1Primeira",
                      "acertosSensor1Segunda",
                      "acertosSensor1Terceira",
                      "acertosSensor2Primeira",
                      "acertosSensor2Segunda",
                      "acertosSensor2Terceira",
                      "acertosSensorCPrimeira",
                      "acertosSensorCSegunda",
                      "acertosSensorCTerceira",
                      "errosSensor1Primeira",
                      "errosSensor1Segunda",
                      "errosSensor1Terceira",
                      "errosSensor2Primeira",
                      "errosSensor2Segunda",
                      "errosSensor2Terceira",
                      "errosSensorCPrimeira",
                      "errosSensorCSegunda",
                      "errosSensorCTerceira",
                      "porcAcertoSensorPrimeira",
                      "porcAcertoSensorSegunda",
                      "porcAcertoSensorTerceira",
                      "totBuracosAcao",
                      "acertosAcaoPrimeira",
                      "acertosAcaoSegunda",
                      "acertosAcaoTerceira",
                      "errosAcaoPrimeira",
                      "errosAcaoSegunda",
                      "errosAcaoTerceira",
                      "porcAcertoAcaoPrimeira",
                      "porcAcertoAcaoSegunda",
                      "porcAcertoAcaoTerceira",
                      "totBuracosSomafusao",
                      "acertosSomafusao1Primeira",
                      "acertosSomafusao1Segunda",
                      "acertosSomafusao1Terceira",
                      "errosSomafusao1Primeira",
                      "errosSomafusao1Segunda",
                      "errosSomafusao1Terceira",
                      "porcAcertoSomafusao1Primeira",
                      "porcAcertoSomafusao1Segunda",
                      "porcAcertoSomafusao1Terceira",
                      "totBuracosAtuador",
                      "acertosAtuador1Primeira",
                      "acertosAtuador1Segunda",
                      "acertosAtuador1Terceira",
                      "errosAtuador1Primeira",
                      "errosAtuador1Segunda",
                      "errosAtuador1Terceira",
                      "porcAcertosAtuadorPrimeira",
                      "porcAcertosAtuadorSegunda",
                      "porcAcertosAtuadorTerceira")
    
    # carregando o dataFrame com as linhas
    for(i in 1:lista_experimento$bateria){
      for(j in 1:length(lista_experimento$registros)){
        for(k in 1:length(lista_experimento$furos)){
          df_resultado <-rbind(df_resultado,list(
            NomeExperimento,                # nomeExperimento
            i,                              # vezExecucao
            lista_experimento$registros[j], # qtdeRegistros
            lista_experimento$furos[k],     # porcBuracos
            -1, # totBuracos
            -1, # totBuracosAcertouPrimeira
            -1, # totBuracosAcertouSegunda
            -1, # totBuracosAcertouTerceira
            -1, # totBuracosErrouPrimeira
            -1, # totBuracosErrouSegunda
            -1, # totBuracosErrouTerceira
            -1, # predicoes100CorretasPrimeira
            -1, # predicoes100CorretasSegunda
            -1, # predicoes100CorretasTerceira
            -1, # totBuracosSensores
            -1, # acertosSensor1Primeira
            -1, # acertosSensor1Segunda
            -1, # acertosSensor1Terceira
            -1, # acertosSensor2Primeira
            -1, # acertosSensor2Segunda
            -1, # acertosSensor2Terceira
            -1, # acertosSensorCPrimeira
            -1, # acertosSensorCSegunda
            -1, # acertosSensorCTerceira
            -1, # errosSensor1Primeira
            -1, # errosSensor1Segunda
            -1, # errosSensor1Terceira
            -1, # errosSensor2Primeira
            -1, # errosSensor2Segunda
            -1, # errosSensor2Terceira
            -1, # errosSensorCPrimeira
            -1, # errosSensorCSegunda
            -1, # errosSensorCTerceira
            -1, # porcAcertoSensorPrimeira
            -1, # porcAcertoSensorSegunda
            -1, # porcAcertoSensorTerceira
            -1, # totBuracosAcao
            -1, # acertosAcaoPrimeira
            -1, # acertosAcaoSegunda
            -1, # acertosAcaoTerceira
            -1, # errosAcaoPrimeira
            -1, # errosAcaoSegunda
            -1, # errosAcaoTerceira
            -1, # porcAcertoAcaoPrimeira
            -1, # porcAcertoAcaoSegunda
            -1, # porcAcertoAcaoTerceira
            -1, # totBuracosSomafusao
            -1, # acertosSomafusao1Primeira
            -1, # acertosSomafusao1Segunda
            -1, # acertosSomafusao1Terceira
            -1, # errosSomafusao1Primeira
            -1, # errosSomafusao1Segunda
            -1, # errosSomafusao1Terceira
            -1, # porcAcertoSomafusao1Primeira
            -1, # porcAcertoSomafusao1Segunda
            -1, # porcAcertoSomafusao1Terceira
            -1, # totBuracosAtuador
            -1, # acertosAtuador1Primeira
            -1, # acertosAtuador1Segunda
            -1, # acertosAtuador1Terceira
            -1, # errosAtuador1Primeira
            -1, # errosAtuador1Segunda
            -1, # errosAtuador1Terceira
            -1, # porcAcertosAtuadorPrimeira
            -1, # porcAcertosAtuadorSegunda
            -1  # porcAcertosAtuadorTerceira
          ))  
        } # for k
      } # for j
    } # for i
    
  } 
  

  colnames(df_resultado) <- nomesColunas  
  # gravação do arquivo de resultado do processamento
  write_json(df_resultado, ArquivoResultado)

  
} #fecha else do if(file.exists(ArquivoResultado))

# criação dos arquivos de entrada
gerarDadosCenario()

# processamento de uma bateria de testes para cada arquivo e 
# para cada percentual de buracos 
processaDadosCenario()

# atualizando as variáveis (elas estavam com os valores originais neste ponto do programa)
lista_controle <- fromJSON(ArquivoControle)
df_resultado <- as.data.frame(fromJSON(ArquivoResultado))
ArquivoResultadoCSV <- paste(NomeExperimento,"_resultado",".csv",sep="",collapse="")
write.csv(df_resultado,ArquivoResultadoCSV)

# Calcular estatísticas do processamento
calcularEstatisticas()

# Término do Processamento
terminoProcessamento = proc.time()
duracaoProcessamento = terminoProcessamento - inicioProcessamento

cat("======================================================\n")
cat("               DURAÇÃO DO PROCESSAMENTO               \n")
cat("======================================================\n")
print (duracaoProcessamento)
cat("======================================================\n")
##################################################################################
#
# Termino do MAIN PROGRAM
#
##################################################################################