#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#Importação das bibliotescas
library(shiny)
library(readr)
library(plyr)
library(chron)#transformar caracter em hora
library(tidyverse)
library(reshape)
library(forcats)#ordenar barras no ggplot
library(gridExtra)#arrumar multiplos gráficos em uma figura
library(knitr)
library(kableExtra)#Fazer tabela do summary
library(pander)#Fazer tabela do chi.test
library(caret)#K-fold cross validation
library(caTools)#Calcular AUC
library(ROSE) #dealing with imbalanced data
library(plotly) #gráficos interativos
library(doParallel) #Parallel Processing
library(mice) #Input de valores para substituir NA
library(tibble) #tabelas
library(fastAdaboost) #AdaBoost Classification Trees
library(gbm) #Stochastic Gradient Boosting
library(binda) #modelo Binary Discriminant Analysis
library(nnet) #Bayesian Regularized Neural Networks
library(xgboost) #eXtreme Gradient Boosting
library(deepnet) #Stacked AutoEncoder Deep Neural Network
library(MASS) #Generalized Linear Model with Stepwise Feature Selection
library(LiblineaR) #SVM
library(ggplot2) #plots
library(corrgram) #correlograma
library(data.table) #otimização do uso de dataframes
library(shinyjs) #js para o shiny


processamento <- function(treino, teste){
  #inputs dos bancos de dados de treino e teste
  treino <- sinasc
  teste <- mort_infantil_18
  
  #lista com os nomes da variáveis para selecionar do banco 
  lista_variaveis <- c("GESTACAO_SINASC", "PARTO_SINASC", "APGAR1_SINASC", "APGAR5_SINASC", "PESO_SINASC", "IDANOMAL_SINASC", "QTDPARTNOR_SINASC", "IDADEMAE_SINASC", "GRAVIDEZ_SINASC", "CONSULTAS_SINASC", "ESCMAE2010_SINASC", "OBITO")
  
  
  #selecionando as variáveis pela lista do banco treino de treino
  treino <- treino[, which(names(treino) %in% lista_variaveis)]
  
  #usando o pacote mice para completar os dados NA do banco
  tempData <- mice(treino,m=5,maxit=50,meth="cart",seed=500)
  treino <- complete(tempData,1)
  
  #transformando a variável OBITO em "sim/nao" e em fator
  treino$OBITO <- as.character(treino$OBITO)
  treino$OBITO <- ifelse(treino$OBITO=="0", treino$OBITO <-"nao", treino$OBITO<-"sim")
  treino$OBITO <- as.factor(treino$OBITO)
  
  #transformando outras variáveis em fator, as variáveis PESO e QTDPARTNOR continuam como numéricas
  treino$GESTACAO_SINASC <- as.factor(treino$GESTACAO_SINASC)
  treino$PARTO_SINASC <- as.factor(treino$PARTO_SINASC)
  treino$APGAR1_SINASC <- as.factor(treino$APGAR1_SINASC)
  treino$APGAR5_SINASC <- as.factor(treino$APGAR5_SINASC)
  treino$IDANOMAL_SINASC <- as.factor(treino$IDANOMAL_SINASC)
  treino$ESCMAE2010_SINASC <- as.factor(treino$ESCMAE2010_SINASC)
  treino$GRAVIDEZ_SINASC <- as.factor(treino$GRAVIDEZ_SINASC)
  treino$CONSULTAS_SINASC <- as.factor(treino$CONSULTAS_SINASC)
  
  #selecionando as variáveis do banco de teste pelo banco de treino
  teste <- teste[, which(names(teste) %in% lista_variaveis)]
  
  
  #usando o pacote mice para completar os dados NA do banco de teste
  tempData2 <- mice(teste,m=5,maxit=50,meth="cart",seed=500)
  teste <- complete(tempData2,1)
  
  #existem alguns dados que foram preenchidos fora do padrão, e são transformados para NA
  for(i in 1:nrow(teste)){
    if(teste$APGAR5_SINASC[i] > 10){teste$APGAR5_SINASC[i] <- NA}
  }
  
  for(i in 1:nrow(teste)){
    if(teste$GESTACAO_SINASC[i] > 9){teste$GESTACAO_SINASC[i] <- NA}
  }
  
  for(i in 1:nrow(teste)){
    if(teste$PARTO_SINASC[i] != 1 & teste$PARTO_SINASC[i] != 2 & teste$PARTO_SINASC[i] != 9){teste$PARTO_SINASC[i] <- NA}
  }
  
  for(i in 1:nrow(teste)){
    if(teste$APGAR1_SINASC[i] > 10){teste$APGAR1_SINASC[i] <- NA}
  }
  
  for(i in 1:nrow(teste)){
    if(teste$IDANOMAL_SINASC[i] != 1 & teste$IDANOMAL_SINASC[i] != 2 & teste$IDANOMAL_SINASC[i] != 9){teste$IDANOMAL_SINASC[i] <- NA}
  }
  
  for(i in 1:nrow(teste)){
    if(teste$ESCMAE2010_SINASC[i] > 9){teste$ESCMAE2010_SINASC[i] <- NA}
  }
  
  for(i in 1:nrow(teste)){
    if(teste$GRAVIDEZ_SINASC[i] > 3){teste$GRAVIDEZ_SINASC[i] <- NA}
  }
  
  for(i in 1:nrow(teste)){
    if(teste$CONSULTAS_SINASC[i] > 9){teste$CONSULTAS_SINASC[i] <- NA}
  }
  
  #Usando o pacote mice novamente para completar os dados
  tempData3 <- mice(teste,m=5,maxit=50,meth="cart",seed=500)
  teste <- complete(tempData3,1)
  
  
  #transformando outras variáveis em fator, as variáveis PESO e QTDPARTNOR continuam como numéricas
  teste$OBITO <- as.character(teste$OBITO)
  teste$OBITO <- ifelse(teste$OBITO=="0", teste$OBITO <-"nao", teste$OBITO<-"sim")
  teste$OBITO <- as.factor(teste$OBITO)
  
  teste$GESTACAO_SINASC <- as.factor(teste$GESTACAO_SINASC)
  teste$PARTO_SINASC <- as.factor(teste$PARTO_SINASC)
  teste$APGAR1_SINASC <- as.factor(teste$APGAR1_SINASC)
  teste$APGAR5_SINASC <- as.factor(teste$APGAR5_SINASC)
  teste$IDANOMAL_SINASC <- as.factor(teste$IDANOMAL_SINASC)
  teste$ESCMAE2010_SINASC <- as.factor(teste$ESCMAE2010_SINASC)
  teste$GRAVIDEZ_SINASC <- as.factor(teste$GRAVIDEZ_SINASC)
  teste$CONSULTAS_SINASC <- as.factor(teste$CONSULTAS_SINASC)
  
  
  levels(treino$OBITO) <- list(sim = "sim", nao = "nao")
  levels(teste$OBITO) <- list(sim = "sim", nao = "nao")
  
  return(list(treino, teste))
  
}

pred_mort <- function(bancoTreino, bancoPredicao){
  
  set.seed(2)
  
  # dados para treino
  train_data <- bancoTreino
  
  # dados para controle do teste
  test_ctrl <- bancoPredicao
  
  # dados para o teste
  test <- test_ctrl[, -which(names(test_ctrl) %in% c("OBITO"))]
  
  #lista com os métodos de balanceamento
  sampling_methods <- c("down", "up", "smote")
  j <- 1
  sm_index <- 1
  
  for(j in length(sampling_methods)){
    
    sm_index <- sm_index + 1
    j <- j + 1
    
    #Create train/test index
    # Create trainControl object: myControl - Deve ser utilizado em todos os modelos para que sejam comparáveis
    myControl <- trainControl(
      method = "cv", #"repeatedcv" é o método para realizar as repetições
      number = 2, #number é o número de folds
      #repeats = 1, #repeats é o número de repetições para cada fold
      summaryFunction = twoClassSummary,
      classProbs = TRUE, # IMPORTANT!
      verboseIter = FALSE,
      savePredictions = TRUE,
      returnResamp = "all",
      sampling = sampling_methods[sm_index], #balanceamento dos dados
      allowParallel = TRUE
    )
    
    #lista de modelos que serão usados inicialmente 
    # "glm" = Generalized Linear Model, "ranger" = Random Forest, "knn" = k-Nearest Neighbors, 
    #"nnet" = Neural Network, "dnn" = Stacked AutoEncoder Deep Neural Network, 
    #"xgbTree" = eXtreme Gradient Boosting, "gbm" = Stochastic Gradient Boosting, "adaboost" = AdaBoost Classification Trees.
    
    # "glm", "ranger", "knn", "gbm", "nnet", "adaboost", "xgbTree", "dnn"
    modelos <- c("glm")
    
    i <- 1 #indice para atualizar o while
    index <- 1 #indice que retorna o modelo da lista
    maior_valor <- 0 #usado para verificar qual o modelo com maior valor preditivo negativo.
    #espec <- 0 #usado para verificar qual o modelo com maior especificidade.
    
    #lista com os métodos de balanceamento
    metrics <- c("ROC", "Sens")
    k <- 1
    m_index <- 1
    
    for(k in length(metrics)){
      
      m_index <- m_index + 1
      k <- k + 1
      
      #loop para selecionar o melhor algoritmo
      while(i <= length((modelos))) {
        
        # Fit model
        model <- train(
          OBITO ~ . , #variável preditiva
          data = train_data, #banco de treino
          preProcess = c("center", "scale"),
          metric = metrics[m_index], # métrica para comparação dos modelos
          method = modelos[index], #lista com indice (retorna uma string com o nome do método para cada modelo)
          trControl = myControl #aplica o controle
          
        )
        
        #fazendo a matriz de confusão para o banco de treino    
        banco_model <- model$trainingData
        
        banco_model$.outcome <- as.factor(banco_model$.outcome)
        
        cm_t <- confusionMatrix(banco_model$.outcome, sample(banco_model$.outcome))
        
        
        # Print model to console
        model
        
        # Print maximum ROC statistic
        max(model[["results"]][["ROC"]])
        
        #predição dos modelos no banco para matriz de confusão
        predictionsCM <- predict(model, test)
        
        #predição dos modelos no banco para probabilidade
        predictions <- predict(model, test, type = "prob")
        
        #o test_control é usado para comparação com os valores da predição, gerando a matriz de confusão.
        cm <- confusionMatrix(predictionsCM, test_ctrl$OBITO)
        
        #extraindo os resultados da matriz de confusão
        cm_results <- cm$byClass %>% as.list()
        
        #extraindo a sensibilidade
        sens <- cm_results[1] %>% as.data.frame()
        sens <- sens$Sensitivity
        
        
        #verificação do maior valor preditivo negativo, como inicialmente o maior valor está atribuído como 0, o primeiro modelo sempre terá o maior valor, ou seja, sempre que um modelo conseguir alcançar um valor preditivo negativo maior que o armazenado na memória, este passa a ser o instrumento de verificação.
        if(sens > maior_valor){
          maior_valor <- sens #valor preditivo positivo passa a ser o maior valor
          resultado <- paste("O melhor modelo foi: ", modelos[index], ", usando o método de balanceamento: ", sampling_methods[sm_index], "com a métrica: ", metrics[m_index], ", com sensibilidade de: ", sens) #mensagem para informar o modelo com melhor resultado
          cm_melhor <- cm #cm armazena os dados da matriz de confusão (teste) do melhor modelo
          cm_t_melhor <- cm_t #cm_t armazena os dados da matriz de confusão (treino) do melhor modelo
          melhor_modelo <- model
          
          #colando a coluna da predição para comparar com a real
          resultados <- cbind(test_ctrl, predictions)
          
          #cria uma coluna com a probabilidade em % de OBITO
          resultados["Prob"] <- resultados$sim * 100
          
          
        }
        else{
          maior_valor <- maior_valor #caso a verificação falhe, o maior_valor continua sendo ele mesmo ("atual")
        }
        
        #atualiza o indice para o i (while), e index (lista de modelos)
        i <- i + 1
        index <- index + 1
        
      }
    }
  }
  #desenha a matriz de confusão para o cm armazenado com o melhor modelo
  # cm_p <- draw_confusion_matrix(cm_melhor)  
  
  #retorno da função (matriz de confusão de treino (cm_t), mensagem de resultado (resultado), desenho da matriz de confusão de teste (cm_p))
  return(melhor_modelo)
  
}


#bancos <- processamento(sinasc, mort_infantil_18)

#treino <- bancos[1]
#teste <- bancos[2]


modelo <- pred_mort(sinasc,mort_infantil_18)


ui <- bootstrapPage(theme = "bootstrap.css",
  headerPanel(div(class="header", h2(id="titulo", 'Predição de Mortalidade Infantil'), align="center")),
  sidebarPanel(class="sidePanel", width = 20,
               #input de texto para a unidade
               div(class="inputs1", align="center",
                   #div(style="display: inline-block;vertical-align:top; width: 200px;", textInput("unidade", label = h6("Unidade"), value = NA)),
                   #input numérico para a idade
                   div(id="idade", style="display: inline-block;vertical-align:top; width: 250px;", numericInput("idade", label = h6(id="h6", "Idade da Mãe"), value = NA)),
                   #input caixa de seleção para evitar erros no preenchimento dos dados
                   div(id="gestacao", style="display: inline-block;vertical-align:top; width: 250px;", selectInput("gestacao", label = h6(id="h6", "Semanas de gestação"), 
                   choices = list("Menos de 22 semanas" = 1, "22 a 27 semanas" = 2, "28 a 31 semanas" = 3, "32 a 36 semanas" = 4, 
                   "37 a 41 semanas" = 5, "42 semanas e mais" = 6, "Ignorado" = 9), 
                   selected = 1)),
                   div(id="gravidez", style="display: inline-block;vertical-align:top; width: 250px;", selectInput("gravidez", label = h6(id="h6", "Tipo de gravidez"), 
                   choices = list("Única" = 1, "Dupla" = 2, "Tripla ou mais" = 3, "Ignorado" = 9),
                   selected = 1))),
               
               tags$br(),
               
               div(class="inputs2", align="center",
                   div(id="parto", style="display: inline-block;vertical-align:top; width: 250px;", selectInput("parto", label = h6(id="h6", "Tipo de parto"), 
                   choices = list("Vaginal" = 1, "Cesário" = 2, "Ignorado" = 9),
                   selected = 1)),
                   
                   div(id="consultas", style="display: inline-block;vertical-align:top; width: 250px;", selectInput("consultas", label = h6(id="h6", "Número de consultas de pré‐natal"), 
                   choices = list("Nenhuma consulta" = 1, "de 1 a 3 consultas" = 2, "de 4 a 6 consultas" = 3, "7 e mais consultas" = 4, "Ignorado." = 9),
                   selected = 1)),
                   div(id="apgar1", style="display: inline-block;vertical-align:top; width: 250px;", numericInput("apgar1", label = h6(id="h6", "APGAR 1º minuto"), value = 1)),
                   div(id="apgar5", style="display: inline-block;vertical-align:top; width: 250px;", numericInput("apgar5", label = h6(id="h6", "APGAR 5º minuto"), value = 1))),
               
               tags$br(),
               
               div(class="inputs3", align="center",
                   div(id="peso", style="display: inline-block;vertical-align:top; width: 250px;", numericInput("peso", label = h6(id="h6", "Peso ao nascer em gramas."), value = 1)),
                   div(id="anomalia", style="display: inline-block;vertical-align:top; width: 250px;;", selectInput("anomalia", label = h6(id="h6", "Anomalia identificada"), 
                   choices = list("Sim" = 1, "Não" = 2, "Ignorado" = 9),
                   selected = 1)),
                   div(id="esc", style="display: inline-block;vertical-align:top; width: 250px;", selectInput("escolaridade", label = h6(id="h6", "Escolaridade 2010"), 
                   choices = list("Sem escolaridade" = 0, "Fundamental I (1ª a 4ª série)" = 1, "Fundamental II (5ª a 8ª série)" = 2, 
                   "Médio (antigo 2º Grau)" = 3, "Superior incompleto" = 4, "Superior completo" = 5, "Ignorado" = 9), 
                   selected = 0)),
                   div(id="numPartos", style="display: inline-block;vertical-align:top; width: 250px;", numericInput("numPartos", label = h6(id="h6", "Número de partos vaginais"), value = 1))),
               
               tags$br(),
               
               div(class="divBotao", align="center", actionButton("pred", "Predição", 
                          style="color: #fff; background-color: #337ab7; border-color: #2e6da4; width: 200px; font-size: 20px;"))
               
  ),
  
  mainPanel(width = 20,
            div(style="margin:50px;",align="center", h2(textOutput("probs")))
  )
  
)



server <- function(input, output) {
  
  observeEvent( input$pred, {
    
    #unidade <- as.character(input$unidade)
    idade <- as.numeric(input$idade)
    gestacao <- as.factor(input$gestacao)
    gravidez <- as.factor(input$gravidez)
    parto <- as.factor(input$parto)
    consultas <- as.factor(input$consultas)
    apgar1 <- as.factor(input$apgar1)
    apgar5 <- as.factor(input$apgar5)
    peso <- as.numeric(input$peso)
    anomalia <- as.factor(input$anomalia)
    escolaridade <- as.factor(input$escolaridade)
    numPartos <- as.numeric(input$numPartos)
    
    teste_obito <- cbind.data.frame(idade, gestacao, gravidez, parto, consultas, apgar1, apgar5, peso, anomalia, escolaridade, numPartos)
    names(teste_obito) <- c("IDADEMAE_SINASC", "GESTACAO_SINASC", "GRAVIDEZ_SINASC", "PARTO_SINASC", "CONSULTAS_SINASC", "APGAR1_SINASC",
                            "APGAR5_SINASC", "PESO_SINASC", "IDANOMAL_SINASC", "ESCMAE2010_SINASC", "QTDPARTNOR_SINASC")
    
    output$probs <- renderText({
      pred <- function(model, teste){
        
        model <- model
        test <- teste
        
        #predição dos modelos no banco para probabilidade
        predictions <- predict(model, test, type = "prob")
        
        #colando a coluna da predição para comparar com a real
        resultados <- cbind(test, predictions)
        
        #cria uma coluna com a probabilidade em % de OBITO
        resultados["Prob"] <- resultados$sim * 100  
        
        prob <- resultados$Prob
        resultados$Prob <- as.numeric(resultados$Prob)
        
        resultados["Probs"] <- NA
        
        for(i in 1:nrow(resultados)){
          if(resultados$Prob[i] <= 100 & resultados$Prob[i] >= 50){resultados$Probs <- "alta"
          }
          else if(resultados$Prob[i] < 50 & resultados$Prob[i] >= 30){resultados$Probs <- "média"
          }
          else{resultados$Probs <- "baixa"
          }
        }
        
        prob <- resultados$Probs
        probNum <- resultados$Prob
        
        
        resultado <- paste("Criança com ", prob, " probabilidade de óbito.", " Probabilidade num: ", probNum)
        
        return(resultado)
        
      }

      probs <- pred(modelo, teste_obito)
      
    })
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
