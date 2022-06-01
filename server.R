library("shiny")
library("shinydashboard")
library("shinyWidgets")
library("waiter")
library("shinyalert")
library("rChoiceDialogs")


shinyServer(function(session,input, output) {
    
    data <- reactive({
      file1 <- input$lwrfile
      if(is.null(file1)) {return()}
      read.table(file = file1$datapath,
                 sep = ';',
                 header = T,
                 stringsAsFactors = T)
    })
    
    output$fileoblwr <- renderPrint({
      if(is.null(input$lwrfile)) {return()}
      str(data())
    })
    
    output$summlwr <- renderPrint({
      if(is.null(input$lwrfile)) {return()}
      summary(data())
    })
    
    output$tableui <- renderUI({
      dataout <- data()
      output$dataout<-renderDataTable(dataout)
      dataTableOutput("dataout")
    })
    
    output$lwrtb <- renderUI({
      if(is.null(input$lwrfile)) {return()}
      tabsetPanel(
        tabPanel("Data",
                 uiOutput("tableui"))
      )
    })
    
    output$summary <- renderUI({
      if(is.null(input$lwrfile)) {return()}
      tabsetPanel(
        tabPanel("Resumo",
                 verbatimTextOutput("summlwr"))
      )
    })
    
    MargemVertical <- reactive({
      as.numeric(input$MargemVert)
    })
    
    MargemHorizontal <- reactive({
      as.numeric(input$MargemHoriz)
    })
    
    Espacamento <- reactive({
      as.numeric(input$Espacamento)
    })
    FontSize <- reactive({
      as.numeric(input$FontSize)
    })
    FontSizeAluno <- reactive({
      as.numeric(input$FontSizeAluno)
    })
    itera <- reactive({
      as.numeric(input$itera)
    })
    
    output$plotlwr <- renderPlot({
      dados <- data()
      Largura <- 297
      Altura <- 210
      i<-itera()
      
      plot(0:Largura, 0:Largura, ty='n',
           axes = F, frame.plot = F,
           xlab="", ylab = "",
           xlim = c(0, Largura),
           ylim = c(0, Altura))
      
      Titulo1 <- 'O Diretor Geral da Faculdade Católica do Rio Grande do Norte, no uso das suas atribuiões,'
      
      Titulo2 <- paste(' tendo em vista a conclusão do Curso de ', dados$Curso[i], ' em ', dados$Data_de_Conclusao[i], ', confere o título de ', ifelse(dados$Sexo[i]=='Masculino','Bacharel', 'Bacharela'), ' em ', dados$Curso[i], sep = '')
      
      Y <- Altura - MargemVertical()-Espacamento()
      text(Largura/2, Y, labels = Titulo1, cex = FontSize())
      Y <- Y - (2*Espacamento())
      text(Largura/2, Y, labels = Titulo2, cex = FontSize())
      
      Aluno <- toupper(dados$Aluno[i])
      Y<- Y - (5 * Espacamento())
      text(Largura/2, Y, labels = Aluno, 
           col = 'red', cex = FontSizeAluno())
      
      Nacionalidade <- paste('Nacionalidade Brasileira, natural de ', dados$Cidade_Natal[i], '-', dados$Estado_Natal[i], ', nascida em ', dados$Data_de_Nascimento[i], ', e ', sep = '')
      
      Nacionalidade2 <- paste('outorga-lhe o presente diploma, para que possa gozar de todos os direitos e prerrogativas legais.', sep="")
      
      Y<- Y - (5 * Espacamento())
      text(Largura/2, Y, labels = Nacionalidade, cex = FontSize())
      Y<- Y - (2 * Espacamento())
      text(Largura/2, Y, labels = Nacionalidade2, cex = FontSize())
      
      Mossoro <- paste('Mossoró (RN),', dados$Data_da_emissao_do_diploma[i])
      
      Y<- Y - (3 * Espacamento())
      text(Largura/2, Y, labels = Mossoro, font = 2, cex = FontSize())
      
      Y<- Y - (5 * Espacamento())
      lines(c(MargemHorizontal(), 100), c(Y, Y))
      lines(c(Largura-MargemHorizontal() -100,
              Largura-MargemHorizontal()), c(Y, Y))
      
      Secretaria <- dados$Secretaria[i]
      Diretor <- dados$Diretor[i]
      TituloSecretaria <- 'Secretária Acadêmica'
      TituloDiretor <- 'Diretor Geral'
      Diplomado <- 'Diplomado (a)'
      DiplomadoRG <- paste('RG:', dados$RG[i],
                           ' - ', dados$Orgao_Emissor[i], '/',
                           dados$Estado_de_Emissao[i], sep="")
      
      Y <- Y - (1.5 * Espacamento())
      text((75/2)+25, Y, Secretaria, cex = FontSize())
      text((Largura - (75/2))-25, Y, Diretor, cex = FontSize())
      
      Y <- Y - (1.5 * Espacamento())
      text((75/2)+25, Y, TituloSecretaria, cex = FontSize())
      text((Largura - (75/2))-25, Y, TituloDiretor, cex = FontSize())
      
      Y <- Y - (3.5 * Espacamento())
      lines(c(Largura/2-50, Largura/2+50), c(Y, Y))
      Y <- Y - (2 * Espacamento())
      text(Largura/2, Y, Diplomado, cex = FontSize())
      Y <- Y - (2 * Espacamento())
      text(Largura/2, Y, DiplomadoRG, cex = FontSize())
    })
    
    
  output$downLWRplot <- downloadHandler(
    filename = 'Certificados.zip',
    content = function(file){
      dados <- data()
      certificados <- seq()
      Largura <- 297
      Altura <- 210
      owd <- setwd( tempdir())
      on.exit( setwd( owd))
      
      for(i in 1:nrow(dados)){
        certificados[i] <- paste('', dados$Aluno[i],'.jpeg', sep='')
        jpeg(certificados[i],
             width = 3508, height = 2480, res = 300)
          plot(0:Largura, 0:Largura, ty='n',
               axes = F, frame.plot = F,
               xlab="", ylab = "",
               xlim = c(0, Largura),
               ylim = c(0, Altura))
          
          Titulo1 <- 'O Diretor Geral da Faculdade Católica do Rio Grande do Norte, no uso das suas atribuiões,'
          
          Titulo2 <- paste(' tendo em vista a conclusão do Curso de ', dados$Curso[i], ' em ', 
                           dados$Data_de_Conclusao[i], ', confere o título de ', ifelse(dados$Sexo[i]=='Masculino','Bacharel', 'Bacharela'), 
                           ' em ', dados$Curso[i], sep = '')
          
          Y <- Altura - MargemVertical()-Espacamento()
          text(Largura/2, Y, labels = Titulo1, cex = FontSize())
          Y <- Y - (2*Espacamento())
          text(Largura/2, Y, labels = Titulo2, cex = FontSize())
          
          Aluno <- toupper(dados$Aluno[i])
          Y<- Y - (5 * Espacamento())
          text(Largura/2, Y, labels = Aluno, 
               col = 'red', cex = FontSizeAluno())
          
          Nacionalidade <- paste('Nacionalidade Brasileira, natural de ', dados$Cidade_Natal[i], '-', dados$Estado_Natal[i], ', nascida em ', dados$Data_de_Nascimento[i], ', e ', sep = '')
          
          Nacionalidade2 <- paste('outorga-lhe o presente diploma, para que possa gozar de todos os direitos e prerrogativas legais.', sep="")
          
          Y<- Y - (5 * Espacamento())
          text(Largura/2, Y, labels = Nacionalidade, cex = FontSize())
          Y<- Y - (2 * Espacamento())
          text(Largura/2, Y, labels = Nacionalidade2, cex = FontSize())
          
          Mossoro <- paste('Mossoró (RN),', dados$Data_da_emissao_do_diploma[i])
          
          Y<- Y - (3 * Espacamento())
          text(Largura/2, Y, labels = Mossoro, font = 2, cex = FontSize())
          
          Y<- Y - (5 * Espacamento())
          lines(c(MargemHorizontal(), 100), c(Y, Y))
          lines(c(Largura-MargemHorizontal() -100,
                  Largura-MargemHorizontal()), c(Y, Y))
          
          Secretaria <- dados$Secretaria[i]
          Diretor <- dados$Diretor[i]
          TituloSecretaria <- 'Secretária Acadêmica'
          TituloDiretor <- 'Diretor Geral'
          Diplomado <- 'Diplomado (a)'
          DiplomadoRG <- paste('RG:', dados$RG[i],
                               ' - ', dados$Orgao_Emissor[i], '/',
                               dados$Estado_de_Emissao[i], sep="")
           
          Y <- Y - (1.5 * Espacamento())
          text((75/2)+25, Y, Secretaria, cex = FontSize())
          text((Largura - (75/2))-25, Y, Diretor, cex = FontSize())
          
          Y <- Y - (1.5 * Espacamento())
          text((75/2)+25, Y, TituloSecretaria, cex = FontSize())
          text((Largura - (75/2))-25, Y, TituloDiretor, cex = FontSize())
          
          Y <- Y - (3.5 * Espacamento())
          lines(c(Largura/2-50, Largura/2+50), c(Y, Y))
          Y <- Y - (2 * Espacamento())
          text(Largura/2, Y, Diplomado, cex = FontSize())
          Y <- Y - (2 * Espacamento())
          text(Largura/2, Y, DiplomadoRG, cex = FontSize())
        dev.off()
      }
      zip( file, certificados)
    }
  )  
})

