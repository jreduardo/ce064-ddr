##-------------------------------------------
## server.R

library(shiny)

shinyServer(
    function(input, output, session) {
        
        ## ##-------------------------------------------
        ## ## Para testes
        ## output$teste <- renderPrint({
        ##     ## summary(ajuste())
        ##     input$grandTab
        ## })
        ## ##-------------------------------------------

        ## Realiza leitura dos dados
        dados <- reactive({
            path <- input$file$datapath
            dados <- read.table(
                file = path,
                header = input$header,
                sep = input$separator,
                dec = input$decimal,
                quote = input$quote
            )
            return(dados)
        })

        ## Exibe os dados na aba `Dados`, para facilitar escolha dos
        ## parametros da read.table
        output$viewData <- renderPrint({
            if (is.null(input$file)) {
                return(NULL)
            } else {
                cat("======================================================================",
                    "Estrutura dos dados",
                    sep = "\n")
                str(dados())
                cat("\n======================================================================",
                    "Visualização das 10 primeiras linhas",
                    sep = "\n")
                head(dados(), n = 10)
            }
        })

        ## Retorna os widgets referentes à escolha da variavel
        ## explicativa e das preditoras
        output$variables <- renderUI({
            if (input$grandTab == "Ajuste") {
                if (is.null(input$file)) {
                    HTML("<font style=\"font-weight: bold; color:red\">Carregue o conjunto de dados</font>")
                } else {
                    tagList(
                        selectInput(
                            inputId = "varY",
                            label = "Selecione a variável resposta",
                            choices = names(dados())),
                        selectInput(
                            inputId = "varX",
                            label = "Selecione a variável resposta",
                            choices = names(dados())[names(dados()) != input$varY],
                            selected = names(dados())[2],
                            multiple = TRUE)
                    )
                }
            } else return(NULL)
        })

        ## Faz gráfico descritivo das variáveis preditoras
        output$descrPlot <- renderPlot({
            if (is.null(input$file)) {
                return(NULL)
            } else {
                da <- dados()[, c(input$varY, input$varX)]
                plot(da)
            }
        })
        
        ##===========================================
        ## Interfaces de usuário
        output$OutputsInterface <- renderUI({
            ##-------------------------------------------
            if (input$grandTab == "Dados") {
                if (is.null(input$file)) {
                    return(NULL)
                } else {
                    return(
                        verbatimTextOutput("viewData")
                    )
                }
            }
            
            ##-------------------------------------------
            if (input$grandTab == "Ajuste") {
                if (is.null(input$file)) {
                    return(NULL)
                } else {
                    tabsetPanel(
                        tabPanel(
                            title = "Descritiva",
                            plotOutput("descrPlot")
                        ),
                        tabPanel("Modelo"),
                        tabPanel("Resíduos"),
                        tabPanel("Ajuste")
                    )
                }
            } else return(NULL) 
        })
        
    }
)
