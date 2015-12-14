##-------------------------------------------
## server.R

library(shiny)

shinyServer(
    function(input, output, session) {
        
        ## ##-------------------------------------------
        ## ## Para testes
        ## output$teste <- renderPrint({
        ##     input$file$datapath
        ## })
        ## ##-------------------------------------------
        
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

        output$viewData <- renderPrint({
            dados()
        })

        ##===========================================
        ## Interfaces de usuário
        output$OutputsInterface <- renderUI({
            if (input$grandTab == "Dados") {
                if (is.null(input$file)) {
                    return(NULL)
                } else {
                    verbatimTextOutput("viewData")
                }
            } else return(NULL)
        })
        
    }
)
