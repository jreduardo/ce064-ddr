##-------------------------------------------
## server.R

library(shiny)
library(CompGLM)
library(rmarkdown)

##-------------------------------------------
## FUNCTIONS
myanova <- function(lista){
    with(lista, {
        logVeroP <- as.numeric(logLik(modelP))
        logVeroC <- as.numeric(logLik(modelC))
        npP <- length(coef(modelP))
        npC <- length(coef(modelC)$beta) + 1
        TRV <- 2 * log(logVeroC - logVeroP)
        pvalue <- 1 - pchisq(TRV, 1)
        return(data.frame(
            "Num.Parameters" = c(npP, npC),
            "logLik" = c(logVeroP, logVeroC),
            "diffLog" = c(NA, logVeroP - logVeroC),
            "TRV" = c(NA, TRV),
            "pvalue" = c(NA, pvalue)))
    })
}
mypredict <- function(model, y){
    range <- extendrange(y, f = 4)
    min <- range[1]; max <- range[2]
    if (min < 0) min <- 0
    x <- min:max
    apply(predict(model), 1, function(par) 
        sum(x * dcomp(x, lam = par[1], nu = par[2])))
}
mycompile <- function(format, file){
    switch(
        format,
        "PDF" = render(file,
                       output_format = pdf_document(
                           toc = TRUE,
                           number_sections = TRUE,
                           includes = includes(in_header = "style_report.tex")
                       )),
        "HTML" = render(file,
                        output_format = html_document(
                            toc = TRUE,
                            number_sections = TRUE,
                            css = "style_report.css"
                        )),
        "MS Word" = render(file,
                           output_format = word_document()),
        "MD" = render(file, output_format = md_document())
    )
}

##-------------------------------------------

shinyServer(
    function(input, output, session) {
        
        ##-------------------------------------------
        ## Para testes
        output$teste <- renderPrint({
            ## input$userExplanation
        })
        ##-------------------------------------------

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
                            choices = names(dados()),
                            selected = NA,
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
                da <- dados()[, c(input$varX, input$varY)]
                if (!is.data.frame(da)) {
                    barplot(table(da))
                } else plot(da)
            }
        })

        ## Realiza ajuste do modelo
        ajuste <- reactive({
            if (is.null(input$file)) {
                return(NULL)
            } else {
                da <- dados()[, c(input$varY, input$varX)]
                if (is.null(input$varX)) {
                    da <- data.frame(y = da)
                    modelP <- glm(y ~ 1, family = poisson, data = da)
                    modelC <- glm.comp(y ~ 1, data = da)
                } else {
                    names(da)[1] <- "Y"
                    modelP <- glm(Y ~ ., family = poisson, data = da)
                    modelC <- glm.comp(Y ~ ., data = da)
                }
                return(list(modelP = modelP, modelC = modelC))
            }
        })

        ## Exibe as inferência para o modelo ajustado
        output$viewModel <- renderPrint({
            if (is.null(input$file)) {
                return(NULL)
            } else {
                cat("======================================================================",
                    "Teste de Razão de verossimilhança entre Poisson e COM-Poisson\n",
                    capture.output(myanova(ajuste())),
                    "\n======================================================================",
                    "Estimativas do modelo",
                    capture.output(summary(ajuste()$modelC)),
                    sep = "\n")
            }
        })

        ## Exibe o gráfico de resíduos
        output$viewResiduals <- renderPlot({
            if (is.null(input$file)) {
                return(NULL)
            } else {
                Y <- dados()[, input$varY]
                plot(Y - mypredict(ajuste()$modelC, Y),
                     ylab = "Resíduos ordinários")
            }
        })

        ## Compila o relatório
        output$download <- downloadHandler(
            filename = function(){
                switch(
                    input$format,
                    "PDF" = "report.pdf",
                    "HTML" = "report.html",
                    "MS Word" = "report.docx",
                )
            },
            content = function(file){
                ## Converte de Rmd para o formato descrito
                generate <- mycompile(
                    input$format,
                    "./www/relatorio.Rmd")
                ## Empurra o arquivo para download.
                file.copy(from = generate,
                          to = file,
                          overwrite = TRUE)
                ## ## Remove arquivos auxiliares descessários.
                ## file.remove(list.files(
                ##     pattern="\\.(log|out|vrb|pdf)$"))
            })

        output$report <- renderUI({
            if (input$grandTab == "Relatório") {
                if (is.null(input$file)) {
                    HTML("<font style=\"font-weight: bold; color:red\">Carregue o conjunto de dados</font>")
                } else {
                    if (is.null(input$varY)) {
                        HTML("<font style=\"font-weight: bold; color:red\">Ajuste o modelo primeiro</font>")
                    } else {
                        tagList(

                            textInput(
                                inputId =  "userName",
                                label = "Nome do utilizador",
                                value = "Utilizador"
                            ),
                            
                            HTML("<label>Uma breve introdução</label>"),
                            HTML('<textarea id="userExplanation"
                              rows="4" cols="40" style="width: 100%"> </textarea>'),

                            HTML("<label>Conclusão</label>"),
                            HTML('<textarea id="userConclusion"
                              rows="4" cols="40" style="width: 100%"> </textarea>'),
                            
                            radioButtons(
                                inputId = "format",
                                label = "Formato do relatório",
                                choices = c("PDF", "HTML", "MS Word"),
                                inline = TRUE),

                            downloadButton(
                                outputId = "download",
                                label = "Download")
                        )
                    }
                }
            } else return(NULL)
        })
        
        ##===========================================
        ## Interfaces de usuário no mainPanel
        output$OutputsInterface <- renderUI({

            ##-------------------------------------------
            if (input$grandTab == "Sobre") {
                HTML("Sobre")
            } else {
                
                ##-------------------------------------------
                if (input$grandTab == "Dados") {
                    if (is.null(input$file)) {
                        return(NULL)
                    } else {
                        return(
                            verbatimTextOutput("viewData")
                        )
                    }
                } else {
                    
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
                                tabPanel(
                                    title = "Modelo",
                                    verbatimTextOutput("viewModel")
                                ),
                                tabPanel(
                                    title = "Resíduos",
                                    plotOutput("viewResiduals")
                                )
                            )
                        }
                    } else {
                        if (input$grandTab == "Relatório") {
                            if (is.null(input$file) || is.null(input$varY)) {
                                return(NULL)
                            } else {
                                ## Salva os objetos utilizados no relatório
                                x <- list(
                                    data = dados(),
                                    vars = c(input$varY, input$varX),
                                    models = ajuste(),
                                    name = input$userName,
                                    text = c(input$userExplanation,
                                             input$userConclusion)
                                )
                                save(x, file = "./www/image.RData")
                                mycompile(
                                    "MD", "./www/relatorio.Rmd"
                                )
                                includeMarkdown("./www/relatorio.md")
                            }
                        }
                    }
                }
            }
        })
    }
)

