##-------------------------------------------
## ui.R

library(shiny)

shinyUI(
    fluidPage(
        ##-------------------------------------------
        ## Inclui estilo css
        tags$head(
            tags$link(
                rel = "stylesheet",
                type = "text/css",
                href = "style.css"),
            tags$script(
                type = "text/javascript",
                src = "http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"
            )
        ),

        ##-------------------------------------------
        ## Inclui cabeçalho da aplicação
        HTML("
        <table class = 'compois'>
           <td class = 'compois' align='right'> 
               <img src = 'logo_tcc.svg' height = '100' width = '100'> </img> 
           </td>  
           <td class = 'compois'> 
              <h1 class = 'compois'>
                  Regressão Conway-Maxwell-Poisson
              </h1><br>
           </td>
        </table>"),

        ##-------------------------------------------
        ## 
        sidebarLayout(
            sidebarPanel(
                width = 5,
                tabsetPanel(
                    id = "grandTab",
                    type = "pills",
                    ##-------------------------------------------
                    ## Informações gerais
                    tabPanel("Sobre"),

                    ##-------------------------------------------
                    ## Ler dados
                    tabPanel(
                        "Dados",
                        hr(),
                        ## Dados em formato texto
                        fileInput(
                            inputId = "file",
                            label = "Selecione o arquivo de dados",
                            accept = c(
                                'text/csv',
                                'text/comma-separated-values',
                                'text/tab-separated-values',
                                'text/plain',
                                '.csv',
                                '.tsv'
                            )),
                        ## Opções adicionais
                        HTML("<label>Selecione as opções para leitura</label>"),
                        checkboxInput(
                            inputId = "header",
                            label = "Cabeçalho",
                            value = TRUE),
                        radioButtons(
                            inputId = "separator",
                            label="Separador de campo",
                            choices=c(
                                "Vírgula" = ",",
                                "Ponto e vírgula" = ";",
                                "Tabulação" = "\t"),
                            selected = ";"),
                        radioButtons(
                            inputId = "decimal",
                            label = "Decimal numérico",
                            choices = c(
                                Ponto = ".",
                                Vírgula = ","),
                            selected = ","),
                        radioButtons(
                            inputId = "quote",
                            label ="Quote",
                            choices = c(
                                Nenhuma = "",
                                Dupla = '"',
                                Simples = "'"),
                            selected = '"')
                        
                    ),

                    ##-------------------------------------------
                    ## Ajustar modelo
                    tabPanel(
                        title = "Ajuste",
                        hr(),
                        uiOutput("variables")
                    ),
                    
                    ##-------------------------------------------
                    ## Editar relatório
                    tabPanel(
                        title = "Relatório",
                        hr(),
                        uiOutput("report")   
                    )
                )
            ),

            mainPanel(
                width = 7,
                ## verbatimTextOutput("teste"),
                uiOutput("OutputsInterface")           
            )
        )
    )
)
