##-------------------------------------------
## ui.R

library(shiny)
library(shinythemes)

shinyUI(
    fluidPage(
        ## theme = shinytheme("united"),
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
                  Regressão Conway-Maxwell-Poisson<br>
                  <font style='font-weight: normal; font-size: 14pt'><em>Versão 0.1</em></font>
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
                    tabPanel(
                        title = "Sobre",
                        hr(),
                        HTML("
                             <center>
                                <p><b>Autor:</b> Eduardo Elias Ribeiro Junior<p/>
                                <p><b>Código-fonte:</b> Disponível no serviço GitLab do C3SL.</p>
                                   <a href=\"https://gitlab.c3sl.ufpr.br/eerj12/ce064/tree/master/appComPoisson\">
                                      <img src = 'gitlabc3sl.svg' height = '110' width = '220'> </img>
                                   </a>
                             </center>
                             ")
                    ),

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
