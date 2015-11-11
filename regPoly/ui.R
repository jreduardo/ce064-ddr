##-------------------------------------------
## ui.R

library(shiny)

ds <- c("swiss", "cars", "longley", "iris", "rock")

shinyUI(
    fluidPage(
        title = "Regressão polinomial",

        h1("Regressão polinomial",
           style = "font-family: 'Ubuntu Light';
                    color: #fff; text-align: center;
                    background-color: #C8C8C8;
                    padding: 20px; font-weight: bold;"),

        sidebarLayout(
            sidebarPanel(
                fileInput("file", "Upload dos dados"),

                selectInput("data", label = "Conjunto de dados",
                            choices = ds),

                selectInput("x", "Variável resposta",
                            choices = names(swiss)),
                
                selectInput("y", "Variável preditora",
                            choices = names(swiss)),
                
                numericInput("degree", "Grau do polinômio",
                             value = 1, min = 1, max = 10, step = 1),

                hr(),

                verbatimTextOutput("teste")
            ),

            mainPanel(
                tabsetPanel(
                    tabPanel("Summary e Anova", {
                        tagList(
                            verbatimTextOutput("summary"),
                            verbatimTextOutput("anova"))
                    }),
                    
                    tabPanel("Medidas de Influência",
                             dataTableOutput("leverage")
                             ),
                    
                    tabPanel("Resíduos",
                             plotOutput("residuals")
                            ),
                    
                    tabPanel("Ajuste",
                             plotOutput("fit")
                             )
                )
            )
        )
    )
)
