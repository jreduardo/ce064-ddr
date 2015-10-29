##-------------------------------------------
## ui.R

shinyUI(
    fluidPage(
        title = "Densidade Kernel",
        
        h1("Estimação Kernel de Densidade",
           style = "font-family: 'Ubuntu Light';
                    color: #fff; text-align: center;
                    background-color: #C8C8C8;
                    padding: 20px; font-weight: bold;"),
        
        column(width = 3,
               sidebarPanel(
                   width = 12,
                   
                   h4("Controladores da estimação"),
                   
                   hr(),
                   
                   selectInput("data", "Escolhao conjunto de dados",
                               choices = c("rend", "precip")),
                   
                   uiOutput("controls")
               )
               ),
        
        column(width = 6,
               plotOutput("density", height = "480px")
               ),
        
        column(width = 3,
               sidebarPanel(
                   width = 12,
                   
                   h4("Controladores gráficos"),
                   
                   hr(),
                   
                   column(width = 6,
                          numericInput("lwd_dn",
                                       "Largura da curva de densidade",
                                       min = 1, max = 10,
                                       value = 2, step = 1)
                          ),
                   
                   column(width = 6,
                          numericInput("lwd_kn",
                                       "Largura da curva de kernel",
                                       min = 1, max = 10,
                                       value = 1, step = 1)
                          ),
                   
                   column(width = 6,
                          textInput("col_dn", "Cor da densidade",
                                    "black")
                          ),
                   
                   column(width = 6,
                          textInput("col_kn", "Cor do kernel",
                                    "blue")
                          ),
                   
                   column(width = 12,
                          checkboxInput("rug", "Posição dos dados"),
                          checkboxInput("box", "Enquadrar gráfico")
                          ),
                   
                   downloadButton('downloadPDF',
                                  'Download da imagem')
               )
               )
    )
)
