##-------------------------------------------
## server.R

rend <- c(6388, 6166, 6047, 5889, 5823, 5513, 5202, 5172, 5166, 4975,
          4778, 4680, 4660, 5403, 5117, 5063, 4993, 4980, 4770, 4685,
          4614, 4552, 3973, 4550, 5056, 4500, 4760, 5110, 4960, 4769,
          4849, 5230)

choi <- c("gaussian","epanechnikov","rectangular",
          "triangular","biweight","cosine","optcosine")

shinyServer(
    function(input, output) {
        ##-------------------------------------------
        ## Estimando a densidade como valores reativos
        base <- reactive({
            switch(input$data,
                   "precip" = list(da = precip, dens = density(precip)),
                   "rend" = list(da = rend, dens = density(rend))
                   )
        })
                
        ##-------------------------------------------
        ## Controladores de kernel
        output$controls <- renderUI({
            tagList(
                selectInput("kernel", "Escolha o Kernel",
                            choices = choi),
                
                sliderInput("bandwidth", "Largura de banda",
                            min = round(base()$dens$bw * 0.2),
                            max = round(base()$dens$bw * 2),
                            value = base()$dens$bw),
                
                sliderInput("c0", "Coordenada de estimação",
                            min = min(base()$da),
                            max = max(base()$da),
                            value = median(base()$da))
            )
        })
        
        ##-------------------------------------------
        ## Gráfico de kernel
        output$density <- renderPlot({
            ## Parametros graficos
            da <- base()$da
            rug <- input$rug
            lwd_dn<- input$lwd_dn
            lwd_kn<- input$lwd_kn
            col_dn <- ifelse(input$col_dn %in% colors(),
                             input$col_dn, 1)
            col_kn <- ifelse(input$col_kn %in% colors(),
                             input$col_kn, 1)
            box <- input$box
            
            ## Estimando a densidade
            aux <- density(
                base()$da,
                bw = input$bandwidth,
                kernel = input$kernel)

            ## Construindo o histrograma e densidade estimada
            par(mar = c(4, 0, 0, 0))
            hist(da, prob = TRUE,
                 main = NA, yaxt="n", xlab = input$data,
                 ## breaks = seq(min(da), max(da),
                 ##              length.out = input$bandwidth),
                 xlim = extendrange(range(da), f = 0.2),
                 ylim = c(0, max(base()$dens$y) * 1.3),
                 col = "gray90", border = "white")
            
            lines(aux$x, aux$y,
                  lwd = lwd_dn, col = col_dn)
            
            ## faz o intervalo com o comprimento da banda
            arrows(input$c0 - 2 * input$bandwidth, 0,
                   input$c0 + 2 * input$bandwidth, 0,
                   length = 0.05, code = 3, angle = 90,
                   col = col_kn, lwd = lwd_kn)
            
            ## faz uma seta que aponta para o valor da função
            y0 <- approx(aux$x, aux$y, xout = input$c0)
            arrows(input$c0, 0, input$c0, y0$y, length = 0.1,
                   col = col_kn, lwd = lwd_kn)
            
            ## desenha a função kernel acima do intevalo
            d <- density(input$c0,
                         bw = input$bandwidth,
                         kernel = input$kernel)
            lines(d$x, d$y / length(da), col = col_kn, lwd = lwd_kn)
            
            if(rug) rug(da, lwd = lwd_dn, col = col_dn)
            if(box) box()
        })

        ##-------------------------------------------
        ## Download da imagem

        output$downloadPDF <- downloadHandler(
            filename = function() { 
                paste(input$data, '_density.pdf', sep='') 
            },
            content = function(file) {
                pdf(file = file)
                ## Parametros graficos
                da <- base()$da
                rug <- input$rug
                lwd_dn<- input$lwd_dn
                lwd_kn<- input$lwd_kn
                col_dn <- ifelse(input$col_dn %in% colors(),
                                 input$col_dn, 1)
                col_kn <- ifelse(input$col_kn %in% colors(),
                                 input$col_kn, 1)
                box <- input$box

                ## Estimando a densidade
                aux <- density(
                    base()$da,
                    bw = input$bandwidth,
                    kernel = input$kernel)

                ## Construindo o histrograma e densidade estimada
                par(mar = c(4, 0, 0, 0))
                hist(da, prob = TRUE,
                     main = NA, yaxt="n", xlab = input$data,
                     ## breaks = seq(min(da), max(da),
                     ##              length.out = input$bandwidth),
                     xlim = extendrange(range(da), f = 0.2),
                     ylim = c(0, max(base()$dens$y) * 1.3),
                     col = "gray90", border = "white")
                
                lines(aux$x, aux$y,
                      lwd = lwd_dn, col = col_dn)
                
                ## faz o intervalo com o comprimento da banda
                arrows(input$c0 - 2 * input$bandwidth, 0,
                       input$c0 + 2 * input$bandwidth, 0,
                       length = 0.05, code = 3, angle = 90,
                       col = col_kn, lwd = lwd_kn)
                
                ## faz uma seta que aponta para o valor da função
                y0 <- approx(aux$x, aux$y, xout = input$c0)
                arrows(input$c0, 0, input$c0, y0$y, length = 0.1,
                       col = col_kn, lwd = lwd_kn)
                
                ## desenha a função kernel acima do intevalo
                d <- density(input$c0,
                             bw = input$bandwidth,
                             kernel = input$kernel)
                lines(d$x, d$y / length(da), col = col_kn, lwd = lwd_kn)

                if(rug) rug(da, lwd = lwd_dn, col = col_dn)
                if(box) box()
                dev.off()
            }
        )
    }
)
