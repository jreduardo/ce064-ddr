##-------------------------------------------
## server.R

library(shiny)

shinyServer(
    function(input, output) {
        ##-------------------------------------------
        ## Testes
        output$teste <- renderPrint({
            str(input$file)
        })

        ##-------------------------------------------
        ## Função de ajuste
        reactive_fit <- reactive({
            dados <- swiss[, c(input$x, input$y)]
            names(dados) <- c("x", "y")
            modelo <- lm(
                formula = y ~ poly(x, degree = input$degree),
                data = dados)
            return(list(dados = dados, modelo = modelo))
        })

        output$summary <- renderPrint({
            summary(reactive_fit()$modelo)
        })

        output$anova <- renderPrint({
            anova(reactive_fit()$modelo)
        })

        output$leverage <- renderDataTable({
            im <- influence.measures(reactive_fit()$modelo)
            im$infmat
        })

        output$residuals <- renderPlot({
            par(mfrow = c(2, 2))
            plot(reactive_fit()$modelo)
        })

        output$fit <- renderPlot({
            ##-------------------------------------------
            ## Obtendos valores preditos
            with(reactive_fit(), {
                pred <- data.frame(
                    x = seq(min(dados$x), max(dados$x), length = 50))
                a <- predict(modelo, newdata = pred,
                             interval = "confidence")
                pred <- cbind(pred, a)
                cols <- c(rgb(0.5, 0.5, 0.5, 0.15),
                          "#0080ff", "#839496")
                par(fg = cols[3], col.axis = cols[3],
                    col.lab = cols[2])
                plot(y ~ x, data = reactive_fit()$dados,
                     xlab = input$x, ylab = input$y,
                     pch = 21, bg = "gray", col = cols[2])
                grid()
                matlines(x = pred[, 1], pred[, 2:4], lty = c(1, 2, 2),
                         col = cols[2])
                polygon(x = c(pred[, 1], rev(pred[, 1])),
                        y = c(pred[, 3], rev(pred[, 4])),
                        col = cols[1], border = NA)
            })
        })

    }
)
