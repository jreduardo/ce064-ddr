##-------------------------------------------
## global.R

## Coleção de funções utilizadas na aplicação

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
                           toc = TRUE)),
        "HTML" = render(file,
                        output_format = html_document(
                            toc = TRUE,
                            number_sections = TRUE
                        )),
        "MS Word" = render(file,
                           output_format = word_document()),
        "MD" = render(file, output_format = md_document())
    )
}
