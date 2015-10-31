##======================================================================
##                                                        Eduardo Junior
##                                                    eduardo.jr@ufpr.br
##                                                            30-10-2015
##======================================================================


##----------------------------------------------------------------------
## Pacotes utilizados
library(XML)
library(reshape)

##----------------------------------------------------------------------
## Carregando a pagina conforme marca e modelo informados
marca_modelo <- "mitsubishi/pajero-dakar"
url <- paste0("http://www.webmotors.com.br/comprar/carros/",
              "novos-usados/veiculos-todos-estados/",
              marca_modelo)

pr <- readLines(con = url)
str(pr)

##----------------------------------------------------------------------
## Decodificando a página
h <- htmlTreeParse(file = pr, asText = TRUE,
                   useInternalNodes = TRUE, encoding = "utf-8")

##----------------------------------------------------------------------
## Obter os dados de interesse / visualize pelo seu navegador as tags
## que envolvem os dados

##-------------------------------------------
## Valor do veiculo

## Os valores em reais estão envolvidos na tag div
valor <- getNodeSet(
    doc = h,
    path = "//div[@class=\"price\"]",
    fun = xmlValue
)

## Agora só temos que capturar o valor dentro da tag
valor <- sapply(valor, FUN = function(text) {
    gsub(x = gsub(x = text, pattern = "[[:punct:]]",
                  replace = "", perl = TRUE),
         pattern = "^.*R([0-9]+).*", replacement = "\\1")
})

valor <- as.numeric(valor)

##-------------------------------------------
## Numero de fotos

## Os valores em reais estão envolvidos na tag div
fotos <- getNodeSet(
    doc = h,
    path = "//div[@class=\"stripe-bottom\"]//span",
    fun = xmlValue
)

## Perceba que em bottom temos sempre o numero de fotos (a esquerda) e a
## estrela de favoritar (a direita) 
fotos <- fotos[seq(1, 24, 2)]

## Agora só temos que capturar o valor dentro da tag
fotos <- sapply(fotos, FUN = function(text) {
    gsub(x = text, pattern = "^([0-9]+).*", replacement = "\\1")
})

fotos <- as.numeric(fotos)

##-------------------------------------------
## Informaçoes gerais
info <- getNodeSet(
    doc = h,
    path = "//span[@class=\"version\"]",
    fun = xmlValue
)

info <- sapply(info, FUN = function(text) {
    gsub(x = text, "^([0-9]\\.[0-9]) .* (.*)$", replacement = "\\1;\\2")
})

info <- colsplit(info, split = ";", names = c("cil", "tipo"))

##-------------------------------------------
## Caracteristicas
carac <- getNodeSet(
    doc = h,
    path ="//div[@class=\"features\"]//span",
    fun = xmlValue
)
carac <- unlist(carac)

## Identificador da caracteristica, caso esteja algum anuncio em ordem
## diferente
id <- xpathSApply(
    doc = h,
    path = "//div[@class=\"features\"]//span//i",
    fun = xmlGetAttr, "class")

id <- sapply(as.list(id), FUN = function(text) {
    gsub(x = text, "^.*-([a-z]+)$", replacement = "\\1")    
})

## Concatenando as caracteristicas com a identificação
carac <- unstack(data.frame(carac, id))

## Higieniza as informações de quilometragem
carac$km <- as.numeric(
    sapply(as.list(carac$km), FUN = function(text) {
        gsub(x = gsub(x = text, pattern = "[[:punct:]]",
                      replace = "", perl = TRUE),
             pattern = "^([0-9]+) km", replacement = "\\1")
    })
)

## Higieniza as informações de ano de fabricação
carac$year <- as.integer(
    sapply(as.list(carac$year), FUN = function(text) {
        gsub(x = text, "^([0-9]{4})/.*$", replacement = "\\1")
    })
)

##-------------------------------------------
## Informações adicionais
info2 <- getNodeSet(
    doc = h,
    path ="//div[@class=\"card-footer\"]//span",
    fun = xmlValue
)

local <- unlist(info2)[seq(1, 24, 2)]
anuncio <- unlist(info2)[seq(2, 24, 2)]

## Ramificando o local
local <- sapply(as.list(local), FUN = function(text) {
    gsub(x = text, "^(.*) \\(([A-Z]{2})\\)$", replacement = "\\1;\\2")
})

local <- colsplit(local, split = ";", names = c("cidade", "estado"))

##-------------------------------------------
## Organizando tudo em um data frame

da <- data.frame(
    valor = valor,
    nfotos = fotos,
    info,
    carac,
    anuncio = anuncio,
    local
)

##======================================================================
## Funcao para coletar de todas as paginas

getWebMotors <- function(url) {
    ## Lendo a pagina
    pr <- readLines(con = url, warn = FALSE)
    ## Decodificando-a
    h <- htmlTreeParse(file = pr, asText = TRUE,
                       useInternalNodes = TRUE, encoding = "utf-8")
    ## Numero de anuncios nesta pagina * O numero de anuncios condiz com
    ## o número de tags h2 (titulo dos mesmos)
    n.anuncios <- length(
        xpathApply(h, "//h2[@itemprop=\"itemOffered\"]",fun = xmlValue)
    )
    ##-------------------------------------------
    ## Valor do veiculo
    valor <- getNodeSet(
        doc = h,
        path = "//div[@class=\"price\"]",
        fun = xmlValue
    )
    valor <- sapply(valor, FUN = function(text) {
        gsub(x = gsub(x = text, pattern = "[[:punct:]]",
                      replace = "", perl = TRUE),
             pattern = "^.*R([0-9]+).*", replacement = "\\1")
    })
    valor <- as.numeric(valor)
    ##-------------------------------------------
    ## Numero de fotos
    fotos <- getNodeSet(
        doc = h,
        path = "//div[@class=\"stripe-bottom\"]//span",
        fun = xmlValue
    )
    fotos <- fotos[seq(1, n.anuncios * 2, 2)]
    fotos <- sapply(fotos, FUN = function(text) {
        gsub(x = text, pattern = "^([0-9]+).*", replacement = "\\1")
    })
    fotos <- as.integer(fotos)
    ##-------------------------------------------
    ## Informaçoes gerais
    info <- getNodeSet(
        doc = h,
        path = "//span[@class=\"version\"]",
        fun = xmlValue
    )
    info <- sapply(info, FUN = function(text) {
        gsub(x = text, "^([0-9]\\.[0-9]) .* (.*)$",
             replacement = "\\1")
    })
    ## info <- colsplit(info, split = ";", names = c("cil", "tipo"))
    ##-------------------------------------------
    ## Caracteristicas
    carac <- getNodeSet(
        doc = h,
        path ="//div[@class=\"features\"]//span",
        fun = xmlValue
    )
    carac <- unlist(carac)
    id <- xpathSApply(
        doc = h,
        path = "//div[@class=\"features\"]//span//i",
        fun = xmlGetAttr, "class")
    id <- sapply(as.list(id), FUN = function(text) {
        gsub(x = text, "^.*-([a-z]+)$", replacement = "\\1")    
    })
    carac <- unstack(data.frame(carac, id))
    carac$km <- as.numeric(
        sapply(as.list(carac$km), FUN = function(text) {
            if(text == "N/I") NA
            else
                gsub(x = gsub(x = text, pattern = "[[:punct:]]",
                              replace = "", perl = TRUE),
                     pattern = "^([0-9]+) km", replacement = "\\1")
        })
    )
    carac$year <- as.integer(
        sapply(as.list(carac$year), FUN = function(text) {
            gsub(x = text, "^([0-9]{4})/.*$", replacement = "\\1")
        })
    )
    ##-------------------------------------------
    ## Informações adicionais
    info2 <- getNodeSet(
        doc = h,
        path ="//div[@class=\"card-footer\"]//span",
        fun = xmlValue
    )
    local <- unlist(info2)[seq(1, n.anuncios * 2, 2)]
    anuncio <- unlist(info2)[seq(2, n.anuncios * 2, 2)]
    local <- sapply(as.list(local), FUN = function(text) {
        gsub(x = text, "^(.*) \\(([A-Z]{2})\\)$",
             replacement = "\\1;\\2")
    })
    local <- colsplit(local, split = ";", names = c("cidade", "estado"))
    ##-------------------------------------------
    ##-------------------------------------------
    ## Organizando a saida
    da <- list(
        valor = valor,
        nfotos = fotos,
        cilindradas = info,
        ## cambio = info$tipo,
        cor = carac$color,
        km = carac$km,
        cambio = carac$shift,
        ano = carac$year,
        anuncio = as.character(anuncio),
        cidade = as.character(local$cidade),
        estado = as.character(local$estado)
    )
    return(da)
}

## Teste
getWebMotors(url = url)

