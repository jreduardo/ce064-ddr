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

##======================================================================
## Extração de dados de todas as veiculos no site webMotors

##-------------------------------------------
## Lendo a primeira página para descobrir o total de veículos

url <- paste0("http://www.webmotors.com.br/comprar/carros/",
              "novos-usados/veiculos-todos-estados/mitsubishi/",
              "pajero-dakar?qt=36&o=1&p=1")
pr <- readLines(con = url)

h <- htmlTreeParse(file = pr, asText = TRUE,
                   useInternalNodes = TRUE, encoding = "utf-8")

nveiculos <- getNodeSet(
    doc = h,
    path = "//span[@class=\"size-xbigger\"]",
    fun = xmlValue
)

nveiculos <- as.numeric(
    gsub(x = as.character(nveiculos),
         "^([0-9]+) .*$", replacement = "\\1")
)

nanuncios <- 36 ## Argumento `qt` na url, pode assumir 12, 24 ou 36
npages <- ceiling(nveiculos / nanuncios)

##----------------------------------------------------------------------
## Fazendo o processo de extração iterativamente para as `npages`
## páginas 

da <- data.frame(
    valor = vector("numeric", nveiculos),
    nfotos = vector("integer", nveiculos),
    cilindradas = vector("character", nveiculos),
    ## cambio = info$tipo,
    cor = vector("character", nveiculos),
    km = vector("numeric", nveiculos),
    cambio = vector("character", nveiculos),
    ano = vector("integer", nveiculos),
    anuncio = vector("character", nveiculos),
    cidade = vector("character", nveiculos),
    estado = vector("character", nveiculos),
    stringsAsFactors = FALSE
)

last <- 0
for(i in 1:npages) {
    ## Montando as urls
    url <- paste0("http://www.webmotors.com.br/comprar/carros/",
                  "novos-usados/veiculos-todos-estados/mitsubishi/",
                  "pajero-dakar?qt=", nanuncios, "&o=1&p=", i)
    ## Extraindo os dados
    resul <- getWebMotors(url = url)
    ## Posições de referencia
    pos <- last + 1
    last <- nanuncios * i
    x <- pos:last
    if( i == npages) x = pos:nveiculos
    ## Preenchendo o data frame
    da$valor[x] <- resul$valor
    da$nfotos[x] <- resul$nfotos
    da$cilindradas[x] <- resul$cilindradas
    da$cor[x] <- resul$cor
    da$km[x] <- resul$km
    da$cambio[x] <- resul$cambio
    da$ano[x] <- resul$ano
    da$anuncio[x] <- resul$anuncio
    da$cidade[x] <- resul$cidade
    da$estado[x] <- resul$estado
}

## Escrevendo em arquivo externo
write.table(
    x = da,
    file = "pajero-dakar.csv",
    sep = ";",
    fileEncoding = "utf-8",
    row.names = FALSE,
    quote = FALSE
)
