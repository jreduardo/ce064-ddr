### Projeto COM-Poisson App ###

Este projeto surgiu como proposta da disciplina
[CE064 - Ferramentas para Documentos Dinâmicos Reproduzíveis][ce064],
ofertada pelo departamento de Estatística na Universidade Federal do
Paraná como disciplina optativa. Porém o desenvolvimento do aplicativo se
perpetuará mesmo após o término da disciplina. O objetivo do aplicativo
é ajustar um modelo _COM-Poisson_ de forma interativa via interface
_web_, onde o utilizador não precisará ter total conhecimento do método
tampouco da construção do aplicativo.

#### A aplicação ####

A aplicação contém quatro abas de navegação que são utilizadas
sequencialmente para o ajuste de um modelo. As abas são:

 - **Sobre**: Contém informações gerais sobre o autor, projeto,
   objetivos e utilização.
 - **Dados**: Esta é a primeira etapa da aplicação, aqui o utilizador
   carrega um conjunto de dados de seu computador pessoal para o
   aplicativo.
 - **Ajuste**: Nesta etapa são escolhidas as variáveis que farão parte
   do modelo, tanto a explicativa quanto as preditoras, gráficos
   descritivas, medidas resumo do modelo e gráficos de resíduos são
   exibidos como auxílio
 - **Relatório**: Finalmente a última etapa da aplicação é responsável
   pela geração de relatório. Estão disponíveis três formatos de saída e
   informações introdutórias e de conclusão são permitidas ao usuário.

#### Pretensões ####

 1. Melhorar as funções de estimação da COM-Poisson, atualmente se faz
    com o pacote `CompGLM` do R.
 2. Permitir o ajuste de outros modelos flexíveis para dados de
    contagem:
        * Binomial Negativo
        * Quase-Poisson
        * Contagem Gama
 3. Interpretações condicionais no relatório, atualmente não se tem
    interpretações de resultados no relatório.
    
[ce064]: http://www.leg.ufpr.br/~walmes/ensino/ce064-2015-02/
[c3sl]: http://www.c3sl.ufpr.br/
