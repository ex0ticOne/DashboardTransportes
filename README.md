# Dashboard de Sinistros de Seguro de Transportes

Esse é um dashboard para exibição de projeções de sinistros de seguros de transportes a nível nacional (embarques e desembarques em território brasileiro) e internacional (embarque nacional e desembarque em outro país).

A fonte dos dados é o dataset fornecido pela SUSEP, em seu portal oficial do Sistema de Estatísticas da SUSEP.

O dashboard carrega dinamicamente os modelos conforme a seleção do usuário e faz as projeções em tempo real conforme a quantidade desejada de anos futuros.

Os modelos são produzidos pelo script `PreparoDadosTransporte.R`

Sintaxe no terminal:

`Rscript PreparoDadosTransporte.R [ano acima de 2003 até o atual]`

O script produzirá modelos considerando os dados a partir do ano escolhido e salvará em arquivos .rds na pasta `modelos`, acessada pelo dashboard (não alterar os nomes dos arquivos, para evitar erros de carregamento).

Durante a produção dos modelos, o script converte a série de dados para uma escala logarítmica de base 10, na intenção de suavizar a influência dos diferentes patamares de valores de indenização (procedimento comum em análises de séries históricas de ações).

O script também pula as combinações companhia/UF/ramo que possuam menos de 12 registros de dados na série histórica, pois esses modelos geram projeções de baixa qualidade.

Pacotes usados:

-   tidyverse;

-   prophet;

-   shiny, shinydashboard, shinyWidgets;

-   dygraphs;

Para ver esse dashboard funcionando em ambiente hospedado, acesse: <https://ricardo-baptista.shinyapps.io/DashboardTransportes/>
