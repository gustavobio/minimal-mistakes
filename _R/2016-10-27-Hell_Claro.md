Rio Claro (SP) tem fama de ser mais violenta que outras cidades da região. Será mesmo? Nessa postagem vou dar uma olhada rápida nos dados da SSP SP. De qualquer forma, em 7 meses morando aqui, já tentaram levar minha bicicleta, explodiram o banco da Unesp, furtaram as panelas do restaurante universitário e por aí vai. A questão é: Rio Claro é mais violenta que outras cidades paulistas do mesmo tamanho?

Para responder a essa pergunta, vou usar os dados de criminalidade da [SSP SP](http://www.ssp.sp.gov.br/novaestatistica/Pesquisa.aspx). Pra variar, não há nenhum serviço facilite a compilação desses dados para todas as cidades. O jeito vai ser arrancar tudo da página deles pelo R. Pacotes:

``` r
library(httr)
library(rvest)
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(broom)
library(vegan)
```

Algumas funções para criar as requisições e baixar os dados já em um formato mais amigável:

``` r
post_request <- function(id_municipio = 0) {
  view_state <- read_html("http://www.ssp.sp.gov.br/novaestatistica/Pesquisa.aspx") %>%
    html_nodes("#__VIEWSTATE") %>%
    html_attr("value")

  event_validation <- read_html("http://www.ssp.sp.gov.br/novaestatistica/Pesquisa.aspx") %>%
    html_nodes("#__EVENTVALIDATION") %>%
    html_attr("value")
  res <- POST(url = "http://www.ssp.sp.gov.br/novaestatistica/Pesquisa.aspx",
              encode="form",
              user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/42.0.2311.50 Safari/537.36"),
              add_headers('Referer'="http://www.ssp.sp.gov.br/novaestatistica/Pesquisa.aspx",
                          'X-MicrosoftAjax'="Delta=true"),
              body=list(
                '__EVENTTARGET' = "ctl00$ContentPlaceHolder1$btnAnual",
                '__EVENTARGUMENT' = "",
                '__LASTFOCUS' = "",
                '__VIEWSTATE' = view_state,
                '__EVENTVALIDATION' = event_validation,
                'ctl00$ucHeaderSSP$drpLinksGov' = "",
                'ctl00$ucHeaderSSP$txtBusca' = 'Digite sua busca...',
                'ctl00$ContentPlaceHolder1$ddlAnos' = 0,
                'ctl00$ContentPlaceHolder1$ddlRegioes' = 0,
                'ctl00$ContentPlaceHolder1$ddlMunicipios' = id_municipio
              )
  )
  res_t <- content(res, as="text")
  res_h <- paste0(unlist(strsplit(res_t, "\r\n"))[-1], sep="", collapse="\n")
  return(res_h)
}

get_stats <- function(id_municipio = 11) {
  res_h <- post_request(id_municipio)
  css_tabela <- "#ContentPlaceHolder1_gridAnual"
  tabela_crimes <- read_html(res_h) %>%
    html_nodes(css_tabela) %>%
    html_table(dec = ",")
  tabela_crimes <- mutate_all(tabela_crimes[[1]], funs(parse_number(., locale = locale(decimal_mark = ","))))
  nomes_colunas <- c("ano", "homicidio_doloso", "furto", "roubo", "furto_roubo_veiculos")
  names(tabela_crimes) <- nomes_colunas
  tabela_crimes$municipio <- subset(get_id(), id == id_municipio)[, 1]
  return(tabela_crimes[c("municipio", nomes_colunas)])
}

get_id <- function() {
  css_id_municipios <- "#ContentPlaceHolder1_ddlMunicipios"
  res_h <- post_request()
  ids <- read_html(res_h) %>%
    html_nodes(xpath = "//select[@class='ddlMunic']/option") %>%
    html_attr("value")
  municipios <- read_html(res_h) %>%
    html_nodes(xpath = "//select[@class='ddlMunic']/option") %>%
    html_text()
  data.frame(municipio = municipios, id = as.numeric(ids), stringsAsFactors = FALSE)
}
```

A função `post_request` seleciona as opções no formulário da página da SSP e envia a requisição. A função `get_stats` processa o resultado da requisição e retorna uma tabela com os resultados compilados pra uma determinada cidade. Finalmente, a função `get_id` cria um data frame com os ids de cada município de acordo com o esperado pela página da SSP.

Depois, basta criar um vetor com os identificadores dos municípios e baixar todos os dados:

``` r
# ids dos municípios. São número sequencias de 1 a 645.
ids <- get_id()

# Baixando os dados pra cada município e retornando uma lista de data frames (DEMORADO!)
municipios <- sapply(ids$id[-1], get_stats, simplify = FALSE)

# Juntando tudo em um data frame só
municipios <- do.call("rbind", municipios)
```

Uma olhada rápida revela seu conteúdo:

``` r
glimpse(municipios)
```

    ## Observations: 9,675
    ## Variables: 6
    ## $ municipio            <chr> "Adamantina", "Adamantina", "Adamantina",...
    ## $ ano                  <int> 2001, 2002, 2003, 2004, 2005, 2006, 2007,...
    ## $ homicidio_doloso     <int> 4, 1, 1, 4, 0, 1, 0, 1, 1, 1, 0, 0, 0, 1,...
    ## $ furto                <int> 268, 319, 277, 420, 376, 294, 327, 397, 2...
    ## $ roubo                <int> 17, 4, 7, 5, 13, 7, 11, 5, 19, 11, 11, 6,...
    ## $ furto_roubo_veiculos <int> 2, 0, 4, 3, 6, 14, 8, 5, 2, 5, 10, 12, 7,...

Como eu quero comparar Rio Claro com cidades de tamanho parecido, preciso do número de habitantes por município. Dados do IBGE para 2015:

``` r
pop <- read.csv("input/pop.csv", stringsAsFactors = FALSE)

# Regex pra tirar tudo que não for numérico da coluna com os números de habitantes.
pop <- pop[-grep("[^\\d]", pop$pop, perl = TRUE), ]

pop$pop <- as.numeric(pop$pop)

# Só as cidades paulistas
pop_sp <- subset(pop, uf == "SP")
```

Alguns nomes de cidades no banco do IBGE não bate com os da SSP:

``` r
with(pop_sp, municipio[!municipio %in% municipios$municipio])
```

    ## [1] "Biritiba-Mirim"        "Embu-Guaçu"            "Moji Mirim"           
    ## [4] "Santa Rosa de Viterbo" "Tarabai"               "Uchoa"

``` r
# O jeito foi resolver na mão
pop_sp$municipio[pop_sp$municipio == "Biritiba-Mirim"] <- "Biritiba Mirim"
pop_sp$municipio[pop_sp$municipio == "Embu-Guaçu"] <- "Embu Guaçú"
pop_sp$municipio[pop_sp$municipio == "Moji Mirim"] <- "Mogi Mirim"
pop_sp$municipio[pop_sp$municipio == "Santa Rosa de Viterbo"] <- "Santa Rosa do Viterbo"
pop_sp$municipio[pop_sp$municipio == "Tarabai"] <- "Tarabaí"
pop_sp$municipio[pop_sp$municipio == "Uchoa"] <- "Uchôa"
```

Juntando os dois data frames:

``` r
municipios <- merge(municipios, pop_sp[, c("municipio", "pop")])
glimpse(municipios)
```

    ## Observations: 9,675
    ## Variables: 7
    ## $ municipio            <chr> "Adamantina", "Adamantina", "Adamantina",...
    ## $ ano                  <int> 2001, 2002, 2003, 2004, 2005, 2006, 2007,...
    ## $ homicidio_doloso     <int> 4, 1, 1, 4, 0, 1, 0, 1, 1, 1, 0, 0, 0, 1,...
    ## $ furto                <int> 268, 319, 277, 420, 376, 294, 327, 397, 2...
    ## $ roubo                <int> 17, 4, 7, 5, 13, 7, 11, 5, 19, 11, 11, 6,...
    ## $ furto_roubo_veiculos <int> 2, 0, 4, 3, 6, 14, 8, 5, 2, 5, 10, 12, 7,...
    ## $ pop                  <dbl> 35048, 35048, 35048, 35048, 35048, 35048,...

Agora já dá pra começar a explorar os dados. Primeiro eu quero saber se Rio Claro, São Carlos e Ribeirão Preto (as cidades que eu costumo frequentar) apresentaram números parecidos com os dos municípios mais violentos em 2015. Começando com o número de furtos por 100 mil habitantes:

``` r
municipios %>%
  filter(ano == 2015) %>%
  mutate(stat = furto/pop*100000, bin = ifelse(municipio %in% c("São Carlos", "Rio Claro", "Ribeirão Preto"), 1, 0)) %>%
  arrange(desc(stat)) %>%
  filter(row_number() %in% 1:30 | municipio %in% c("São Carlos", "Rio Claro", "Ribeirão Preto")) %>%
  mutate(municipio = reorder(municipio, stat)) %>%
  ggplot(aes(x = municipio, y = stat, fill = factor(bin))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("1" = "darkgreen", "0" = "grey"), guide = FALSE) +
  coord_flip() +
  xlab("") + 
  ylab("") + 
  ggtitle("Furtos por 100 mil habitantes em 2015")
```

![](/fig/2016-10-27-Hell_Claro_files/figure-markdown_github/unnamed-chunk-8-1.png)

O negócio está feio na praia. E quanto aos roubos?

``` r
municipios %>%
  filter(ano == 2015) %>%
  mutate(stat = roubo/pop*100000, bin = ifelse(municipio %in% c("São Carlos", "Rio Claro", "Ribeirão Preto"), 1, 0)) %>%
  arrange(desc(stat)) %>%
  filter(row_number() %in% 1:30 | municipio %in% c("São Carlos", "Rio Claro", "Ribeirão Preto")) %>%
  mutate(municipio = reorder(municipio, stat)) %>%
  ggplot(aes(x = municipio, y = stat, fill = factor(bin))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("1" = "darkgreen", "0" = "grey"), guide = FALSE) +
  coord_flip() +
  xlab("") + 
  ylab("") + 
  ggtitle("Roubos por 100 mil habitantes em 2015")
```

![](/fig/2016-10-27-Hell_Claro_files/figure-markdown_github/unnamed-chunk-9-1.png)

Homicídios?

``` r
municipios %>%
  filter(ano == 2015) %>%
  mutate(stat = homicidio_doloso/pop*100000, bin = ifelse(municipio %in% c("São Carlos", "Rio Claro", "Ribeirão Preto"), 1, 0)) %>%
  arrange(desc(stat)) %>%
  filter(row_number() %in% 1:30 | municipio %in% c("São Carlos", "Rio Claro", "Ribeirão Preto")) %>%
  mutate(municipio = reorder(municipio, stat)) %>%
  ggplot(aes(x = municipio, y = stat, fill = factor(bin))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("1" = "darkgreen", "0" = "grey"), guide = FALSE) +
  coord_flip() +
  xlab("") + 
  ylab("") + 
  ggtitle("Homicídios por 100 mil habitantes em 2015")
```

![](/fig/2016-10-27-Hell_Claro_files/figure-markdown_github/unnamed-chunk-10-1.png)

Qual a tendência dos crimes em relação à outras cidades de tamanho parecido? Aqui precisei massagear um pouco os dados pra criar uma coluna só com os tipos de crimes e outra com o número de ocorrências:

``` r
municipios$cat_mun <- municipios$municipio
municipios$cat_mun[!municipios$cat_mun %in% c("São Carlos", "Rio Claro")] <- "Outros"
municipios %>%
  gather(key = tipo, value = ocorrencias, homicidio_doloso, furto, roubo, furto_roubo_veiculos) %>%
  filter(pop >= 150000 & pop <= 250000 | municipio %in% c("São Carlos", "Rio Claro")) %>%
  mutate(tipo = gsub("_", " ", tipo), ocorrencias = ocorrencias/pop*100000) %>%
  ggplot(aes(x = ano, y = ocorrencias, color = cat_mun)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) + 
  facet_wrap(~tipo, scale = "free") + 
  scale_color_manual(values = c("São Carlos" = "blue", "Rio Claro" = "darkgreen", "Outros" = "lightgrey")) + 
  ggtitle("Ocorrências em municípios com população entre 150 e 250 mil habitantes") + 
  labs(color = NULL) + 
  ylab("Ocorrências por 100 mil habitantes") + 
  xlab("Ano")
```

![](/fig/2016-10-27-Hell_Claro_files/figure-markdown_github/unnamed-chunk-11-1.png)

Parece que Rio Claro e São Carlos são parecidas entre si e com os outros municípios. Quero saber se o número de roubos em Rio Claro é maior que em São Carlos. Vou assumir que as duas regressões tem a mesma inclinação e rodar a ANCOVA sem a interação:

``` r
mod <- lm(roubo ~ ano + cat_mun, filter(municipios, municipio %in% c("São Carlos", "Rio Claro")))
tidy(mod)
```

    ##                term    estimate    std.error statistic      p.value
    ## 1       (Intercept) -83584.5000 10169.335502 -8.219269 7.962133e-09
    ## 2               ano     42.1375     5.064387  8.320356 6.264690e-09
    ## 3 cat_munSão Carlos   -437.3333    43.761302 -9.993609 1.439450e-10

É, parece que em Rio Claro o número de roubos é maior. Quais cidades estão com maiores tendências de alta e queda?

``` r
# Criando uma lista de data frames, um pra cada município
lista_municipios <- split(municipios, municipios$municipio)

# Regressões
res_municipios <- do.call("rbind", lapply(lista_municipios, function(x) tidy(lm(roubo/pop*100000 ~ ano, x))[2, ]))
res_municipios$municipio <- rownames(res_municipios)
res_municipios <- merge(res_municipios, pop_sp)
```

As tendências:

``` r
res_municipios %>% 
  filter(pop > 100000 & p.value < 0.05 & estimate > 0) %>%
  mutate(bin = ifelse(municipio %in% c("São Carlos", "Rio Claro", "Ribeirão Preto"), 1, 0)) %>%
  arrange(desc(estimate)) %>%
  filter(row_number() %in% 1:30 | municipio %in% c("São Carlos", "Rio Claro", "Ribeirão Preto")) %>%
  mutate(municipio = reorder(municipio, estimate)) %>%
  ggplot(aes(x = municipio, y = estimate, fill = factor(bin))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("1" = "darkgreen", "0" = "grey"), guide = FALSE) +
  coord_flip() +
  xlab("") + 
  ylab("Aumento no número de roubos por ano") + 
  ggtitle("Cidades com maiores tendências de alta do número de roubos")
```

![](/fig/2016-10-27-Hell_Claro_files/figure-markdown_github/unnamed-chunk-14-1.png)

Nenhuma cidade apresentou tendência de queda. Quais cidades são parecidas com Rio Claro de maneira geral?

``` r
dist_mun <- municipios %>%
  filter(pop > 100000 & pop < 300000) %>%
  mutate(roubo = roubo/pop*100000) %>%
  mutate(furto = furto/pop*100000) %>%
  mutate(homicidio_doloso = homicidio_doloso/pop*100000) %>%
  mutate(furto_roubo_veiculos = furto_roubo_veiculos/pop*100000) %>%
  group_by(municipio) %>%
  summarise(roubo = mean(roubo), 
            furto = mean(furto), 
            homicidio_doloso = mean(homicidio_doloso),
            furto_roubo_veiculos = mean(furto_roubo_veiculos))
```

``` r
rownames(dist_mun) <- dist_mun$municipio
clust <- dist_mun %>%
  select(-municipio) %>%
  dist() %>%
  hclust()
par(mar=c(4,4,3,10)) 
plot(as.dendrogram(clust), horiz = TRUE, main = "Municípios com número de habitantes entre 100 e 300 mil")
```

![](/fig/2016-10-27-Hell_Claro_files/figure-markdown_github/unnamed-chunk-16-1.png)

Uma ordenação:

``` r
mun_nmds <- dist_mun %>%
  select(-municipio) %>%
  metaMDS(distance = "euclidean")
plot(mun_nmds, type = "t", cex = 0.5)
```

![](/fig/2016-10-27-Hell_Claro_files/figure-markdown_github/unnamed-chunk-17-1.png)

Barretos parece ter muito mais furtos que os outros municípios de porte parecido. São Caetano do Sul, roubos de veículos. Cidades mais próximas são mais parecidas. Rio Claro parece estar na média. Em uma outra postagem vou mapear os registros de ocorrências em Rio Claro pelos endereços nos BOs.
