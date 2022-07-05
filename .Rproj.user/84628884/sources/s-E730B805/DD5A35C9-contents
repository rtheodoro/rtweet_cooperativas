
# -------------------------------------------------------------------------#
#              Principal
#
# Pacotes utilizados
#
# library(rtweet)
# library(tidyverse)
# library(tidytext)
#
# -------------------------------------------------------------------------#


# Buscar os dados sobre os tuítes que citam cooperativas. Dados coletados em 05/07/2022
# Iremos buscar as palavras cooperativas, cooperativa, cooperativimos e coop

tw_coop <- rtweet::search_tweets(
   q = c("cooperativismo"), 
   n = 20000, 
   include_rts = FALSE)

tw_coop |>
   readr::write_csv("dados_salvos/tw_coop.csv")


# Visualizando os dados
tw_coop |>
   dplyr::mutate(data_publicacao = lubridate::as_date(created_at)) |>
   ggplot2::ggplot() +
   ggplot2::geom_bar(ggplot2::aes(x = data_publicacao)) +
   ggplot2::theme_light() +
   ggplot2::labs(
      x = NULL,
      y = "Número de tweets"
   )



# Carregar funcoes criadas pelo Fernando Barbalho. Abrir arquivo pra ver mais
source("funcoes.R")

# Funções para buscar as hashtags distintas. 
tw_coop <- analise_hashtags(tw_coop)

tw_coop |>
  dplyr::slice_max(order_by = quantidade, n = 20) |>
  dplyr::mutate(hashtag = reorder(hashtag, quantidade)) |>
  ggplot2::ggplot() +
  ggplot2::geom_col(ggplot2::aes(y = hashtag, x = quantidade)) +
  ggplot2::theme_light() +
  ggplot2::labs(
    x = "Número de tweets"
  )


tidy_twitter <-
  tw_coop |>
  tidytext::unnest_tokens(word, text, token = "tweets", strip_punct = TRUE, strip_url = TRUE) %>%
  # filter(str_detect(word, "@", negate = TRUE)) %>%
  dplyr::group_by(word) |>
  dplyr::filter(n() > 10) |>
  dplyr::ungroup()