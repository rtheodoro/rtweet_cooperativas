# Criado por Fernando Barbalho
# Ver: https://github.com/fernandobarbalho/datafolha_tweets/blob/main/funcoes.R


# Função que gera um dataframe com as hashtags associadas aos twittes onde aparecem os candidatos
analise_hashtags <- function(.data, n = NULL) {
  library(purrr)

  hashtags <- .data$hashtags

  keywords <-
    map_dfr(1:length(hashtags), function(inc) {

      # print(inc)
      if (!is.na(hashtags[[inc]])) {
        # print(hashtags[[inc]])
        map_dfr(1:NROW(hashtags[[inc]]), function(inc_int) {
          # print(hashtags[[inc]][inc_int])
          tibble::tibble(hashtag = hashtags[[inc]][inc_int])
        })
      }
    })

  analise_keywords <-
    keywords |>
    dplyr::mutate(hashtag = stringr::str_to_lower(hashtag)) |>
    dplyr::group_by(hashtag) |>
    dplyr::summarise(
      quantidade = n()
    )

  if (!is.null(n)) {
    analise_keywords <-
      analise_keywords %>%
      dplyr::slice_max(quantidade, n = n)
  }

  .data <- analise_keywords

  .data
}


filter_hashtags <- function(.data, filtro) {
  library(purrr)
  library(abjutils)
  filtro <- abjutils::rm_accent(stringr::str_to_lower(filtro))


  hashtags <- .data$hashtags

  purrr::map_dfr(1:length(hashtags), function(inc) {
    if (!is.na(hashtags[[inc]])) {
      purrr::map_dfr(1:NROW(hashtags[[inc]]), function(inc_int) {
        # print(hashtags[[inc]][inc_int])

        if (abjutils::rm_accent(stringr::str_to_lower(hashtags[[inc]][inc_int])) == filtro) {
          .data[inc, ]
        }
      })
    }
  })
}


# associação de mensagens positivas, negativas e neutras aos candidatos a uma amostra das mensagens

associa_tipo_mensagem <- function(.data, n, tags_positivas, tags_negativas, usuarios_noticias = NULL, tipo = 1, nome_arquivo = NULL, delim = ",", seed = 1972) {
  .data <-
    .data %>%
    dplyr::mutate(tipo_mensagem = dplyr::case_when(
      tolower(hashtags) %in% tags_positivas ~ "positivo",
      tolower(hashtags) %in% tags_negativas ~ "negativo",
      screen_name %in% usuarios_noticias ~ "noticia"
    )) %>%
    dplyr::select(status_id, screen_name, text, tipo_mensagem, hashtags)

  set.seed(seed)

  if (tipo == 1) {
    .data <-
      .data %>%
      dplyr::slice_sample(n = n)

    # seleciona uma amostra das mensagens e sugere as primeiras definições do tipo de mesnagem a partir das hashtags e usuários
  } else {

    # seleciona uma amostra das mensagens e sugere as primeiras definições do tipo de mesnagem a partir das hashtags e usuários
    .data <-
      .data %>%
      dplyr::filter(!is.na(tipo_mensagem)) %>%
      dplyr::slice_sample(n = n)
  }


  if (!is.null(nome_arquivo)) {
    .data %>%
      readr::write_delim(nome_arquivo, delim = delim, )
  }

  .data
}

################# Processamento de tweets

processa_tweets <- function(.data) {
  .data <-
    .data %>%
    dplyr::select(status_id, screen_name, tipo_mensagem, text) %>%
    tidytext::unnest_tweets(word, text, strip_url = TRUE) %>%
    dplyr::filter(
      !stringr::str_detect(word, "#"),
      !stringr::str_detect(word, "@"),
      !stringr::str_detect(word, "[^[:graph:][:space:]]")
    )

  .data$word <- abjutils::rm_accent(.data$word)

  palavras_unicas_usuario <-
    .data %>%
    dplyr::group_by(screen_name, word) %>%
    dplyr::summarise(
      quantidade = dplyr::n()
    ) %>%
    dplyr::filter(quantidade == 1) %>%
    dplyr::select(screen_name, word) %>%
    dplyr::ungroup()

  .data <-
    .data %>%
    dplyr::inner_join(palavras_unicas_usuario)

  library(stopwords)
  library(tibble)

  stopword <- tibble::as_tibble(stopwords::stopwords("pt"))
  stopword <- dplyr::rename(stopword, word = value)
  .data <- dplyr::anti_join(.data, stopword, by = "word")

  .data
}


########### Term frequency (tf)

frequencia_termos <- function(.data) {
  word_count <- dplyr::count(.data, word, sort = TRUE)

  tipo_count <- .data %>%
    dplyr::count(tipo_mensagem, word, sort = TRUE)

  .data <- tipo_count
  .data
}



########## Term frequency and inverse document frequency (tf-idf)

tabela_tf_idf <- function(.data) {
  library(forcats)

  tabela <- .data %>%
    dplyr::count(tipo_mensagem, word, sort = TRUE) %>%
    tidytext::bind_tf_idf(word, tipo_mensagem, n) %>%
    dplyr::mutate(word = forcats::fct_reorder(word, tf_idf)) %>%
    dplyr::mutate(tipo_mensagem = factor(tipo_mensagem,
      levels = c(
        "negativo",
        "positivo",
        "noticias"
      )
    ))

  graph <-
    tabela %>%
    dplyr::group_by(tipo_mensagem) %>%
    dplyr::top_n(15, tf_idf) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(word = reorder(word, tf_idf)) %>%
    ggplot2::ggplot(ggplot2::aes(word, tf_idf, fill = tipo_mensagem)) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::facet_wrap(~tipo_mensagem, ncol = 3, scales = "free") +
    ggplot2::coord_flip() +
    ggplot2::theme_classic(base_size = 12) +
    ggplot2::labs(
      fill = "Tipo Mensagem",
      x = NULL,
      y = NULL
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(lineheight = .8, face = "bold"),
      axis.title = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank()
    ) +
    ggplot2::scale_fill_viridis(discrete = TRUE)

  list(tabela = tabela, graph = graph)
}


### Preparação das base de ML
prepara_base_ml <- function(.data) {
  library(caret)
  library(tidyr)

  pml_train_trabalho <-
    .data %>%
    dplyr::group_by(status_id, tipo_mensagem, word) %>%
    dplyr::summarise(
      quantidade = dplyr::n()
    ) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider(names_from = word, values_from = quantidade)


  pml_train_trabalho[is.na(pml_train_trabalho)] <- 0

  library(janitor)

  pml_train_model <- pml_train_trabalho[, -1]

  status_id_model <- pml_train_trabalho$status_id

  colnames(pml_train_model) <- make.names(colnames(pml_train_model))
  # pml_train_model<- janitor::clean_names(pml_train_model)

  pml_train_model <- pml_train_model[, substr(names(pml_train_model), 1, 2) != "X."]

  .data <- list(status_id_model = status_id_model, pml_train_model = pml_train_model)
}


########### decision tree para todos os elementos
cria_arvore_decisao <- function(.data, seed = 1972) {
  if (class(.data) == "list") {
    .data <- .data$pml_train_model
  }

  control_dt <- caret::trainControl(method = "cv")

  set.seed(seed)

  dt_model <- caret::train(tipo_mensagem ~ ., data = .data, method = "rpart", trControl = control_dt)

  dt_model
}


########### gráfico da decision tree para todos os elementos
grafico_arvore_decisao <- function(.data) {
  library(rattle)
  rattle::fancyRpartPlot(.data$finalModel)
}
