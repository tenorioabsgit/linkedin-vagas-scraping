# 📌 Carregar bibliotecas necessárias
library(chromote)
library(rvest)
library(dplyr)
library(stringr)
library(tidyverse)
library(parsedate)
library(ggplot2)
library(readr)
library(lubridate)
library(crayon)  # Adicionando pacote para cores no console

# 📌 Criar uma nova sessão do Chromote
b <- ChromoteSession$new()

# 📌 Função para capturar vagas no LinkedIn com filtro de período e descrição da vaga
scrape_linkedin_jobs <- function(keyword, pages = 3, periodo = "r5184000") {
  message(blue$bold(paste("Iniciando raspagem para:", keyword)))  # Mensagem colorida de status
  base_url <- "https://www.linkedin.com/jobs/search/?"
  query <- paste0("keywords=", URLencode(keyword), 
                  "&location=Brazil", 
                  "&f_TPR=", periodo)
  full_url <- paste0(base_url, query)
  
  b$Page$navigate(full_url)
  Sys.sleep(5)  # Tempo para carregar a página
  
  job_list <- list()
  
  for (i in 1:pages) {
    message(blue$bold(paste("Processando página", i, "de", pages)))  # Mensagem colorida
    page_source <- b$Runtime$evaluate("document.documentElement.outerHTML")$result$value
    html <- read_html(page_source)
    
    titles <- html %>% html_nodes(".base-search-card__title") %>% html_text(trim = TRUE)
    companies <- html %>% html_nodes(".base-search-card__subtitle") %>% html_text(trim = TRUE)
    locations <- html %>% html_nodes(".job-search-card__location") %>% html_text(trim = TRUE)
    links <- html %>% html_nodes(".base-card__full-link") %>% html_attr("href")
    dates <- html %>% html_nodes(".job-search-card__listdate") %>% html_text(trim = TRUE)
    
    # Garantir que todas as colunas tenham o mesmo tamanho
    max_length <- max(length(titles), length(companies), length(locations), length(links), length(dates))
    titles <- c(titles, rep(NA, max_length - length(titles)))
    companies <- c(companies, rep(NA, max_length - length(companies)))
    locations <- c(locations, rep(NA, max_length - length(locations)))
    links <- c(links, rep(NA, max_length - length(links)))
    dates <- c(dates, rep(NA, max_length - length(dates)))
    
    # Converter texto das datas para formato legível
    dates <- dates %>%
      str_replace_all("há ", "") %>%
      str_replace_all("dia", "dias") %>%
      str_replace_all("um", "1") %>%
      str_extract("\\d+") %>%
      as.numeric() %>%
      replace_na(0) %>%
      {Sys.Date() - .}
    
    # 🔹 Coletar descrição de cada vaga
    descriptions <- map_chr(links, function(url) {
      if (!is.na(url)) {
        message(green$bold(paste("Coletando descrição da vaga em:", url)))  # Mensagem colorida
        b$Page$navigate(url)
        Sys.sleep(3)
        page_source <- b$Runtime$evaluate("document.documentElement.outerHTML")$result$value
        job_html <- read_html(page_source)
        
        job_html %>%
          html_nodes(".description__text") %>%
          html_text(trim = TRUE) %>%
          paste(collapse = " ")
      } else {
        NA
      }
    })
    
    descriptions <- c(descriptions, rep(NA, max_length - length(descriptions)))
    
    # Criar dataframe e adicionar à lista
    job_data <- tibble(
      Titulo = titles,
      Empresa = companies,
      Localizacao = locations,
      Link = links,
      Data_Publicacao = dates,
      Descricao = descriptions
    )
    
    job_list <- append(job_list, list(job_data))
  }
  
  return(bind_rows(job_list))
}

# 📌 Lista de palavras-chave para busca
keywords <- c("Data Scientist", "Data Science", "Ciência de Dados", 
              "Data Engineering", "Engenharia de Dados", "Engenheiro de Dados")

# 📌 Coletar vagas para cada palavra-chave (Últimos 60 dias)
vagas_total <- map_dfr(keywords, ~scrape_linkedin_jobs(.x, pages = 5, periodo = "r5184000"))

# 📌 Salvar os dados coletados
write_csv(vagas_total, "vagas_linkedin.csv")
message(green$bold("✅ Dados salvos em 'vagas_linkedin.csv'"))

# 📌 Fechar sessão Chromote
b$close()

# 📌 Verificar se o arquivo foi salvo corretamente
if (file.exists("vagas_linkedin.csv")) {
  message(green$bold("✅ O arquivo 'vagas_linkedin.csv' foi gerado com sucesso!"))
  data_linkedin <- read_csv("vagas_linkedin.csv")
} else {
  stop(red$bold("❌ ERRO: O arquivo 'vagas_linkedin.csv' não foi encontrado!"))
}
