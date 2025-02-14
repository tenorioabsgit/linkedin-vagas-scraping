# 📌 Carregar bibliotecas necessárias
library(chromote)
library(rvest)
library(dplyr)
library(stringr)
library(tidyverse)
library(parsedate)

# 📌 Criar uma nova sessão do Chromote
b <- ChromoteSession$new()

# 📌 Função para capturar vagas no LinkedIn (sem restrição de cidade)
scrape_linkedin_jobs <- function(keyword, pages = 15) {
  base_url <- "https://www.linkedin.com/jobs/search/?"
  query <- paste0("keywords=", URLencode(keyword))  # 🔹 Sem cidade
  full_url <- paste0(base_url, query)
  
  b$Page$navigate(full_url)
  Sys.sleep(5)
  
  job_list <- list()
  
  for (i in 1:pages) {
    page_source <- b$Runtime$evaluate("document.documentElement.outerHTML")$result$value
    html <- read_html(page_source)
    
    titles <- html %>% html_nodes(".base-search-card__title") %>% html_text(trim = TRUE)
    companies <- html %>% html_nodes(".base-search-card__subtitle") %>% html_text(trim = TRUE)
    locations <- html %>% html_nodes(".job-search-card__location") %>% html_text(trim = TRUE)
    links <- html %>% html_nodes(".base-card__full-link") %>% html_attr("href")
    
    # Criar dataframe e adicionar à lista
    job_data <- tibble(
      Titulo = titles,
      Empresa = companies,
      Localizacao = locations,
      Link = links
    )
    
    job_list <- append(job_list, list(job_data))
    
    # Verificar botão de "Próxima Página"
    next_button <- html %>% html_nodes(".artdeco-pagination__button--next")
    if (length(next_button) == 0) break
    
    Sys.sleep(5)
  }
  
  return(bind_rows(job_list))  # 🔹 Retorna dataframe consolidado
}

# 📌 Lista de palavras-chave para busca
keywords <- c("Data Scientist", "Data Science", "Ciência de Dados", 
              "Data Engineering", "Engenharia de Dados", "Engenheiro de Dados")

# 📌 Coletar vagas para cada palavra-chave (SEM LOCALIZAÇÃO)
vagas_total <- map_dfr(keywords, ~scrape_linkedin_jobs(.x, pages = 15))

# 📌 Exibir resultados iniciais
print(vagas_total)

# 📌 Fechar sessão Chromote
b$close()

# ================================================================
#  🔹 2. Raspagem de detalhes das vagas (descrição, requisitos, etc.)
# ================================================================

# 📌 Função retry para lidar com falhas na raspagem
retry <- function(expr, max = 3, init = 0) {
  suppressWarnings(tryCatch({
    if (init < max) expr
  }, error = function(e) {
    if (init + 1 < max) {
      Sys.sleep(2)  # Aguarda 2 segundos antes de tentar novamente
      retry(expr, max, init = init + 1)
    } else {
      message("⚠️ Erro após ", max, " tentativas: ", e$message)
      return(NULL)
    }
  }))
}

# 📌 DataFrame final para armazenar detalhes das vagas
base_vagas_linkedin <- tibble()

# 📌 Função para extrair detalhes de cada vaga
extract_job_details <- function(job_link) {
  retry({
    html <- read_html(job_link)
    
    nome_empresa <- html %>% html_element("h4 .topcard__flavor") %>% html_text(trim = TRUE)
    local <- html %>% html_element("h4 .topcard__flavor") %>% html_text(trim = TRUE)
    nome_vaga <- html %>% html_element('h1') %>% html_text(trim = TRUE)
    data_publicacao <- html %>% html_element("h4 .posted-time-ago__text") %>% html_text(trim = TRUE)
    descricao_vaga <- html %>% html_element('.show-more-less-html__markup') %>% html_text(trim = TRUE)
    
    criterios <- html %>% html_elements('.description__job-criteria-text') %>% html_text(trim = TRUE)
    
    # Preenchimento com NA para evitar erros
    nome_empresa <- ifelse(!is.null(nome_empresa), nome_empresa, NA_character_)
    local <- ifelse(!is.null(local), local, NA_character_)
    nome_vaga <- ifelse(!is.null(nome_vaga), nome_vaga, NA_character_)
    data_publicacao <- ifelse(!is.null(data_publicacao), data_publicacao, NA_character_)
    descricao_vaga <- ifelse(!is.null(descricao_vaga), descricao_vaga, NA_character_)
    nivel <- ifelse(length(criterios) >= 1, criterios[1], NA_character_)
    tipo <- ifelse(length(criterios) >= 2, criterios[2], NA_character_)
    funcao <- ifelse(length(criterios) >= 3, criterios[3], NA_character_)
    setor <- ifelse(length(criterios) >= 4, criterios[4], NA_character_)
    
    return(tibble(
      data_publicacao, nome_empresa, local, nome_vaga, descricao_vaga, nivel, tipo, funcao, setor
    ))
  }, max = 3)  # 🔹 Tenta 3 vezes antes de desistir
}

# 📌 Processar cada link de vaga coletado
for (link in vagas_total$Link) {
  df_detalhes <- extract_job_details(link)
  base_vagas_linkedin <- bind_rows(base_vagas_linkedin, df_detalhes)
}

# 📌 Converter datas corretamente
base_vagas_linkedin$data_publicacao <- parse_date(base_vagas_linkedin$data_publicacao, approx = TRUE, default_tz = "UTC") 
base_vagas_linkedin$data_publicacao <- str_sub(base_vagas_linkedin$data_publicacao, start = 1, end = 10)

# 📌 Remover duplicatas
base_vagas_linkedin <- base_vagas_linkedin %>% distinct(descricao_vaga, .keep_all = TRUE)

# 📌 Exibir resultado final
print(base_vagas_linkedin)
