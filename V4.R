# 📌 Carregar bibliotecas necessárias
library(chromote)
library(rvest)
library(dplyr)
library(stringr)
library(tidyverse)
library(parsedate)
library(gmailr)
library(knitr)
library(kableExtra)

# 📌 Criar uma nova sessão do Chromote
b <- ChromoteSession$new()

# 📌 Função para capturar vagas no LinkedIn com filtro de período e descrição da vaga
scrape_linkedin_jobs <- function(keyword, pages = 3, periodo = "r5184000") {  # 🔹 Últimos 60 dias (60 dias * 86400 seg)
  base_url <- "https://www.linkedin.com/jobs/search/?"
  query <- paste0("keywords=", URLencode(keyword), 
                  "&location=Brazil", 
                  "&f_TPR=", periodo)  # 🔹 Adicionando filtro de período na URL
  full_url <- paste0(base_url, query)
  
  b$Page$navigate(full_url)
  Sys.sleep(5)  # Tempo para carregar a página
  
  job_list <- list()
  
  for (i in 1:pages) {
    page_source <- b$Runtime$evaluate("document.documentElement.outerHTML")$result$value
    html <- read_html(page_source)
    
    titles <- html %>% html_nodes(".base-search-card__title") %>% html_text(trim = TRUE)
    companies <- html %>% html_nodes(".base-search-card__subtitle") %>% html_text(trim = TRUE)
    locations <- html %>% html_nodes(".job-search-card__location") %>% html_text(trim = TRUE)
    links <- html %>% html_nodes(".base-card__full-link") %>% html_attr("href")
    dates <- html %>% html_nodes(".job-search-card__listdate") %>% html_text(trim = TRUE)
    
    # Converter texto das datas para formato legível
    dates <- dates %>%
      str_replace_all("há ", "") %>%
      str_replace_all("dia", "dias") %>%
      str_replace_all("um", "1") %>%
      str_extract("\\d+") %>%
      as.numeric() %>%
      replace_na(0) %>%
      {Sys.Date() - .}  # 🔹 Subtrai os dias para estimar a data real da postagem
    
    # 🔹 Coletar descrição de cada vaga
    descriptions <- map_chr(links, function(url) {
      if (!is.na(url)) {
        b$Page$navigate(url)
        Sys.sleep(3)  # Esperar carregamento
        page_source <- b$Runtime$evaluate("document.documentElement.outerHTML")$result$value
        job_html <- read_html(page_source)
        
        # Extrair descrição da vaga
        job_html %>%
          html_nodes(".description__text") %>%
          html_text(trim = TRUE) %>%
          paste(collapse = " ")  # Concatenar caso haja múltiplos elementos
      } else {
        NA
      }
    })
    
    # Garantindo que as listas tenham o mesmo comprimento
    max_length <- max(length(titles), length(companies), length(locations), length(links), length(dates), length(descriptions))
    titles <- c(titles, rep(NA, max_length - length(titles)))
    companies <- c(companies, rep(NA, max_length - length(companies)))
    locations <- c(locations, rep(NA, max_length - length(locations)))
    links <- c(links, rep(NA, max_length - length(links)))
    dates <- c(dates, rep(NA, max_length - length(dates)))
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
    
    # Verificar botão de "Próxima Página"
    next_button <- html %>% html_nodes(".artdeco-pagination__button--next")
    if (length(next_button) == 0) break
    
    Sys.sleep(5)  # Pausa antes de carregar a próxima página
  }
  
  return(bind_rows(job_list))
}

# 📌 Lista de palavras-chave para busca
keywords <- c("Data Scientist", "Data Science", "Ciência de Dados", 
              "Data Engineering", "Engenharia de Dados", "Engenheiro de Dados")

# 📌 Coletar vagas para cada palavra-chave (Últimos 60 dias)
vagas_total <- map_dfr(keywords, ~scrape_linkedin_jobs(.x, pages = 3, periodo = "r5184000"))

# 📌 Fechar sessão Chromote
b$close()

# ================================================================
# 🔹 2. Criar coluna Dummy para vagas remotas (1 = remoto, 0 = híbrido/presencial)
# ================================================================

print("🔍 Vagas extraídas (antes da categorização de remoto):")
print(vagas_total)

# 📌 Lista de palavras-chave para identificar trabalho remoto
termos_remoto <- c("remoto", "home office", "teletrabalho", "trabalho remoto", 
                   "anywhere", "remote", "virtual", "trabalho à distância", 
                   "full remote", "trabalho distribuído", "remote work", "work from home", 
                   "fully remote", "remote-first", "remote position", "distributed team", 
                   "work anywhere", "flexible location", "telecommuting", "trabalho de qualquer lugar",
                   "vaga 100% remota", "vaga remota", "trabalho híbrido opcional")

# 📌 Criar colunas auxiliares para análise de remoto
vagas_total <- vagas_total %>%
  mutate(
    local_lower = str_to_lower(Localizacao),
    titulo_lower = str_to_lower(Titulo),
    descricao_lower = str_to_lower(Descricao),
    Remoto = ifelse(
      str_detect(local_lower, paste(termos_remoto, collapse = "|")) | 
        str_detect(titulo_lower, paste(termos_remoto, collapse = "|")) |
        str_detect(descricao_lower, paste(termos_remoto, collapse = "|")), 
      1, 0  # 🔹 1 = remoto, 0 = híbrido/presencial
    )
  )

# 📌 Salvar os resultados em um arquivo CSV
write_csv(vagas_total, "vagas_linkedin.csv")

# 📌 Exibir resultado final
print("✅ Vagas extraídas com classificação de trabalho remoto. Arquivo salvo como 'vagas_linkedin.csv'.")
print(vagas_total)
