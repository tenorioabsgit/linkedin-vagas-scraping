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
library(jsonlite)  # Adicionando biblioteca para manipulação de JSON

# 📌 Criar uma nova sessão do Chromote
b <- ChromoteSession$new()

# 📌 Função para capturar vagas no LinkedIn
scrape_linkedin_jobs <- function(keyword, pages = 3) {
  base_url <- "https://www.linkedin.com/jobs/search/?"
  query <- paste0("keywords=", URLencode(keyword), "&location=Brazil")  # 🔹 Busca no Brasil
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
    links <- html %>% html_nodes(".base-card__full-link") %>% html_attr("href")  # ✅ Pegando links corretamente
    
    # Garantindo que as listas tenham o mesmo comprimento
    max_length <- max(length(titles), length(companies), length(locations), length(links))
    titles <- c(titles, rep(NA, max_length - length(titles)))
    companies <- c(companies, rep(NA, max_length - length(companies)))
    locations <- c(locations, rep(NA, max_length - length(locations)))
    links <- c(links, rep(NA, max_length - length(links)))
    
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
  
  return(bind_rows(job_list))
}

# 📌 Lista de palavras-chave para busca
keywords <- c("Data Scientist", "Data Science", "Ciência de Dados", 
              "Data Engineering", "Engenharia de Dados", "Engenheiro de Dados")

# 📌 Coletar vagas para cada palavra-chave (Apenas no Brasil)
vagas_total <- map_dfr(keywords, ~scrape_linkedin_jobs(.x, pages = 3))

# 📌 Fechar sessão Chromote
b$close()

# 📌 Criar colunas auxiliares para filtragem
vagas_total <- vagas_total %>%
  mutate(
    local_lower = str_to_lower(Localizacao),
    titulo_lower = str_to_lower(Titulo)
  )

# 📌 Lista de palavras-chave para trabalho remoto
termos_remoto <- c("remoto", "home office", "remote", "virtual", "trabalho à distância", 
                   "full remote", "remote work", "work from home", "trabalho de qualquer lugar",
                   "vaga 100% remota", "vaga remota", "trabalho híbrido opcional")

# 📌 Filtrar apenas vagas 100% remotas
df_final <- vagas_total %>%
  filter(
    str_detect(local_lower, paste(termos_remoto, collapse = "|")) |
      str_detect(titulo_lower, paste(termos_remoto, collapse = "|"))
  )

# 📌 Se `df_final` estiver vazio, usar todas as vagas
if (nrow(df_final) == 0) {
  print("⚠️ Nenhuma vaga remota encontrada! Enviando todas as vagas extraídas...")
  df_final <- vagas_total
}

# 📌 Salvar o dataframe em formato JSON
json_path <- "vagas_remotas.json"
write_json(df_final, json_path, pretty = TRUE)
print(paste("✅ JSON salvo em:", json_path))

# ================================================================
# 🔹 3. Envio de E-mail com os Links do LinkedIn
# ================================================================
print("🔍 Enviando e-mail com as seguintes vagas:")
print(df_final)

# 📌 Criar e-mail
email <- gm_mime() %>%
  gm_to("tenorioabs@gmail.com") %>%
  gm_from("tenorioabs@gmail.com") %>%
  gm_subject("Relatório de Vagas Remotas do LinkedIn") %>%
  gm_attach(json_path, type = "application/json")  # Anexando JSON ao e-mail

# 📌 Enviar e-mail
tryCatch({
  gm_send_message(email)
  print("✅ E-mail enviado com sucesso!")
}, error = function(e) {
  print(paste("⚠️ Erro ao enviar e-mail:", e$message))
})