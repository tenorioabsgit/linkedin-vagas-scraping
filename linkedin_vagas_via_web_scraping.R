library(rvest)
library(tidyverse)
library(googlesheets4)

options(warn = -1)

retry <- function(a, max = Inf, init = 0){suppressWarnings( tryCatch({
  if(init<max) a
}, error = function(e){retry(a, max, init = init+1)}))}

googlesheets4::gs4_auth("tenorioabs@gmail.com")

base_input_output <- "https://docs.google.com/spreadsheets/d/1DUoEB1KZIW__NkpvKZU88da788bC1FIOHgV7Q8rfxO8/edit#gid=0"
base_links_raspagens <- googlesheets4::read_sheet(ss = base_input_output, sheet = "links")

base_vagas_linkedin <- data.frame() 
for (i in 1:nrow(base_links_raspagens)) {
  retry(lista_vagas <- read_html(base_links_raspagens$links_vagas[i]) %>% html_elements("ul") %>% html_elements("li")%>% html_elements("a") %>% html_attr('href'))
  lista_vagas <- grep(pattern = 'https://br.linkedin.com/jobs/', x = lista_vagas, value = TRUE)
  area <- base_links_raspagens$area[i]
    for (x in 1:length(lista_vagas)) {
      retry(a = (html <- read_html(lista_vagas[x])))
      nome_empresa <- (html %>% html_elements("h4") %>% html_elements('.topcard__flavor') %>% html_text2())[1]
      local <- (html %>% html_elements("h4") %>% html_elements('.topcard__flavor') %>% html_text2())[2]
      nome_vaga <- html %>% html_elements('h1') %>% html_text2()
      data_publicacao <- (html %>% html_elements("h4") %>% html_elements('.posted-time-ago__text') %>% html_text2())
      descricao_vaga <- html %>% html_elements('.show-more-less-html__markup') %>% html_text()
      descricao_vaga <- str_remove(string = descricao_vaga, pattern = '\n    Job Description')
      descricao_vaga <- str_remove(string = descricao_vaga, pattern = '\n   ')
      descricao_vaga <- str_remove(string = descricao_vaga, pattern = '     ')
      nivel <- (html %>% html_elements('.description__job-criteria-text') %>% html_text2())[1]
      tipo <- (html %>% html_elements('.description__job-criteria-text') %>% html_text2())[2]
      funcao <- (html %>% html_elements('.description__job-criteria-text') %>% html_text2())[3]
      setor <- (html %>% html_elements('.description__job-criteria-text') %>% html_text2())[4]
      df_passagem <- data.frame(data_publicacao, area, nome_empresa, local, descricao_vaga, nivel, tipo, funcao, setor)
      base_vagas_linkedin <- rbind(base_vagas_linkedin, df_passagem)
      #Sys.sleep(3)
 }
}

base_vagas_linkedin$data_publicacao <- parsedate::parse_date(base_vagas_linkedin$data_publicacao, approx = TRUE, default_tz = "UTC") 
base_vagas_linkedin$data_publicacao <- str_sub(string = base_vagas_linkedin$data_publicacao, start = 1, end = 10)

# base_vagas_linkedin$area <- case_when(
#   str_detect(string = base_vagas_linkedin$area, "Cientista de Dados") == T ~ "Ciência de Dados",
#   str_detect(string = base_vagas_linkedin$area, "Ciencia de Dados") == T ~ "Ciência de Dados",
#   str_detect(string = base_vagas_linkedin$area, "Data Science") == T ~ "Ciência de Dados",
#   str_detect(string = base_vagas_linkedin$area, "Engenheiro de Dados") == T ~ "Engenharia de Dados",
#   str_detect(string = base_vagas_linkedin$area, "Engenharia de Dados") == T ~ "Engenharia de Dados",
#   str_detect(string = base_vagas_linkedin$area, "Data Engineer") == T ~ "Engenharia de Dados"
#   
# )

base_vagas_linkedin <- base_vagas_linkedin %>% distinct(descricao_vaga, .keep_all = T)

######## Upload file to Big Query #############

#devtools::install_github("r-dbi/bigrquery")

httr::set_config(httr::config(ssl_verifypeer = 0L))
httr::set_config(httr::config(http_version = 0))
options(httr_oob_default = TRUE)

library(bigrquery)

bq_auth(email = "tenorioabs@gmail.com")

billing <- "algoritimusproject"

library(DBI)

con <- dbConnect(
  bigrquery::bigquery(),
  project = "algoritimusproject",
  dataset = "vagaslinkedin",
  billing = billing
)
con

tablename <- "vagaslinkedin_database"


bigrquery::dbWriteTable(con, tablename, base_vagas_linkedin, append=F, verbose = T, row.names=F, overwrite=T, fields=base_vagas_linkedin)
print("Dataframe vagaslinkedin_database Uploaded")

dbDisconnect(con)

print(paste0("Data e horário do fim: ",Sys.time()))

