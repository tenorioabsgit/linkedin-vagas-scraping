

#devtools::install_github("r-dbi/bigrquery")

httr::set_config(httr::config(ssl_verifypeer = 0L))
httr::set_config(httr::config(http_version = 0))
options(httr_oob_default = TRUE)

library(bigrquery)

bq_auth(email = "jose.abs@ibpad.com.br", path = "service_account_bq.json")

billing <- "meu-primeiro-projeto-197214"

library(DBI)

con <- dbConnect(
  bigrquery::bigquery(),
  project = "meu-primeiro-projeto-197214",
  dataset = "linkedin_vagas",
  billing = billing
)
con

tablename <- "linkedin_vagas_web_scraping"

## Extração

sql <- "SELECT * FROM meu-primeiro-projeto-197214.linkedin_vagas.linkedin_vagas_web_scraping"
print(sql)

download_bigquery <- dbGetQuery(con, sql)

