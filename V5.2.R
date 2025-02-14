# 📌 Carregar bibliotecas necessárias
library(dplyr)
library(stringr)
library(ggplot2)
library(readr)
library(tidyr)

# 📌 Carregar os dados do CSV
df <- read_csv("vagas_linkedin.csv")

# 📌 Lista de ferramentas para buscar nas descrições
ferramentas <- c(
  "Python", "Linguagem R", "R Language", "SQL", "Scala", "Java", "Julia", "C++", "C#", "Go", "Rust",
  "Pandas", "NumPy", "Scikit-Learn", "TensorFlow", "PyTorch", "Keras",
  "Matplotlib", "Seaborn", "NLTK", "OpenCV", "XGBoost", "LightGBM",
  "Hugging Face", "FastAPI", "Streamlit", "Plotly", "DVC", "MLflow",
  "AWS", "Azure", "Google Cloud", "GCP", "BigQuery", "Snowflake", "Databricks",
  "Oracle Cloud", "IBM Cloud", "Alibaba Cloud", "DigitalOcean", "Linode", "Heroku",
  "Firebase", "Cloud Functions", "Lambda", "Cloud Run", "EC2", "S3", "IAM",
  "Redshift", "Cloud SQL", "Cloud Storage", "Vertex AI", "SageMaker", "Dataflow",
  "Glue", "Cloud Composer",
  "Power BI", "Tableau", "Looker", "Google Data Studio", "Qlik Sense", "QlikView",
  "Metabase", "Superset", "Grafana", "Sisense", "Domo", "Mode Analytics"
)

# 📌 Criar uma tabela para armazenar as contagens das ferramentas
df_ferramentas <- tibble(Ferramenta = ferramentas, Quantidade = 0)

# 📌 Remover NAs da coluna "Descricao"
df <- df %>% mutate(Descricao = ifelse(is.na(Descricao), "", tolower(Descricao)))

# 📌 Aplicar Regex para contar menções das ferramentas na coluna Descrição
for (ferramenta in ferramentas) {
  df_ferramentas <- df_ferramentas %>%
    mutate(Quantidade = ifelse(Ferramenta == ferramenta,
                               sum(str_detect(df$Descricao, regex(ferramenta, ignore_case = TRUE))),
                               Quantidade))
}

# 📌 Selecionar as 15 ferramentas mais mencionadas
df_top15 <- df_ferramentas %>%
  arrange(desc(Quantidade)) %>%
  head(15)

# 📌 Criar um gráfico de barras com as 15 ferramentas mais mencionadas
ggplot(df_top15, aes(x = reorder(Ferramenta, Quantidade), y = Quantidade)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 15 Ferramentas mais Requisitadas em Vagas de Data Science",
       x = "Ferramenta",
       y = "Número de Menções") +
  theme_minimal()
