# 📌 Projeto de Raspagem de Vagas no LinkedIn

## 📖 Visão Geral
Este projeto é um conjunto de scripts em R para realizar a raspagem de dados de vagas no LinkedIn. Ele coleta, processa e armazena informações sobre oportunidades de emprego na área de **Data Science** e afins, além de gerar relatórios automatizados e gráficos de tendências.

## 🛠 Tecnologias Utilizadas
- **R**
- **Chromote** (Automação do Navegador)
- **rvest** (Web Scraping)
- **tidyverse** (Manipulação de Dados)
- **gmailr** (Envio de E-mails)
- **jsonlite** (Exportação de Dados)
- **ggplot2** (Visualização de Dados)

## 📂 Estrutura do Projeto
```
📦 projeto-linkedin-jobs
├── 📄 V0.R            # Primeira versão da raspagem (sem localização)
├── 📄 V1.R            # Adiciona busca específica no Brasil
├── 📄 V2.R            # Inclui filtro de vagas 100% remotas
├── 📄 V3_json.R       # Exportação dos dados em formato JSON
├── 📄 V4.R            # Adiciona filtro de período e coleta de descrições
├── 📄 V5.1.R          # Mensagens coloridas e otimizações de execução
├── 📄 V5.2.R          # Análise estatística das tecnologias mais requisitadas
├── 📄 README.md       # Documentação do projeto
```

## 🚀 Funcionalidades Principais
- **Coleta de Vagas**: Raspagem automática de vagas relacionadas a Data Science.
- **Filtragem Inteligente**: Identificação de vagas 100% remotas.
- **Extração de Descrição**: Captura da descrição detalhada das vagas.
- **Geração de Relatórios**: Exportação dos resultados em JSON e CSV.
- **Envio Automático por E-mail**: Relatório diário enviado por e-mail.
- **Análise de Tecnologias**: Identificação das ferramentas mais demandadas no mercado.

## 📌 Como Executar
1. **Instale as dependências**:
   ```r
   install.packages(c("chromote", "rvest", "dplyr", "stringr", "tidyverse", "parsedate", "gmailr", "knitr", "kableExtra", "jsonlite", "ggplot2", "readr", "lubridate", "crayon"))
   ```
2. **Configure o Gmail API** (para envio de e-mails, opcional).
3. **Execute um dos scripts**:
   ```r
   source("V5.2.R")
   ```
4. **Visualize os resultados** no arquivo gerado `vagas_linkedin.csv` ou `vagas_remotas.json`.

## 📊 Exemplo de Output
Um gráfico das ferramentas mais mencionadas nas vagas:

![Top Ferramentas](https://raw.githubusercontent.com/seu-repositorio/top-ferramentas.png)

## 📌 Próximos Passos
- Melhorar a performance da raspagem.
- Adicionar suporte a mais países e regiões.
- Implementar Machine Learning para classificar vagas automaticamente.


