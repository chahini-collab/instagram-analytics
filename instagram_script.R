library(httr)
library(jsonlite)
library(openxlsx)

access_token <- Sys.getenv("ACCESS_TOKEN")
ig_id <- "17841411701744440"

url <- paste0(
  "https://graph.facebook.com/v25.0/",
  ig_id,
  "/media?fields=id,caption,media_type,media_url,timestamp,like_count,comments_count&access_token=",
  access_token
)

# Requisição
response <- GET(url)

# Verifica status HTTP
if (status_code(response) != 200) {
  stop(paste("Erro HTTP:", status_code(response)))
}

# Converte resposta
data <- content(response, "text", encoding = "UTF-8")
json_data <- fromJSON(data, flatten = TRUE)

# Verifica erro da API
if (!is.null(json_data$error)) {
  stop(paste("Erro da API:", json_data$error$message))
}

# Verifica se há dados
if (is.null(json_data$data) || length(json_data$data) == 0) {
  stop("Nenhum dado retornado pela API.")
}

# Dataframe
df <- json_data$data

# Exporta Excel
write.xlsx(df, "instagram_posts.xlsx", overwrite = TRUE)

print("Arquivo gerado com sucesso!")
