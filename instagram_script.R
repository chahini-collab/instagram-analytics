library(httr)
library(jsonlite)

access_token <- Sys.getenv("ACCESS_TOKEN")
ig_id <- "17841411701744440"

base_url <- paste0(
  "https://graph.facebook.com/v25.0/",
  ig_id,
  "/media?fields=id,caption,media_type,media_url,timestamp,like_count,comments_count&limit=50&access_token=",
  access_token
)

all_data <- list()
next_url <- base_url

while (!is.null(next_url)) {
  
  response <- GET(next_url)
  
  if (status_code(response) != 200) {
    stop(paste("Erro HTTP:", status_code(response)))
  }
  
  data <- content(response, "text", encoding = "UTF-8")
  json_data <- fromJSON(data, flatten = TRUE)
  
  if (!is.null(json_data$error)) {
    stop(paste("Erro da API:", json_data$error$message))
  }
  
  all_data <- append(all_data, json_data$data)
  
  # próxima página
  if (!is.null(json_data$paging$`next`)) {
    next_url <- json_data$paging$`next`
  } else {
    next_url <- NULL
  }
}

df <- do.call(rbind, all_data)

# 🔥 EXPORTA CSV (CORRETO PRO POWER BI)
write.csv(df, "instagram_posts.csv", row.names = FALSE, fileEncoding = "UTF-8")

print("Arquivo CSV atualizado com paginação completa!")
