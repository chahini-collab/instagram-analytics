library(httr)
library(jsonlite)

access_token <- Sys.getenv("ACCESS_TOKEN")

url <- paste0(
  "https://graph.facebook.com/v19.0/",
  "111286197030443",
  "?fields=instagram_business_account{media.limit(100){id,caption,media_type,media_url,timestamp,like_count,comments_count}}",
  "&access_token=", access_token
)

all_data <- list()
next_url <- url

while (!is.null(next_url)) {
  
  res <- GET(next_url)
  content <- content(res, as = "text", encoding = "UTF-8")
  json <- fromJSON(content, flatten = TRUE)

  cat("Status:", status_code(res), "\n")

  # 🔥 CAMINHO CORRETO DOS DADOS
  data_page <- json$instagram_business_account$media$data

  if (is.null(data_page)) break

  all_data <- append(all_data, list(data_page))

  # Paginação
  next_url <- json$instagram_business_account$media$paging$`next`
}

# Junta tudo
df <- do.call(rbind, all_data)

# 🔥 GARANTE QUE É DATAFRAME
df <- as.data.frame(df)

cat("Total de posts:", nrow(df), "\n")

# =========================
# LIMPEZA (CRÍTICA)
# =========================
df$caption <- ifelse(is.na(df$caption), "", df$caption)

df$caption <- gsub("\n", " ", df$caption)
df$caption <- gsub("\r", " ", df$caption)
df$caption <- gsub("\"", "'", df$caption)

df$caption <- as.character(df$caption)

# =========================
# EXPORT
# =========================
write.csv(
  df[, c("caption", "timestamp")],
  "instagram_posts.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8",
  quote = TRUE
)

cat("Finalizado com sucesso!\n")
