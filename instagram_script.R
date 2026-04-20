library(httr)
library(jsonlite)
library(dplyr)

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
  content_res <- content(res, as = "text", encoding = "UTF-8")
  json <- fromJSON(content_res, flatten = TRUE)

  cat("Status:", status_code(res), "\n")

  data_page <- json$instagram_business_account$media$data

  if (is.null(data_page)) break

  all_data <- append(all_data, list(data_page))

  next_url <- json$instagram_business_account$media$paging$`next`
}

# Junta tudo
df <- bind_rows(all_data)

cat("Total de posts:", nrow(df), "\n")

# =========================
# LIMPEZA
# =========================

df <- df %>%
  mutate(
    caption = ifelse(is.na(caption), "", caption),
    caption = gsub("\n", " ", caption),
    caption = gsub("\r", " ", caption),
    caption = gsub("\"", "'", caption),
    caption = as.character(caption),

    like_count = as.numeric(like_count),
    comments_count = as.numeric(comments_count),

    timestamp = as.POSIXct(timestamp, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
  )

# =========================
# EXPORT (AGORA COMPLETO)
# =========================

write.csv(
  df %>%
    select(
      id,
      caption,
      media_type,
      media_url,
      timestamp,
      like_count,
      comments_count
    ),
  "instagram_posts.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8",
  quote = TRUE
)

cat("Finalizado com sucesso!\n")
