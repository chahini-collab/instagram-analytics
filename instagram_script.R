library(httr)
library(jsonlite)
library(dplyr)

access_token <- Sys.getenv("ACCESS_TOKEN")
ig_user_id <- "111286197030443"

# =========================
# 1. BUSCAR POSTS
# =========================

base_url <- paste0(
  "https://graph.facebook.com/v19.0/",
  ig_user_id,
  "?fields=instagram_business_account{media.limit(100){id,caption,media_type,media_url,timestamp,like_count,comments_count}}",
  "&access_token=", access_token
)

all_posts <- list()
next_url <- base_url

while (!is.null(next_url)) {
  
  res <- GET(next_url)
  json <- fromJSON(content(res, "text", encoding = "UTF-8"), flatten = TRUE)

  data_page <- json$instagram_business_account$media$data
  if (is.null(data_page)) break

  all_posts <- append(all_posts, list(data_page))
  next_url <- json$instagram_business_account$media$paging$`next`
}

df <- bind_rows(all_posts)

cat("Total de posts:", nrow(df), "\n")

# =========================
# 2. FUNÇÃO PARA INSIGHTS
# =========================

get_insights <- function(media_id) {
  
  url <- paste0(
    "https://graph.facebook.com/v19.0/",
    media_id,
    "/insights?metric=reach,impressions,saved&access_token=",
    access_token
  )
  
  res <- GET(url)
  json <- fromJSON(content(res, "text", encoding = "UTF-8"), flatten = TRUE)
  
  metrics <- json$data
  
  out <- list(
    reach = NA,
    impressions = NA,
    saved = NA
  )
  
  if (!is.null(metrics)) {
    for (i in 1:length(metrics$name)) {
      name <- metrics$name[i]
      value <- metrics$values[[i]]$value
      
      if (name == "reach") out$reach <- value
      if (name == "impressions") out$impressions <- value
      if (name == "saved") out$saved <- value
    }
  }
  
  return(out)
}

# =========================
# 3. COLETAR INSIGHTS
# =========================

insights <- lapply(df$id, get_insights)
insights_df <- bind_rows(insights)

df <- bind_cols(df, insights_df)

# =========================
# 4. FOLLOWERS (CONTA)
# =========================

followers_url <- paste0(
  "https://graph.facebook.com/v19.0/",
  ig_user_id,
  "?fields=followers_count&access_token=",
  access_token
)

res_followers <- GET(followers_url)
followers_json <- fromJSON(content(res_followers, "text", encoding = "UTF-8"))

followers_count <- followers_json$followers_count

# adiciona em todas as linhas
df$followers <- followers_count

# =========================
# 5. LIMPEZA
# =========================

df <- df %>%
  mutate(
    caption = ifelse(is.na(caption), "", caption),
    caption = gsub("\n", " ", caption),
    caption = gsub("\r", " ", caption),
    caption = gsub("\"", "'", caption),
    timestamp = as.POSIXct(timestamp, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC"),
    like_count = as.numeric(like_count),
    comments_count = as.numeric(comments_count),
    reach = as.numeric(reach),
    impressions = as.numeric(impressions),
    saved = as.numeric(saved),
    followers = as.numeric(followers)
  )

# =========================
# 6. EXPORT
# =========================

write.table(
  df,
  "instagram_posts.csv",
  sep = ";",
  row.names = FALSE,
  fileEncoding = "UTF-8",
  quote = TRUE
)

cat("Finalizado com sucesso!\n")
