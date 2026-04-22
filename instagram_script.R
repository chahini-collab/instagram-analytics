library(httr)
library(jsonlite)
library(dplyr)

access_token <- Sys.getenv("ACCESS_TOKEN")

ig_user_id <- "17841405094259143"

base_url <- paste0(
  "https://graph.facebook.com/v25.0/",
  ig_user_id,
  "/media?fields=id,caption,media_type,media_url,timestamp,like_count,comments_count&limit=100&access_token=",
  access_token
)

all_posts <- list()
next_url <- base_url

while (!is.null(next_url)) {

  res <- GET(next_url)
  json <- fromJSON(content(res, "text", encoding = "UTF-8"), flatten = TRUE)

  data_page <- json$data
  if (is.null(data_page)) break

  all_posts <- append(all_posts, list(data_page))

  next_url <- json$paging$`next`
}

df <- bind_rows(all_posts)

# =========================
# 🔥 FUNÇÃO DE INSIGHTS
# =========================

get_insights <- function(media_id, token) {

  url <- paste0(
    "https://graph.facebook.com/v25.0/",
    media_id,
    "/insights?metric=reach,impressions,saved&access_token=",
    token
  )

  res <- GET(url)

  if (status_code(res) != 200) {
    return(data.frame(
      reach = NA,
      impressions = NA,
      saved = NA
    ))
  }

  json <- fromJSON(content(res, "text", encoding = "UTF-8"))

  if (is.null(json$data)) {
    return(data.frame(
      reach = NA,
      impressions = NA,
      saved = NA
    ))
  }

  metrics <- json$data

  reach <- NA
  impressions <- NA
  saved <- NA

  for (m in metrics) {
    if (m$name == "reach") reach <- m$values[[1]]$value
    if (m$name == "impressions") impressions <- m$values[[1]]$value
    if (m$name == "saved") saved <- m$values[[1]]$value
  }

  return(data.frame(
    reach = reach,
    impressions = impressions,
    saved = saved
  ))
}

# =========================
# 🔥 LOOP INSIGHTS
# =========================

insights_list <- lapply(df$id, get_insights, token = access_token)

insights_df <- bind_rows(insights_list)

df <- bind_cols(df, insights_df)

# =========================
# LIMPEZA
# =========================

df$caption <- ifelse(is.na(df$caption), "", df$caption)
df$caption <- gsub("\n", " ", df$caption)
df$caption <- gsub("\r", " ", df$caption)
df$caption <- gsub("\"", "'", df$caption)

# =========================
# EXPORT
# =========================

write.csv(
  df,
  "instagram_posts.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8",
  sep = ";"
)

cat("Finalizado com sucesso!\n")
