library(httr)
library(jsonlite)

access_token <- Sys.getenv("ACCESS_TOKEN")

ig_user_id <- "17841405094259143"

# =========================
# GET POSTS
# =========================
url <- paste0(
  "https://graph.facebook.com/v19.0/",
  ig_user_id,
  "?fields=media.limit(100){id,caption,media_type,media_url,timestamp,like_count,comments_count}",
  "&access_token=", access_token
)

all_data <- list()
next_url <- url

while (!is.null(next_url)) {
  
  res <- GET(next_url)
  json <- fromJSON(content(res, "text", encoding = "UTF-8"), flatten = TRUE)

  data_page <- json$media$data

  if (is.null(data_page)) break

  all_data <- append(all_data, list(data_page))

  next_url <- json$media$paging$`next`
}

df <- do.call(rbind, all_data)
df <- as.data.frame(df)

# =========================
# GET INSIGHTS POR POST
# =========================
df$reach <- NA
df$impressions <- NA
df$saved <- NA

for (i in 1:nrow(df)) {

  media_id <- df$id[i]

  insights_url <- paste0(
    "https://graph.facebook.com/v19.0/",
    media_id,
    "/insights?metric=reach,impressions,saved",
    "&access_token=", access_token
  )

  res <- GET(insights_url)
  json <- fromJSON(content(res, "text", encoding = "UTF-8"), flatten = TRUE)

  if (!is.null(json$data)) {

    for (j in 1:length(json$data$name)) {

      metric_name <- json$data$name[j]
      value <- json$data$values[[j]]$value[1]

      if (metric_name == "reach") df$reach[i] <- value
      if (metric_name == "impressions") df$impressions[i] <- value
      if (metric_name == "saved") df$saved[i] <- value
    }
  }

  Sys.sleep(0.3)
}

# =========================
# GET FOLLOWERS (FORMA ROBUSTA)
# =========================
followers_url <- paste0(
  "https://graph.facebook.com/v19.0/me",
  "?fields=accounts{instagram_business_account{followers_count}}",
  "&access_token=", access_token
)

res <- GET(followers_url)
json <- fromJSON(content(res, "text", encoding = "UTF-8"), flatten = TRUE)

followers <- NA

if (!is.null(json$accounts$data)) {
  for (i in 1:length(json$accounts$data)) {
    if (!is.null(json$accounts$data[[i]]$instagram_business_account$followers_count)) {
      followers <- json$accounts$data[[i]]$instagram_business_account$followers_count
      break
    }
  }
}

df$followers <- followers

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
  quote = TRUE
)

cat("Finalizado com sucesso!\n")
