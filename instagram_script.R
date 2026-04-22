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

  if (status_code(res) != 200) break

  txt <- content(res, "text", encoding = "UTF-8")

  json <- tryCatch(fromJSON(txt, flatten = TRUE), error = function(e) NULL)

  if (is.null(json) || is.null(json$data)) break

  all_posts <- append(all_posts, list(json$data))

  next_url <- json$paging$`next`
}

df <- bind_rows(all_posts)

# =========================
# FUNÇÃO INSIGHTS SEGURA
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
    return(data.frame(reach=NA, impressions=NA, saved=NA))
  }

  txt <- content(res, "text", encoding = "UTF-8")

  json <- tryCatch(fromJSON(txt), error = function(e) NULL)

  if (is.null(json) || is.null(json$data)) {
    return(data.frame(reach=NA, impressions=NA, saved=NA))
  }

  reach <- NA
  impressions <- NA
  saved <- NA

  for (i in seq_along(json$data)) {
    m <- json$data[[i]]

    if (!is.null(m$name) && m$name == "reach") {
      reach <- m$values[[1]]$value
    }

    if (!is.null(m$name) && m$name == "impressions") {
      impressions <- m$values[[1]]$value
    }

    if (!is.null(m$name) && m$name == "saved") {
      saved <- m$values[[1]]$value
    }
  }

  return(data.frame(
    reach = reach,
    impressions = impressions,
    saved = saved
  ))
}

# =========================
# LOOP SEGURO
# =========================

insights_list <- lapply(df$id, function(id) {
  tryCatch(get_insights(id, access_token),
           error = function(e) data.frame(reach=NA, impressions=NA, saved=NA))
})

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

write.table(
  df,
  "instagram_posts.csv",
  sep = ";",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

cat("Finalizado com sucesso!\n")
