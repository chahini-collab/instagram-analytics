# ================================
# DEBUG INICIAL
# ================================
cat("🚀 INICIANDO SCRIPT\n")

ACCESS_TOKEN <- Sys.getenv("ACCESS_TOKEN")
IG_USER_ID   <- Sys.getenv("IG_USER_ID")

cat("🔍 TOKEN:", ifelse(ACCESS_TOKEN == "", "VAZIO ❌", "OK ✅"), "\n")
cat("🔍 USER_ID:", IG_USER_ID, "\n")

if (ACCESS_TOKEN == "" || IG_USER_ID == "") {
  stop("❌ ERRO: ACCESS_TOKEN ou IG_USER_ID não definidos")
}

# ================================
# LIBS
# ================================
suppressPackageStartupMessages({
  library(httr)
  library(jsonlite)
  library(dplyr)
  library(readr)
})

`%||%` <- function(a, b) if (!is.null(a)) a else b

# ================================
# BUSCAR POSTS
# ================================
cat("📡 Buscando mídia...\n")

url_media <- paste0(
  "https://graph.facebook.com/v19.0/",
  IG_USER_ID,
  "/media?fields=id,caption,like_count,comments_count,media_type,timestamp&limit=50&access_token=",
  ACCESS_TOKEN
)

res_media <- GET(url_media)

if (status_code(res_media) != 200) {
  stop(paste("❌ Erro API media:", content(res_media, "text", encoding = "UTF-8")))
}

data_media <- fromJSON(content(res_media, "text", encoding = "UTF-8"), flatten = TRUE)$data

if (is.null(data_media) || nrow(data_media) == 0) {
  stop("❌ Nenhum post retornado pela API")
}

cat("✅ Posts encontrados:", nrow(data_media), "\n")

# ================================
# FUNÇÃO INSIGHTS
# ================================
get_insights <- function(post_id) {
  url <- paste0(
    "https://graph.facebook.com/v19.0/",
    post_id,
    "/insights?metric=reach,impressions,saved&access_token=",
    ACCESS_TOKEN
  )

  res <- GET(url)

  if (status_code(res) != 200) {
    return(data.frame(
      reach = NA,
      impressions = NA,
      saved = NA
    ))
  }

  data <- fromJSON(content(res, "text", encoding = "UTF-8"), flatten = TRUE)

  if (is.null(data$data)) {
    return(data.frame(
      reach = NA,
      impressions = NA,
      saved = NA
    ))
  }

  metrics <- setNames(
    lapply(data$data$values, function(x) x$value),
    data$data$name
  )

  return(data.frame(
    reach = metrics$reach %||% NA,
    impressions = metrics$impressions %||% NA,
    saved = metrics$saved %||% NA
  ))
}

# ================================
# LOOP POSTS
# ================================
cat("🔄 Coletando insights...\n")

insights_list <- lapply(data_media$id, function(id) {
  cat("➡️ Post:", id, "\n")
  get_insights(id)
})

insights_df <- bind_rows(insights_list)

# ================================
# FOLLOWERS
# ================================
cat("👥 Buscando followers...\n")

url_followers <- paste0(
  "https://graph.facebook.com/v19.0/",
  IG_USER_ID,
  "?fields=followers_count&access_token=",
  ACCESS_TOKEN
)

res_followers <- GET(url_followers)

if (status_code(res_followers) != 200) {
  stop(paste("❌ Erro followers:", content(res_followers, "text", encoding = "UTF-8")))
}

followers <- fromJSON(content(res_followers, "text", encoding = "UTF-8"))$followers_count

cat("✅ Followers:", followers, "\n")

# ================================
# DATA FINAL
# ================================
df <- bind_cols(data_media, insights_df) %>%
  mutate(followers = followers)

# ================================
# SALVAR CSV
# ================================
cat("💾 Salvando CSV...\n")

write_csv(df, "instagram_posts.csv")

cat("✅ FINALIZADO COM SUCESSO\n")
