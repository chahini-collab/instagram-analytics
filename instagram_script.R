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

safe_GET <- function(url) {
  tryCatch({
    res <- GET(url)
    if (status_code(res) != 200) {
      stop(content(res, "text", encoding = "UTF-8"))
    }
    return(res)
  }, error = function(e) {
    cat("❌ ERRO NA REQUISIÇÃO:", url, "\n")
    cat(e$message, "\n")
    return(NULL)
  })
}

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

res_media <- safe_GET(url_media)
if (is.null(res_media)) stop("❌ Falha ao buscar mídia")

data_media <- fromJSON(content(res_media, "text", encoding = "UTF-8"), flatten = TRUE)$data

if (is.null(data_media) || nrow(data_media) == 0) {
  stop("❌ Nenhum post retornado")
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

  res <- safe_GET(url)

  if (is.null(res)) {
    return(data.frame(reach=NA, impressions=NA, saved=NA))
  }

  data <- fromJSON(content(res, "text", encoding = "UTF-8"), flatten = TRUE)

  if (is.null(data$data)) {
    return(data.frame(reach=NA, impressions=NA, saved=NA))
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
  Sys.sleep(0.2) # evita rate limit
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

res_followers <- safe_GET(url_followers)
if (is.null(res_followers)) stop("❌ Erro followers")

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
