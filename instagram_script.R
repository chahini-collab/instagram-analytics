# ================================
# CONFIG
# ================================
cat("🚀 INICIANDO SCRIPT\n")

ACCESS_TOKEN <- Sys.getenv("ACCESS_TOKEN")
IG_USER_ID   <- Sys.getenv("IG_USER_ID")

if (ACCESS_TOKEN == "" || IG_USER_ID == "") {
  stop("❌ ERRO: ACCESS_TOKEN ou IG_USER_ID não definidos")
}

BASE_URL <- "https://graph.facebook.com/v25.0/"

# ================================
# LIBS
# ================================
suppressPackageStartupMessages({
  library(httr)
  library(jsonlite)
  library(dplyr)
  library(readr)
  library(stringr)
})

`%||%` <- function(a, b) if (!is.null(a)) a else b

# ================================
# SAFE GET
# ================================
safe_GET <- function(url) {
  tryCatch({
    res <- GET(url)
    if (status_code(res) != 200) {
      stop(content(res, "text", encoding = "UTF-8"))
    }
    return(res)
  }, error = function(e) {
    cat("❌ ERRO:", url, "\n")
    cat(e$message, "\n")
    return(NULL)
  })
}

# ================================
# PAGINAÇÃO
# ================================
get_all_pages <- function(url) {
  results <- list()

  while (!is.null(url)) {
    res <- safe_GET(url)
    if (is.null(res)) break

    json <- fromJSON(content(res, "text", encoding = "UTF-8"), flatten = TRUE)

    results[[length(results)+1]] <- json$data
    url <- json$paging$`next` %||% NULL
  }

  bind_rows(results)
}

# ================================
# BUSCAR POSTS
# ================================
cat("📡 Buscando mídia...\n")

url_media <- paste0(
  BASE_URL,
  IG_USER_ID,
  "/media?fields=id,caption,like_count,comments_count,media_type,timestamp&limit=50&access_token=",
  ACCESS_TOKEN
)

data_media <- get_all_pages(url_media)

if (nrow(data_media) == 0) {
  stop("❌ Nenhum post encontrado")
}

cat("✅ Total de posts:", nrow(data_media), "\n")

# ================================
# LIMPEZA CRÍTICA DO CAPTION (🔥)
# ================================
data_media$caption <- data_media$caption %>%
  str_replace_all("\r\n|\r|\n", " ") %>%   # remove quebra de linha
  str_replace_all(",", " ")               # remove vírgula (evita quebrar CSV)

# ================================
# INSIGHTS
# ================================
get_insights <- function(post_id) {

  url <- paste0(
    BASE_URL,
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
# LOOP
# ================================
cat("🔄 Coletando insights...\n")

insights_list <- lapply(data_media$id, function(id) {
  cat("➡️ Post:", id, "\n")
  Sys.sleep(0.15)
  get_insights(id)
})

insights_df <- bind_rows(insights_list)

# ================================
# FOLLOWERS
# ================================
cat("👥 Buscando followers...\n")

url_followers <- paste0(
  BASE_URL,
  IG_USER_ID,
  "?fields=followers_count&access_token=",
  ACCESS_TOKEN
)

res_followers <- safe_GET(url_followers)
if (is.null(res_followers)) stop("❌ Erro followers")

followers <- fromJSON(content(res_followers, "text", encoding = "UTF-8"))$followers_count

# ================================
# DATA FINAL
# ================================
df <- bind_cols(data_media, insights_df) %>%
  distinct(id, .keep_all = TRUE) %>%
  mutate(
    followers = followers,
    timestamp = as.POSIXct(timestamp)
  )

# ================================
# SALVAR (BLINDADO)
# ================================
cat("💾 Salvando CSV...\n")

write_csv(df, "instagram_posts.csv", na = "")

cat("✅ FINALIZADO COM SUCESSO\n")
