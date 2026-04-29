# ================================
# INSTALL AUTOMÁTICO (🔥 ESSENCIAL)
# ================================
required_packages <- c("httr", "jsonlite", "dplyr", "stringr")

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
    library(pkg, character.only = TRUE)
  }
}

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

`%||%` <- function(a, b) if (!is.null(a)) a else b

# ================================
# SAFE GET
# ================================
safe_GET <- function(url) {
  tryCatch({
    res <- httr::GET(url)

    if (httr::status_code(res) != 200) {
      stop(httr::content(res, "text", encoding = "UTF-8"))
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

    json <- jsonlite::fromJSON(httr::content(res, "text", encoding = "UTF-8"), flatten = TRUE)

    if (is.null(json$data)) break

    results[[length(results)+1]] <- json$data
    url <- json$paging$`next` %||% NULL
  }

  dplyr::bind_rows(results)
}

# ================================
# BUSCAR POSTS
# ================================
cat("📡 Buscando mídia...\n")

url_media <- paste0(
  BASE_URL,
  IG_USER_ID,
  "/media?fields=id,caption,like_count,comments_count,media_type,timestamp,media_url,permalink,thumbnail_url,username&limit=50&access_token=",
  ACCESS_TOKEN
)

data_media <- get_all_pages(url_media)

if (is.null(data_media) || nrow(data_media) == 0) {
  stop("❌ Nenhum post encontrado")
}

# ================================
# GARANTIR COLUNAS
# ================================
required_cols <- c(
  "id","caption","like_count","comments_count","media_type","timestamp",
  "media_url","permalink","thumbnail_url","username"
)

for (col in required_cols) {
  if (!col %in% colnames(data_media)) {
    data_media[[col]] <- NA
  }
}

# ================================
# LIMPEZA CAPTION
# ================================
data_media$caption <- data_media$caption %>%
  stringr::str_replace_all("[\r\n]+", " ") %>%
  stringr::str_replace_all('"', "'") %>%
  stringr::str_replace_all(";", " ") %>%
  stringr::str_replace_all(",", " ") %>%
  stringr::str_squish()

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

  data <- jsonlite::fromJSON(httr::content(res, "text", encoding = "UTF-8"), flatten = TRUE)

  if (is.null(data$data)) {
    return(data.frame(reach=NA, impressions=NA, saved=NA))
  }

  metrics <- setNames(
    lapply(data$data$values, function(x) x$value),
    data$data$name
  )

  data.frame(
    reach = metrics$reach %||% NA,
    impressions = metrics$impressions %||% NA,
    saved = metrics$saved %||% NA
  )
}

# ================================
# LOOP INSIGHTS
# ================================
cat("🔄 Coletando insights...\n")

insights_df <- dplyr::bind_rows(lapply(data_media$id, function(id) {
  Sys.sleep(0.15)
  get_insights(id)
}))

# ================================
# FOLLOWERS
# ================================
url_followers <- paste0(
  BASE_URL,
  IG_USER_ID,
  "?fields=followers_count&access_token=",
  ACCESS_TOKEN
)

res_followers <- safe_GET(url_followers)

followers <- if (!is.null(res_followers)) {
  jsonlite::fromJSON(httr::content(res_followers, "text", encoding = "UTF-8"))$followers_count
} else {
  NA
}

# ================================
# DATA FINAL
# ================================
df <- dplyr::bind_cols(data_media, insights_df) %>%
  dplyr::distinct(id, .keep_all = TRUE) %>%
  dplyr::mutate(
    followers = followers,
    timestamp = as.POSIXct(timestamp, tz = "UTC")
  ) %>%
  dplyr::select(
    id, caption, like_count, comments_count, media_type, timestamp,
    media_url, permalink, username, thumbnail_url,
    reach, impressions, saved, followers
  )

# ================================
# SALVAR
# ================================
cat("💾 Salvando CSV...\n")

write.csv(
  df,
  "instagram_posts.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8",
  na = ""
)

cat("✅ FINALIZADO COM SUCESSO\n")
