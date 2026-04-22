library(httr)
library(jsonlite)
library(dplyr)
library(readr)

cat("🚀 PIPELINE START\n")

# ========================
# VARIÁVEIS
# ========================
access_token <- Sys.getenv("ACCESS_TOKEN")
ig_user_id   <- Sys.getenv("IG_USER_ID")

if (access_token == "" || ig_user_id == "") {
  stop("❌ ENV VARS AUSENTES: ACCESS_TOKEN ou IG_USER_ID")
}

cat("✅ ENV OK\n")

# ========================
# FUNÇÃO SEGURA API
# ========================
safe_get_json <- function(url) {
  tryCatch({
    res <- GET(url)
    
    if (status_code(res) != 200) {
      cat("⚠️ API ERROR:", status_code(res), "\n")
      return(NULL)
    }
    
    txt <- content(res, "text", encoding = "UTF-8")
    
    if (txt == "" || is.null(txt)) return(NULL)
    
    fromJSON(txt)
    
  }, error = function(e) {
    cat("❌ JSON ERROR:", e$message, "\n")
    return(NULL)
  })
}

# ========================
# BUSCAR POSTS
# ========================
cat("📡 Buscando posts...\n")

url_media <- paste0(
  "https://graph.facebook.com/v19.0/",
  ig_user_id,
  "/media?fields=id,caption,media_type,media_url,timestamp,like_count,comments_count&limit=50&access_token=",
  access_token
)

json_data <- safe_get_json(url_media)

if (is.null(json_data) || is.null(json_data$data)) {
  stop("❌ Falha ao obter posts")
}

posts <- json_data$data

cat("📊 Posts:", nrow(posts), "\n")

# ========================
# INSIGHTS SEGURO
# ========================
get_insight <- function(media_id, metric) {
  
  url <- paste0(
    "https://graph.facebook.com/v19.0/",
    media_id,
    "/insights?metric=",
    metric,
    "&access_token=",
    access_token
  )
  
  json <- safe_get_json(url)
  
  if (is.null(json)) return(NA)
  if (is.null(json$data)) return(NA)
  if (length(json$data) == 0) return(NA)
  
  val <- tryCatch({
    json$data[[1]]$values[[1]]$value
  }, error = function(e) NA)
  
  return(val)
}

# ========================
# LOOP
# ========================
cat("🔄 Coletando insights...\n")

output <- list()

for (i in seq_len(nrow(posts))) {
  
  tryCatch({
    
    media_id <- posts$id[i]
    
    cat("➡️", i, "/", nrow(posts), "|", media_id, "\n")
    
    reach <- get_insight(media_id, "reach")
    impressions <- get_insight(media_id, "impressions")
    saved <- get_insight(media_id, "saved")
    
    output[[i]] <- data.frame(
      id = media_id,
      caption = ifelse(is.null(posts$caption[i]), "", posts$caption[i]),
      media_type = posts$media_type[i],
      media_url = posts$media_url[i],
      timestamp = posts$timestamp[i],
      like_count = posts$like_count[i],
      comments_count = posts$comments_count[i],
      reach = reach,
      impressions = impressions,
      saved = saved,
      stringsAsFactors = FALSE
    )
    
    Sys.sleep(1)
    
  }, error = function(e) {
    cat("❌ ERRO NO POST:", i, "|", e$message, "\n")
  })
}

df <- bind_rows(output)

# ========================
# FOLLOWERS
# ========================
cat("👥 Buscando followers...\n")

followers <- NA

tryCatch({
  url_followers <- paste0(
    "https://graph.facebook.com/v19.0/",
    ig_user_id,
    "?fields=followers_count&access_token=",
    access_token
  )
  
  json_f <- safe_get_json(url_followers)
  
  if (!is.null(json_f)) {
    followers <- json_f$followers_count
  }
}, error = function(e) {
  cat("⚠️ erro followers\n")
})

df$followers <- followers

# ========================
# LIMPEZA
# ========================
df <- df %>%
  mutate(
    reach = as.numeric(reach),
    impressions = as.numeric(impressions),
    saved = as.numeric(saved),
    followers = as.numeric(followers)
  )

# ========================
# EXPORT
# ========================
cat("💾 Salvando CSV...\n")

write_csv(df, "instagram_posts.csv", na = "")

cat("✅ FINALIZADO COM SUCESSO\n")
