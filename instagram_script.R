library(httr)
library(jsonlite)
library(dplyr)
library(readr)

cat("🚀 PIPELINE INICIADO\n")

# ========================
# VARIÁVEIS
# ========================
access_token <- Sys.getenv("ACCESS_TOKEN")
ig_user_id   <- Sys.getenv("IG_USER_ID")

if (access_token == "" || ig_user_id == "") {
  stop("❌ ACCESS_TOKEN ou IG_USER_ID não definidos")
}

cat("✅ Variáveis carregadas\n")

# ========================
# BUSCAR POSTS
# ========================
url_media <- paste0(
  "https://graph.facebook.com/v19.0/",
  ig_user_id,
  "/media?fields=id,caption,media_type,media_url,timestamp,like_count,comments_count&limit=50&access_token=",
  access_token
)

res <- GET(url_media)

if (status_code(res) != 200) {
  stop("❌ Erro ao buscar mídia: ", content(res, "text"))
}

json_data <- fromJSON(content(res, "text", encoding = "UTF-8"))

posts <- json_data$data

if (is.null(posts) || nrow(posts) == 0) {
  stop("❌ Nenhum post encontrado")
}

cat("📊 Posts encontrados:", nrow(posts), "\n")

# ========================
# FUNÇÃO SEGURA INSIGHTS
# ========================
get_insight <- function(media_id, metric) {
  
  tryCatch({
    
    url <- paste0(
      "https://graph.facebook.com/v19.0/",
      media_id,
      "/insights?metric=",
      metric,
      "&access_token=",
      access_token
    )
    
    res <- GET(url)
    
    if (status_code(res) != 200) {
      return(NA)
    }
    
    json <- fromJSON(content(res, "text", encoding = "UTF-8"))
    
    if (is.null(json$data)) return(NA)
    
    if (length(json$data) == 0) return(NA)
    
    val <- json$data[[1]]$values[[1]]$value
    
    if (is.null(val)) return(NA)
    
    return(val)
    
  }, error = function(e) {
    return(NA)
  })
}

# ========================
# LOOP PRINCIPAL
# ========================
cat("🔄 Coletando insights...\n")

output <- list()

for (i in seq_len(nrow(posts))) {
  
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
}

df <- bind_rows(output)

# ========================
# FOLLOWERS (SEGURO)
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
  
  res_f <- GET(url_followers)
  
  if (status_code(res_f) == 200) {
    json_f <- fromJSON(content(res_f, "text", encoding = "UTF-8"))
    followers <- json_f$followers_count
  }
  
}, error = function(e) {
  followers <<- NA
})

df$followers <- followers

# ========================
# LIMPEZA FINAL
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

cat("✅ FINALIZADO SEM QUEBRAR\n")
