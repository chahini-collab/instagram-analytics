library(httr)
library(jsonlite)
library(dplyr)
library(readr)

cat("🚀 INICIANDO PIPELINE...\n")

# ================================
# 🔐 VARIÁVEIS
# ================================
access_token <- Sys.getenv("ACCESS_TOKEN")
ig_user_id   <- Sys.getenv("IG_USER_ID")

cat("🔎 Validando variáveis...\n")

if (access_token == "" || ig_user_id == "") {
  stop("❌ ERRO: Variáveis de ambiente não definidas")
}

cat("✅ Variáveis OK\n")

# ================================
# 📥 BUSCAR POSTS
# ================================
cat("📥 Buscando posts...\n")

url_media <- paste0(
  "https://graph.facebook.com/v19.0/",
  ig_user_id,
  "/media?fields=id,caption,media_type,media_url,timestamp,like_count,comments_count&limit=50&access_token=",
  access_token
)

res <- GET(url_media)

if (status_code(res) != 200) {
  stop("❌ ERRO API MEDIA: ", content(res, "text"))
}

json_data <- fromJSON(content(res, "text", encoding = "UTF-8"))

if (is.null(json_data$data) || nrow(json_data$data) == 0) {
  stop("❌ Nenhum post retornado")
}

posts <- json_data$data

cat("✅ Posts:", nrow(posts), "\n")

# ================================
# 🔄 FUNÇÃO INSIGHTS SEGURA
# ================================
get_insight <- function(media_id, metric) {
  
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
    cat("⚠️ Falha:", metric, "ID:", media_id, "\n")
    return(NA)
  }
  
  json <- fromJSON(content(res, "text", encoding = "UTF-8"))
  
  if (is.null(json$data)) return(NA)
  
  val <- tryCatch({
    json$data[[1]]$values[[1]]$value
  }, error = function(e) NA)
  
  return(val)
}

# ================================
# 📊 LOOP PRINCIPAL
# ================================
cat("📊 Coletando insights...\n")

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

# ================================
# 👥 FOLLOWERS
# ================================
cat("👥 Buscando followers...\n")

url_followers <- paste0(
  "https://graph.facebook.com/v19.0/",
  ig_user_id,
  "?fields=followers_count&access_token=",
  access_token
)

res_f <- GET(url_followers)

if (status_code(res_f) != 200) {
  cat("⚠️ Erro ao buscar followers\n")
  followers <- NA
} else {
  json_f <- fromJSON(content(res_f, "text", encoding = "UTF-8"))
  followers <- json_f$followers_count
}

df$followers <- followers

# ================================
# 🧹 LIMPEZA
# ================================
df <- df %>%
  mutate(
    reach = suppressWarnings(as.numeric(reach)),
    impressions = suppressWarnings(as.numeric(impressions)),
    saved = suppressWarnings(as.numeric(saved)),
    followers = suppressWarnings(as.numeric(followers))
  )

# ================================
# 💾 EXPORT
# ================================
cat("💾 Salvando CSV...\n")

write_csv(df, "instagram_posts.csv", na = "")

cat("✅ FINALIZADO COM SUCESSO\n")
