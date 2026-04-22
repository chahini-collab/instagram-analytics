# ================================
# INSTAGRAM ANALYTICS PIPELINE
# ================================

library(httr)
library(jsonlite)
library(dplyr)
library(readr)

# ================================
# 🔐 VARIÁVEIS DE AMBIENTE (GITHUB)
# ================================

access_token <- Sys.getenv("ACCESS_TOKEN")
ig_user_id   <- Sys.getenv("IG_USER_ID")

# Validação obrigatória (evita erro silencioso)
if (access_token == "" || ig_user_id == "") {
  stop("❌ ERRO: ACCESS_TOKEN ou IG_USER_ID não definidos")
}

# ================================
# 📥 BUSCAR POSTS
# ================================

url_media <- paste0(
  "https://graph.facebook.com/v19.0/",
  ig_user_id,
  "/media?fields=id,caption,media_type,media_url,timestamp,like_count,comments_count&limit=100&access_token=",
  access_token
)

res <- GET(url_media)
data_json <- fromJSON(content(res, "text", encoding = "UTF-8"))

posts <- data_json$data

# ================================
# 🔄 FUNÇÃO SEGURA PARA INSIGHTS
# ================================

get_insights <- function(media_id, metric) {
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
  
  tryCatch({
    return(json$data[[1]]$values[[1]]$value)
  }, error = function(e) {
    return(NA)
  })
}

# ================================
# 📊 LOOP DE ENRIQUECIMENTO
# ================================

results <- list()

for (i in 1:nrow(posts)) {
  
  media_id <- posts$id[i]
  
  cat("🔄 Processando:", media_id, "\n")
  
  reach <- get_insights(media_id, "reach")
  impressions <- get_insights(media_id, "impressions")
  saved <- get_insights(media_id, "saved")
  
  results[[i]] <- data.frame(
    id = media_id,
    caption = posts$caption[i],
    media_type = posts$media_type[i],
    media_url = posts$media_url[i],
    timestamp = posts$timestamp[i],
    like_count = posts$like_count[i],
    comments_count = posts$comments_count[i],
    reach = reach,
    impressions = impressions,
    saved = saved
  )
  
  Sys.sleep(1) # evita rate limit
}

df_posts <- bind_rows(results)

# ================================
# 👥 FOLLOWERS (NÍVEL CONTA)
# ================================

url_followers <- paste0(
  "https://graph.facebook.com/v19.0/",
  ig_user_id,
  "?fields=followers_count&access_token=",
  access_token
)

res_followers <- GET(url_followers)
followers_json <- fromJSON(content(res_followers, "text", encoding = "UTF-8"))

followers <- followers_json$followers_count

df_posts$followers <- followers

# ================================
# 🧹 LIMPEZA FINAL
# ================================

df_posts <- df_posts %>%
  mutate(
    reach = as.numeric(reach),
    impressions = as.numeric(impressions),
    saved = as.numeric(saved),
    followers = as.numeric(followers)
  )

# ================================
# 💾 EXPORT CSV
# ================================

write_csv(df_posts, "instagram_posts.csv", na = "")

cat("✅ CSV atualizado com sucesso!\n")
