library(httr)
library(jsonlite)
library(dplyr)

# ==============================
# 🔐 CONFIG
# ==============================
access_token <- Sys.getenv("ACCESS_TOKEN")
ig_user_id <- Sys.getenv("IG_USER_ID")

# ==============================
# 📥 FUNÇÃO: PEGAR POSTS
# ==============================
get_posts <- function() {
  url <- paste0(
    "https://graph.facebook.com/v25.0/",
    ig_user_id,
    "/media?fields=id,caption,media_type,media_url,timestamp,like_count,comments_count&limit=50&access_token=",
    access_token
  )
  
  res <- fromJSON(url)
  
  return(res$data)
}

# ==============================
# 📊 FUNÇÃO: INSIGHTS POR POST
# ==============================
get_insights <- function(media_id) {
  
  url <- paste0(
    "https://graph.facebook.com/v25.0/",
    media_id,
    "/insights?metric=reach,impressions,saved&access_token=",
    access_token
  )
  
  res <- tryCatch({
    fromJSON(url)
  }, error = function(e) return(NULL))
  
  # ⚠️ tratamento de erro
  if (is.null(res) || !is.null(res$error) || is.null(res$data)) {
    return(data.frame(
      reach = NA,
      impressions = NA,
      saved = NA
    ))
  }
  
  metrics <- setNames(
    sapply(res$data, function(x) x$values[[1]]$value),
    sapply(res$data, function(x) x$name)
  )
  
  return(data.frame(
    reach = metrics["reach"],
    impressions = metrics["impressions"],
    saved = metrics["saved"]
  ))
}

# ==============================
# 👥 FUNÇÃO: FOLLOWERS
# ==============================
get_followers <- function() {
  
  url <- paste0(
    "https://graph.facebook.com/v25.0/me?fields=accounts{instagram_business_account{followers_count}}&access_token=",
    access_token
  )
  
  res <- fromJSON(url)
  
  followers <- res$accounts$data[[1]]$instagram_business_account$followers_count
  
  return(followers)
}

# ==============================
# 🚀 PIPELINE
# ==============================

cat("🔄 Buscando posts...\n")
posts <- get_posts()

df <- posts %>%
  select(
    id,
    caption,
    media_type,
    media_url,
    timestamp,
    like_count,
    comments_count
  )

# ==============================
# 📊 ENRIQUECER COM INSIGHTS
# ==============================

cat("📊 Coletando insights...\n")

insights_list <- list()

for (i in 1:nrow(df)) {
  
  cat("Post:", i, "de", nrow(df), "\n")
  
  insights <- get_insights(df$id[i])
  
  insights_list[[i]] <- insights
  
  Sys.sleep(1) # 🐢 evita bloqueio da API
}

insights_df <- bind_rows(insights_list)

df <- bind_cols(df, insights_df)

# ==============================
# 👥 ADICIONAR FOLLOWERS
# ==============================

cat("👥 Buscando followers...\n")

followers <- get_followers()

df$followers <- followers

# ==============================
# 🧹 LIMPEZA FINAL
# ==============================

df <- df %>%
  mutate(
    reach = as.numeric(reach),
    impressions = as.numeric(impressions),
    saved = as.numeric(saved)
  )

# ==============================
# 💾 EXPORTAR CSV
# ==============================

cat("💾 Salvando CSV...\n")

write.csv(df, "instagram_posts.csv", row.names = FALSE)

cat("✅ FINALIZADO!\n")
