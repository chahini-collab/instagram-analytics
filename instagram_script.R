library(httr)
library(jsonlite)
library(dplyr)

# ==============================
# 🔐 CONFIG
# ==============================
access_token <- Sys.getenv("ACCESS_TOKEN")
ig_user_id <- Sys.getenv("IG_USER_ID")

cat("🔑 Token carregado\n")
cat("🆔 IG User ID:", ig_user_id, "\n")

if (access_token == "" || ig_user_id == "") {
  stop("❌ ERRO: ACCESS_TOKEN ou IG_USER_ID não definidos")
}

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
  
  cat("📡 Buscando posts...\n")
  
  res <- tryCatch({
    fromJSON(url)
  }, error = function(e) return(NULL))
  
  if (is.null(res) || !is.null(res$error) || is.null(res$data)) {
    stop("❌ ERRO ao buscar posts")
  }
  
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
  
  if (is.null(res) || !is.null(res$error) || is.null(res$data)) {
    cat("⚠️ Insight falhou:", media_id, "\n")
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
    reach = as.numeric(metrics["reach"]),
    impressions = as.numeric(metrics["impressions"]),
    saved = as.numeric(metrics["saved"])
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
  
  res <- tryCatch({
    fromJSON(url)
  }, error = function(e) return(NULL))
  
  if (is.null(res) || is.null(res$accounts$data[[1]]$instagram_business_account)) {
    cat("⚠️ Não conseguiu pegar followers\n")
    return(NA)
  }
  
  return(res$accounts$data[[1]]$instagram_business_account$followers_count)
}

# ==============================
# 🚀 PIPELINE
# ==============================

cat("🚀 INICIANDO PIPELINE\n")

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
  
  cat("Post", i, "de", nrow(df), "\n")
  
  insights <- get_insights(df$id[i])
  
  insights_list[[i]] <- insights
  
  Sys.sleep(1) # evita bloqueio API
}

insights_df <- bind_rows(insights_list)

df <- bind_cols(df, insights_df)

# ==============================
# 👥 FOLLOWERS
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
    saved = as.numeric(saved),
    like_count = as.numeric(like_count),
    comments_count = as.numeric(comments_count)
  )

# ==============================
# 💾 EXPORT
# ==============================

cat("💾 Salvando CSV...\n")

write.csv(df, "instagram_posts.csv", row.names = FALSE, na = "")

cat("✅ PIPELINE FINALIZADO COM SUCESSO\n")
