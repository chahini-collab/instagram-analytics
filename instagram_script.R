library(httr)
library(jsonlite)
library(dplyr)
library(readr)

options(error = function(e) {
  cat("🔥 ERRO GLOBAL:", conditionMessage(e), "\n")
})

cat("🚀 START PIPELINE\n")

# ========================
# ENV
# ========================
access_token <- Sys.getenv("ACCESS_TOKEN")
ig_user_id   <- Sys.getenv("IG_USER_ID")

cat("TOKEN:", ifelse(access_token == "", "VAZIO", "OK"), "\n")
cat("USER:", ig_user_id, "\n")

if (access_token == "" || ig_user_id == "") {
  cat("❌ ENV VARS AUSENTES\n")
}

# ========================
# SAFE REQUEST
# ========================
safe_request <- function(url) {
  tryCatch({
    res <- GET(url)
    
    cat("🌐 STATUS:", status_code(res), "\n")
    
    if (status_code(res) != 200) {
      cat("❌ API:", content(res, "text"), "\n")
      return(NULL)
    }
    
    txt <- content(res, "text", encoding = "UTF-8")
    
    if (is.null(txt) || txt == "") return(NULL)
    
    fromJSON(txt)
    
  }, error = function(e) {
    cat("❌ REQUEST:", e$message, "\n")
    return(NULL)
  })
}

# ========================
# POSTS
# ========================
posts <- NULL

try({
  cat("📡 BUSCANDO POSTS\n")
  
  url <- paste0(
    "https://graph.facebook.com/v19.0/",
    ig_user_id,
    "/media?fields=id,caption,media_type,media_url,timestamp,like_count,comments_count&limit=50&access_token=",
    access_token
  )
  
  data_json <- safe_request(url)
  
  if (!is.null(data_json) && !is.null(data_json$data)) {
    posts <- data_json$data
  }
}, silent = TRUE)

# ========================
# FALLBACK POSTS
# ========================
if (is.null(posts)) {
  cat("⚠️ SEM POSTS - criando estrutura\n")
  
  posts <- data.frame(
    id = NA,
    caption = NA,
    media_type = NA,
    media_url = NA,
    timestamp = NA,
    like_count = NA,
    comments_count = NA
  )
}

cat("📊 TOTAL POSTS:", nrow(posts), "\n")

# ========================
# INSIGHT
# ========================
get_metric <- function(id, metric) {
  tryCatch({
    
    url <- paste0(
      "https://graph.facebook.com/v19.0/",
      id,
      "/insights?metric=",
      metric,
      "&access_token=",
      access_token
    )
    
    json <- safe_request(url)
    
    if (is.null(json)) return(NA)
    if (is.null(json$data)) return(NA)
    if (length(json$data) == 0) return(NA)
    
    json$data[[1]]$values[[1]]$value
    
  }, error = function(e) {
    cat("⚠️ insight erro:", e$message, "\n")
    return(NA)
  })
}

# ========================
# LOOP
# ========================
cat("🔄 LOOP\n")

rows <- list()

for (i in seq_len(nrow(posts))) {
  
  cat("➡️ POST", i, "\n")
  
  try({
    
    id <- posts$id[i]
    
    rows[[i]] <- data.frame(
      id = id,
      caption = ifelse(is.null(posts$caption[i]), "", posts$caption[i]),
      media_type = posts$media_type[i],
      media_url = posts$media_url[i],
      timestamp = posts$timestamp[i],
      like_count = posts$like_count[i],
      comments_count = posts$comments_count[i],
      reach = get_metric(id, "reach"),
      impressions = get_metric(id, "impressions"),
      saved = get_metric(id, "saved"),
      stringsAsFactors = FALSE
    )
    
    Sys.sleep(1)
    
  }, silent = TRUE)
}

df <- bind_rows(rows)

# ========================
# FOLLOWERS
# ========================
followers <- NA

try({
  cat("👥 FOLLOWERS\n")
  
  url_f <- paste0(
    "https://graph.facebook.com/v19.0/",
    ig_user_id,
    "?fields=followers_count&access_token=",
    access_token
  )
  
  json_f <- safe_request(url_f)
  
  if (!is.null(json_f)) {
    followers <- json_f$followers_count
  }
}, silent = TRUE)

df$followers <- followers

# ========================
# GARANTIA FINAL
# ========================
if (is.null(df) || nrow(df) == 0) {
  cat("⚠️ DF VAZIO - criando fallback\n")
  
  df <- data.frame(
    id = NA,
    caption = NA,
    media_type = NA,
    media_url = NA,
    timestamp = NA,
    like_count = NA,
    comments_count = NA,
    reach = NA,
    impressions = NA,
    saved = NA,
    followers = followers
  )
}

# ========================
# EXPORT
# ========================
cat("💾 SALVANDO CSV\n")

write_csv(df, "instagram_posts.csv", na = "")

cat("✅ FINALIZADO (SEM CRASH)\n")
