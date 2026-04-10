library(httr)
library(jsonlite)
library(openxlsx)

# ================================
# CONFIG
# ================================
access_token <- Sys.getenv("ACCESS_TOKEN")
page_id <- "111286197030443"

cat("🚀 Iniciando coleta...\n")

if (access_token == "") {
  stop("❌ ACCESS_TOKEN não encontrado.")
}

# ================================
# REQUEST
# ================================
make_request <- function(url, query_list) {

  response <- GET(url, query = query_list)
  content_text <- content(response, "text", encoding = "UTF-8")

  cat("📡 Status:", status_code(response), "\n")

  if (status_code(response) != 200) {
    cat(content_text)
    stop("❌ Erro HTTP")
  }

  json_data <- fromJSON(content_text, flatten = TRUE)

  if (!is.null(json_data$error)) {
    print(json_data$error)
    stop(json_data$error$message)
  }

  return(json_data)
}

# ================================
# PEGAR IG ID
# ================================
get_ig_id <- function(page_id, token) {

  url <- paste0("https://graph.facebook.com/v25.0/", page_id)

  data <- make_request(
    url,
    list(
      fields = "instagram_business_account",
      access_token = token
    )
  )

  return(data$instagram_business_account$id)
}

# ================================
# PEGAR MEDIA
# ================================
get_all_media <- function(ig_id, token) {

  url <- paste0("https://graph.facebook.com/v25.0/", ig_id, "/media")

  all_data <- list()
  page <- 1

  repeat {

    cat("📦 Página:", page, "\n")

    data <- make_request(
      url,
      list(
        fields = "id,caption,media_type,media_url,timestamp,like_count,comments_count",
        access_token = token
      )
    )

    if (!is.null(data$data)) {
      all_data <- append(all_data, data$data)
    }

    if (!is.null(data$paging) && !is.null(data$paging[["next"]])) {
      url <- data$paging[["next"]]
      page <- page + 1
    } else {
      break
    }
  }

  return(all_data)
}

# ================================
# EXECUÇÃO
# ================================
ig_id <- get_ig_id(page_id, access_token)
cat("🔗 IG ID:", ig_id, "\n")

media_data <- get_all_media(ig_id, access_token)

if (length(media_data) == 0) {
  stop("❌ Nenhum dado retornado.")
}

# ================================
# DATAFRAME
# ================================
df <- do.call(rbind, lapply(media_data, as.data.frame))

# ================================
# GARANTE COLUNAS
# ================================
cols_needed <- c(
  "id",
  "caption",
  "media_type",
  "media_url",
  "timestamp",
  "like_count",
  "comments_count"
)

for (col in cols_needed) {
  if (!(col %in% colnames(df))) {
    df[[col]] <- NA
  }
}

# ================================
# TRATAMENTOS
# ================================

# caption
df$caption <- as.character(df$caption)
df$caption[is.na(df$caption)] <- ""

# 🔥 LIMPEZA PARA POWER BI
df$caption <- gsub("\n", " ", df$caption)
df$caption <- gsub("\r", " ", df$caption)
df$caption <- gsub("\"", "'", df$caption)

# timestamp
df$timestamp <- as.POSIXct(df$timestamp, format="%Y-%m-%dT%H:%M:%S")

# métricas
df$like_count[is.na(df$like_count)] <- 0
df$comments_count[is.na(df$comments_count)] <- 0

cat("📊 Total de posts:", nrow(df), "\n")

# ================================
# EXPORT
# ================================
write.xlsx(df, "instagram_posts.xlsx", overwrite = TRUE)

write.csv(
  df,
  "instagram_posts.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8",
  quote = TRUE
)

cat("✅ Finalizado com sucesso!\n")
