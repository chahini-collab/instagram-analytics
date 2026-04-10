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
# FUNÇÃO REQUEST
# ================================
make_request <- function(url) {
  response <- GET(url)

  content_text <- content(response, "text", encoding = "UTF-8")
  json_data <- fromJSON(content_text, flatten = TRUE)

  if (status_code(response) != 200) {
    print(json_data)
    stop(paste("Erro HTTP:", status_code(response)))
  }

  if (!is.null(json_data$error)) {
    print(json_data$error)
    stop(paste("Erro da API:", json_data$error$message))
  }

  return(json_data)
}

# ================================
# GET IG ID
# ================================
get_ig_id <- function(page_id, token) {
  url <- paste0(
    "https://graph.facebook.com/v25.0/",
    page_id,
    "?fields=instagram_business_account&access_token=",
    token
  )

  data <- make_request(url)

  return(data$instagram_business_account$id)
}

# ================================
# GET MEDIA COM PAGINAÇÃO
# ================================
get_all_media <- function(ig_id, token) {
  
  url <- paste0(
    "https://graph.facebook.com/v25.0/",
    ig_id,
    "/media?fields=id,caption,media_type,media_url,timestamp,like_count,comments_count&access_token=",
    token
  )
  
  all_data <- list()
  page <- 1
  
  repeat {
    cat("📦 Página:", page, "\n")
    
    data <- make_request(url)
    
    if (!is.null(data$data)) {
      all_data <- append(all_data, data$data)
    }
    
    if (!is.null(data$paging$next)) {
      url <- data$paging$next
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

# Tratamentos
df$caption[is.na(df$caption)] <- ""
df$timestamp <- as.POSIXct(df$timestamp, format="%Y-%m-%dT%H:%M:%S")

# Garantir colunas
if (!"like_count" %in% colnames(df)) df$like_count <- 0
if (!"comments_count" %in% colnames(df)) df$comments_count <- 0

# ================================
# EXPORT
# ================================
write.xlsx(df, "instagram_posts.xlsx", overwrite = TRUE)
write.csv(df, "instagram_posts.csv", row.names = FALSE)

cat("✅ Finalizado com sucesso!\n")
cat("📊 Total de posts:", nrow(df), "\n")
