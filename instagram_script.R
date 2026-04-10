library(httr)
library(jsonlite)
library(openxlsx)

# 🔐 Token via variável de ambiente (RECOMENDADO)
access_token <- Sys.getenv("ACCESS_TOKEN")

# 🆔 ID correto do Instagram
ig_id <- "17841411707444440"

base_url <- paste0("https://graph.facebook.com/v25.0/", ig_id, "/media")

# 📦 Função para buscar TODOS os posts (com paginação)
get_all_media <- function(url, token) {
  all_data <- list()
  
  repeat {
    response <- GET(
      url,
      query = list(
        fields = "id,caption,media_type,media_url,timestamp,like_count,comments_count",
        access_token = token
      )
    )
    
    if (status_code(response) != 200) {
      stop(paste("Erro HTTP:", status_code(response), content(response, "text")))
    }
    
    data <- content(response, "text", encoding = "UTF-8")
    json_data <- fromJSON(data, flatten = TRUE)
    
    # ❗ Verifica erro da API
    if (!is.null(json_data$error)) {
      stop(paste("Erro da API:", json_data$error$message))
    }
    
    # 📥 Adiciona dados
    if (!is.null(json_data$data)) {
      all_data <- append(all_data, json_data$data)
    }
    
    # 🔁 PAGINAÇÃO (CORRIGIDO AQUI)
    if (!is.null(json_data$paging) && !is.null(json_data$paging[["next"]])) {
      url <- json_data$paging[["next"]]
    } else {
      break
    }
  }
  
  return(all_data)
}

# ▶️ Executa coleta
cat("🚀 Iniciando coleta...\n")

media_data <- get_all_media(base_url, access_token)

if (length(media_data) == 0) {
  stop("Nenhum dado retornado.")
}

# 📊 Converte para dataframe
df <- do.call(rbind, lapply(media_data, as.data.frame))

# 🧹 Limpeza
if ("caption" %in% colnames(df)) {
  df$caption[is.na(df$caption)] <- ""
}

# 📅 Ajusta data
if ("timestamp" %in% colnames(df)) {
  df$timestamp <- as.POSIXct(df$timestamp, format="%Y-%m-%dT%H:%M:%S")
}

# 🔢 Garante colunas (evita erro se API não retornar)
expected_cols <- c("like_count", "comments_count")
for (col in expected_cols) {
  if (!(col %in% colnames(df))) {
    df[[col]] <- 0
  }
}

# 💾 Exporta Excel
write.xlsx(df, "instagram_posts.xlsx", overwrite = TRUE)

cat("✅ Finalizado! Arquivo salvo como instagram_posts.xlsx\n")
