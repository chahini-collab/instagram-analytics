library(httr)
library(jsonlite)
library(openxlsx)

access_token <- Sys.getenv("ACCESS_TOKEN")
ig_id <- "111286197030443"

base_url <- paste0("https://graph.facebook.com/v25.0/", ig_id, "/media")

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
    
    data <- content(response, "text", encoding = "UTF-8")
    json_data <- fromJSON(data, flatten = TRUE)
    
    if (status_code(response) != 200) {
      print(json_data)
      stop(paste("Erro HTTP:", status_code(response)))
    }
    
    if (!is.null(json_data$error)) {
      print(json_data$error)
      stop(paste("Erro da API:", json_data$error$message))
    }
    
    if (!is.null(json_data$data)) {
      all_data <- append(all_data, json_data$data)
    }
    
    if (!is.null(json_data$paging) && !is.null(json_data$paging[["next"]])) {
      url <- json_data$paging[["next"]]
    } else {
      break
    }
  }
  
  return(all_data)
}

cat("🚀 Iniciando coleta...\n")

media_data <- get_all_media(base_url, access_token)

if (length(media_data) == 0) {
  stop("Nenhum dado retornado.")
}

df <- do.call(rbind, lapply(media_data, as.data.frame))

if ("caption" %in% colnames(df)) {
  df$caption[is.na(df$caption)] <- ""
}

if ("timestamp" %in% colnames(df)) {
  df$timestamp <- as.POSIXct(df$timestamp, format="%Y-%m-%dT%H:%M:%S")
}

expected_cols <- c("like_count", "comments_count")
for (col in expected_cols) {
  if (!(col %in% colnames(df))) {
    df[[col]] <- 0
  }
}

write.xlsx(df, "instagram_posts.xlsx", overwrite = TRUE)

cat("✅ Finalizado com sucesso!\n")
