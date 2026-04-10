import requests
import pandas as pd
import datetime
import os

# --- CONFIGURAÇÕES DO USUÁRIO ---
ACCESS_TOKEN = 'EAAXokkudCowBRFJTmrgqfmc80sqO5BFt2LN4leEZB7xX6bkIIDS4Cro0e5BNWE6RrpZCrBpTODOLiegyCXUSZC97bWK1ZASmCRvvY6KFwfV98KF4GuIwr5g91uN8gizd5EBJSoOAhZC1gvzD5kxxQv10yIUtJekIizmfyI6HlFoHBSUTsoxkN2kYUT3kgCN02D7JHrqT7glZBJJFZAQW2AXE2C7SjzwJUUcCudL6TmVtP2RrK9YCv7T'  # Substitua pelo seu Token de Acesso
INSTAGRAM_ACCOUNT_ID = '17841411707444440'  # Substitua pelo seu ID da conta do Instagram Business
OUTPUT_FILE = 'instagram_analytics.csv'

# --- CONFIGURAÇÕES DA API ---
BASE_URL = 'https://graph.facebook.com/v19.0/'

def get_instagram_media():
    """Busca todos os posts (media) da conta do Instagram."""
    url = f"{BASE_URL}{INSTAGRAM_ACCOUNT_ID}/media"
    params = {
        'access_token': ACCESS_TOKEN,
        'fields': 'id,caption,media_type,timestamp,permalink'
    }
    
    response = requests.get(url, params=params)
    if response.status_code != 200:
        print(f"Erro ao buscar media: {response.json()}")
        return []
    
    return response.json().get('data', [])

def get_media_insights(media_id, media_type):
    """Busca métricas específicas de um post (media)."""
    # Métricas variam dependendo do tipo de mídia (IMAGE, VIDEO, CAROUSEL_ALBUM)
    metrics = 'impressions,reach,engagement,saved'
    if media_type == 'VIDEO':
        metrics += ',video_views'
    
    url = f"{BASE_URL}{media_id}/insights"
    params = {
        'access_token': ACCESS_TOKEN,
        'metric': metrics
    }
    
    response = requests.get(url, params=params)
    if response.status_code != 200:
        # Alguns posts antigos ou específicos podem não ter insights disponíveis
        return {}
    
    insights_data = response.json().get('data', [])
    insights_dict = {item['name']: item['values'][0]['value'] for item in insights_data}
    return insights_dict

def run_pipeline():
    print(f"[{datetime.datetime.now()}] Iniciando extração de dados do Instagram...")
    
    media_list = get_instagram_media()
    if not media_list:
        print("Nenhum post encontrado ou erro na API.")
        return

    all_data = []
    
    for media in media_list:
        media_id = media['id']
        media_type = media['media_type']
        
        print(f"Processando post ID: {media_id} ({media_type})")
        
        # Dados básicos do post
        post_info = {
            'post_id': media_id,
            'caption': media.get('caption', ''),
            'timestamp': media['timestamp'],
            'media_type': media_type,
            'permalink': media['permalink']
        }
        
        # Buscar métricas de insights
        insights = get_media_insights(media_id, media_type)
        post_info.update(insights)
        
        all_data.append(post_info)

    # Criar DataFrame e salvar em CSV
    df = pd.DataFrame(all_data)
    
    # Garantir que as colunas de métricas existam (mesmo que vazias)
    expected_metrics = ['impressions', 'reach', 'engagement', 'saved', 'video_views']
    for metric in expected_metrics:
        if metric not in df.columns:
            df[metric] = 0
            
    # Preencher valores nulos com 0
    df = df.fillna(0)
    
    # Salvar arquivo
    df.to_csv(OUTPUT_FILE, index=False, encoding='utf-8-sig')
    print(f"[{datetime.datetime.now()}] Pipeline concluído! Dados salvos em: {OUTPUT_FILE}")

if __name__ == "__main__":
    # Verifica se o usuário já substituiu as credenciais
    if ACCESS_TOKEN == 'XXXXX' or INSTAGRAM_ACCOUNT_ID == 'YYYYY':
        print("ERRO: Você precisa substituir 'XXXXX' e 'YYYYY' pelas suas credenciais reais no script.")
    else:
        run_pipeline()
