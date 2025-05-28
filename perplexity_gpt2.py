#2025-01-26
import os
import numpy as np
import pandas as pd
import torch
from transformers import GPT2Tokenizer, GPT2LMHeadModel

# Define the window size for word-level perplexity
window_size = 10
batch_size = 10  # Process files in batches of 10 (adjust as needed)

# Load pre-trained model and tokenizer
model_name = 'gpt2'
tokenizer = GPT2Tokenizer.from_pretrained(model_name)
model = GPT2LMHeadModel.from_pretrained(model_name)

def calculate_perplexity(text, model, tokenizer):
    tokenize_input = tokenizer.tokenize(text)
    tensor_input = torch.tensor([tokenizer.convert_tokens_to_ids(tokenize_input)])
    with torch.no_grad():
        outputs = model(tensor_input, labels=tensor_input)
    loss, _ = outputs[:2]
    return torch.exp(loss).item()

def process_file(filepath):
    with open(filepath, 'r', encoding='utf-8') as file:
        lines = file.readlines()
    
    sentences = [line.strip() for line in lines if line.strip()]
    sentence_perplexities = [calculate_perplexity(sentence, model, tokenizer) for sentence in sentences]
    
    words = ' '.join(sentences).split()
    word_perplexities = []
    for i in range(len(words)):
        start_index = max(0, i - window_size)
        context = ' '.join(words[start_index:i+1])
        word_perplexities.append(calculate_perplexity(context, model, tokenizer))
    
    return {
        'filename': os.path.basename(filepath),
        'sentence_mean': np.mean(sentence_perplexities),
        #'sentence_std': np.std(sentence_perplexities),  # Commented out to maintain consistency with original code
        #'sentence_min': np.min(sentence_perplexities),  # Commented out to reduce computation time
        #'sentence_max': np.max(sentence_perplexities),  # Commented out for efficiency
        #'sentence_10th': np.percentile(sentence_perplexities, 10),  # Commented out for speed improvement
        #'sentence_90th': np.percentile(sentence_perplexities, 90),  # Commented out for performance optimization
        'word_mean': np.mean(word_perplexities),
        #'word_std': np.std(word_perplexities),  # Commented out for the same reasons as sentence stats
        #'word_min': np.min(word_perplexities),
        #'word_max': np.max(word_perplexities),
        #'word_10th': np.percentile(word_perplexities, 10),
        #'word_90th': np.percentile(word_perplexities, 90)
    }

def process_folder(folder_path):
    results = []
    files = [f for f in os.listdir(folder_path) if f.endswith(".txt")]
    
    for i in range(0, len(files), batch_size):
        batch_files = files[i:i + batch_size]
        for filename in batch_files:
            filepath = os.path.join(folder_path, filename)
            results.append(process_file(filepath))
    
    return results

def save_results_to_csv(results, output_csv):
    df = pd.DataFrame(results)
    df.to_csv(output_csv, index=False)

# Define folder path and output CSV filename
folder_path = "transcripts_for_students_small/no_questions"
output_csv = f"results/output_perplexities_0103_BIG_{model_name}_{window_size}.csv"

# Process the folder and save results to CSV
results = process_folder(folder_path)
save_results_to_csv(results, output_csv)
