# 2025-01-28
import os
import numpy as np
import pandas as pd
import torch
from transformers import GPT2Tokenizer, GPT2LMHeadModel

torch.set_num_threads(os.cpu_count())  # Use all available CPU cores


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
        'word_mean': np.mean(word_perplexities),
    }

def process_folder_in_batches(folder_path, output_csv, batch_size=10):
    files = [os.path.join(folder_path, f) for f in os.listdir(folder_path) if f.endswith(".txt")]
    total_files = len(files)
    
    for i in range(0, total_files, batch_size):
        batch_files = files[i:i + batch_size]
        batch_results = [process_file(filepath) for filepath in batch_files]
        
        df = pd.DataFrame(batch_results)
        if not os.path.exists(output_csv):
            df.to_csv(output_csv, index=False)
        else:
            df.to_csv(output_csv, mode='a', header=False, index=False)
        
        print(f"Processed {min(i + batch_size, total_files)} of {total_files} files...")

# Define folder path and output CSV filename
folder_path = "transcripts_for_students/no_questions"
output_csv = f"results/output_perplexities_0103_BIG_{model_name}_{window_size}.csv"

# Process the folder in batches and save results to CSV
process_folder_in_batches(folder_path, output_csv, batch_size=batch_size)