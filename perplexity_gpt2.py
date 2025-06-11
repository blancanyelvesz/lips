#2025-03-26
import os
import unicodedata
import numpy as np
import pandas as pd
import torch
from transformers import GPT2Tokenizer, GPT2LMHeadModel
# THIS IS FOR SENTENCES!!

torch.set_num_threads(os.cpu_count())  # Use all available CPU cores

# these should be inputs or arguments imo
# Define the window size for word-level perplexity
window_size = 10
batch_size = 10  # Process files in batches of 10 (adjust as needed)
method = 'word'

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
    try: # DEBUG LINE
        print(f"Processing file: {filepath}") # DEBUG LINE
        with open(filepath, 'r', encoding='utf-8') as file:
            lines = file.readlines()
        base_filename = os.path.basename(filepath)
        short_filename = '_'.join(base_filename.split('_')[:3])

        sentences = [unicodedata.normalize('NFKC', line.strip()) for line in lines if line.strip()]
        
        if method == 'sentence':
            sentence_perplexities = []
            for i in range(window_size - 1, len(sentences)):
                start_index = i - (window_size - 1)
                context = ' '.join(sentences[start_index:i+1])
                sentence_perplexities.append(calculate_perplexity(context, model, tokenizer))
                sentence_perplexities = [p for p in sentence_perplexities if not np.isnan(p)] 
            results = {
            'filename': short_filename,
            'sentence_mean': np.mean(sentence_perplexities),
            'sentence_std': np.std(sentence_perplexities),
            'sentence_min': np.min(sentence_perplexities),
            'sentence_max': np.max(sentence_perplexities),
            'sentence_10th': np.percentile(sentence_perplexities, 10),
            'sentence_90th': np.percentile(sentence_perplexities, 90)
            }

        if method == 'word':
            words = ' '.join(sentences).split()
            word_perplexities = []

            for i in range(window_size - 1, len(words)):
                start_index = i - (window_size - 1)
                context = ' '.join(words[start_index:i+1])
                word_perplexities.append(calculate_perplexity(context, model, tokenizer))
            results = {
            'filename': short_filename,
            'word_mean': np.mean(word_perplexities),
            'word_std': np.std(word_perplexities),
            'word_min': np.min(word_perplexities),
            'word_max': np.max(word_perplexities),
            'word_10th': np.percentile(word_perplexities, 10),
            'word_90th': np.percentile(word_perplexities, 90)
            }

        return results
        
    except Exception as e:
        print(f"Error processing {filepath}: {e}")
        return {
            'filename': short_filename,
            'word_mean': None
        }


def process_folder(folder_path, output_csv, batch_size=10):
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
folder_path = "problematicfiles"
output_csv = f"results/output_perp_0326_{method}_{model_name}_{window_size}.csv"

# Process the folder in batches and save results to CSV
process_folder(folder_path, output_csv, batch_size=batch_size)