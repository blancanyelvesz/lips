#2025-03-19
import os
import numpy as np
import pandas as pd
import torch
from transformers import GPT2Tokenizer, GPT2LMHeadModel
from concurrent.futures import ProcessPoolExecutor

# THIS DOES NOT WORK!!!

# Set device to CPU or GPU: this code is from an online tutorial idk what im doing!
# device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
# if device == "cpu": 
#     torch.set_num_threads(os.cpu_count()) # Use all available CPU cores
#     print(f"Using CPU with {os.cpu_count()} cores")
# else:
#     print(f"GPU: {torch.cuda.get_device_name(0)}")
torch.set_num_threads(os.cpu_count())

# Define the window size for word-level perplexity
window_size = 50
batch_size = 10  # Process files in batches of 10 (adjust as needed)

# Load pre-trained model and tokenizer
model_name = 'gpt2'
tokenizer = GPT2Tokenizer.from_pretrained(model_name)
model = GPT2LMHeadModel.from_pretrained(model_name)

# def calculate_perplexity(text, model, tokenizer):
#     tokenize_input = tokenizer.tokenize(text)
#     tensor_input = torch.tensor([tokenizer.convert_tokens_to_ids(tokenize_input)])
#     with torch.no_grad():
#         outputs = model(tensor_input, labels=tensor_input)
#     loss, _ = outputs[:2]
#     return torch.exp(loss).item()
def calculate_perplexity_batch(texts, model, tokenizer):
    tokenized_inputs = tokenizer(texts, return_tensors="pt", padding=True, truncation=True)
    tensor_input = tokenized_inputs["input_ids"]
    with torch.no_grad():
        outputs = model(tensor_input, labels=tensor_input)
    losses = outputs.loss
    return torch.exp(losses).tolist()

# def process_file(filepath):
#     with open(filepath, 'r', encoding='utf-8') as file:
#         lines = file.readlines()
    
#     #sentences = [line.strip() for line in lines if line.strip()]
#     #sentence_perplexities = [calculate_perplexity(sentence, model, tokenizer) for sentence in sentences]
    
#     words = ' '.join(line.strip() for line in lines if line.strip()).split()
#     word_perplexities = []
#     for i in range(len(words)):
#         start_index = max(0, i - window_size)
#         context = ' '.join(words[start_index:i+1])
#         word_perplexities.append(calculate_perplexity(context, model, tokenizer))
#         base_filename = os.path.basename(filepath)
#         short_filename = '_'.join(base_filename.split('_')[:3])

#     return {
#         'filename': short_filename,
#         #'sentence_mean': np.mean(sentence_perplexities),
#         'word_mean': np.mean(word_perplexities),
#     }

def process_file(filepath):
    words = []
    with open(filepath, 'r', encoding='utf-8') as file:
        for line in file:
            if line.strip():
                words.extend(line.strip().split())
    
    contexts = [
        ' '.join(words[max(0, i - window_size):i + 1])
        for i in range(len(words))
    ]
    
    word_perplexities = calculate_perplexity_batch(contexts, model, tokenizer)
    base_filename = os.path.basename(filepath)
    short_filename = '_'.join(base_filename.split('_')[:3])

    return {
        'filename': short_filename,
        'word_mean': np.mean(word_perplexities),
    }

# def process_folder_in_batches(folder_path, output_csv, batch_size):
#     files = [os.path.join(folder_path, f) for f in os.listdir(folder_path) if f.endswith(".txt")]
#     total_files = len(files)
    
#     for i in range(0, total_files, batch_size):
#         batch_files = files[i:i + batch_size]
#         batch_results = [process_file(filepath) for filepath in batch_files]
        
#         df = pd.DataFrame(batch_results)
#         if not os.path.exists(output_csv):
#             df.to_csv(output_csv, index=False)
#         else:
#             df.to_csv(output_csv, mode='a', header=False, index=False)
        
#         print(f"Processed {min(i + batch_size, total_files)} of {total_files} files...")


def process_folder_in_batches(folder_path, output_csv, batch_size):
    files = [os.path.join(folder_path, f) for f in os.listdir(folder_path) if f.endswith(".txt")]
    total_files = len(files)
    
    with ProcessPoolExecutor() as executor:
        for i in range(0, total_files, batch_size):
            batch_files = files[i:i + batch_size]
            batch_results = list(executor.map(process_file, batch_files))
            
            df = pd.DataFrame(batch_results)
            if not os.path.exists(output_csv):
                df.to_csv(output_csv, index=False)
            else:
                df.to_csv(output_csv, mode='a', header=False, index=False)
            
            print(f"Processed {min(i + batch_size, total_files)} of {total_files} files...")

# Define folder path and output CSV filename
folder_path = "1st_trans_filtered"
output_csv = f"results/output_wordperp_0319_{model_name}_{window_size}.csv"

# Process the folder in batches and save results to CSV
process_folder_in_batches(folder_path, output_csv, batch_size)