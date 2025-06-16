"""
This script calculates the perplexity of text files using the pre-trained language model LLaMA available in huggingface 🤗 for contexts of sizes 10, 20, 30, 40, and 50.
This context consist of words or sentences depending on the --method used. 
The perplexity is calculated for each context and the results are saved to a CSV file in the 'results' folder.
Read README.md for more information. 
# TO DO: WRITE A READ ME. 

THIS DOES NOT WORK BECAUSE LLAMA IS GATED!!!

Usage:
    perp.py [options]
    perp.py (-h | --help)

Options:
    -h --help                              Show this screen.
    -v --version                           Show version.
    -t --method <method>                   Method to use for calculating perplexity: 'sentence' or 'word' [default: word].
    -f --folder <folder>                   Folder path containing text files to process [default: 1st_trans_smallest]
"""

import os
import unicodedata
import numpy as np
import pandas as pd
import torch
from transformers import LlamaTokenizer, LlamaForCausalLM
from docopt import docopt

# Parse command line arguments
args = docopt(__doc__, version='1.0 - OpenLLaMA')
method = args['--method']
folder_path = args['--folder']
batch_size = 10

# Set up device
device = torch.device("cuda:0" if torch.cuda.is_available() else "cpu")
print(f"Using device: {device}")
if device.type == "cpu":
    torch.set_num_threads(os.cpu_count())

# Model name - using the OpenLLaMA model as you wanted
model_name = 'openlm-research/open_llama_3b'
print(f"Using model: {model_name}")

# Ensure the results directory exists
os.makedirs("results", exist_ok=True)

# Load tokenizer and model
print("Loading tokenizer and model...")
try:
    # Load tokenizer with SentencePiece
    tokenizer = LlamaTokenizer.from_pretrained(model_name, use_fast=False)
    print("Tokenizer loaded successfully!")
    
    # Load model with sharded weights - the key fix!
    model = LlamaForCausalLM.from_pretrained(
        model_name,
        torch_dtype=torch.float32,  # Use float32 for CPU compatibility
        device_map=None,  # Let the model load on the default device
        offload_folder="offload"  # Temporary folder for offloading
    )
    model = model.to(device)
    print("Model loaded successfully!")
except Exception as e:
    print(f"Error loading tokenizer or model: {e}")
    exit(1)  # Exit if loading fails

def calculate_perplexity(text, model, tokenizer):
    """Calculate perplexity of a text using the given model and tokenizer."""
    if not text.strip():
        return float('nan')  # Return NaN for empty inputs
    
    try:
        # Use the modern API for tokenizer
        inputs = tokenizer(text, return_tensors="pt").to(device)
        
        # Handle input sequences that might be too long
        if inputs.input_ids.size(1) > tokenizer.model_max_length:
            inputs.input_ids = inputs.input_ids[:, :tokenizer.model_max_length]
            if 'attention_mask' in inputs:
                inputs.attention_mask = inputs.attention_mask[:, :tokenizer.model_max_length]
        
        with torch.no_grad():
            outputs = model(**inputs, labels=inputs.input_ids)
        
        return torch.exp(outputs.loss).item()
    except Exception as e:
        print(f"Error calculating perplexity: {e}")
        return float('nan')

def process_file(filepath):
    """Process a single file and calculate perplexity metrics."""
    try:
        with open(filepath, 'r', encoding='utf-8', errors='replace') as file:
            lines = file.readlines()
        
        base_filename = os.path.basename(filepath)
        short_filename = '_'.join(base_filename.split('_')[:3])
        sentences = [unicodedata.normalize('NFKC', line.strip()) for line in lines if line.strip()]
        perplexities = []
            
        if method == 'sentence':
            for i in range(window_size - 1, len(sentences)):
                start_index = i - (window_size - 1)
                context = ' '.join(sentences[start_index:i+1])
                perp = calculate_perplexity(context, model, tokenizer)
                if not np.isnan(perp):
                    perplexities.append(perp)
                    print(f"Perplexity for context: {context[:50]}... is {perp}")

        elif method == 'word':
            words = ' '.join(sentences).split()
            for i in range(window_size - 1, len(words)):
                start_index = i - (window_size - 1)
                context = ' '.join(words[start_index:i+1])
                perp = calculate_perplexity(context, model, tokenizer)
                if not np.isnan(perp):
                    perplexities.append(perp)
                    print(f"Perplexity for context: {context[:50]}... is {perp}")
            
        if not perplexities:
            print(f"Warning: No valid perplexities calculated for {filepath}")
            return {
                'filename': short_filename,
                'word_mean': float('nan'),
                'word_std': float('nan'),
                'word_min': float('nan'),
                'word_max': float('nan'),
                'word_10th': float('nan'),
                'word_90th': float('nan')
            }

        results = {
            'filename': short_filename,
            'word_mean': np.mean(perplexities),
            'word_std': np.std(perplexities),
            'word_min': np.min(perplexities),
            'word_max': np.max(perplexities),
            'word_10th': np.percentile(perplexities, 10),
            'word_90th': np.percentile(perplexities, 90)
        }

        return results
    except Exception as e:
        print(f"Error processing file {filepath}: {e}")
        return {
            'filename': os.path.basename(filepath),
            'word_mean': float('nan'),
            'word_std': float('nan'),
            'word_min': float('nan'),
            'word_max': float('nan'),
            'word_10th': float('nan'),
            'word_90th': float('nan')
        }
        
def process_folder(folder_path, output_csv, batch_size=10):
    """Process all text files in a folder and save results to CSV."""
    if not os.path.exists(folder_path):
        print(f"Error: Folder {folder_path} not found")
        return
        
    files = [os.path.join(folder_path, f) for f in os.listdir(folder_path) if f.endswith(".txt")]
    total_files = len(files)
    
    if total_files == 0:
        print(f"No text files found in {folder_path}")
        return
        
    print(f"Found {total_files} files to process")
    
    for i in range(0, total_files, batch_size):
        batch_files = files[i:i + batch_size]
        batch_results = [process_file(filepath) for filepath in batch_files]
        
        df = pd.DataFrame(batch_results)
        if not os.path.exists(output_csv):
            df.to_csv(output_csv, index=False)
        else:
            df.to_csv(output_csv, mode='a', header=False, index=False)
        
        print(f"Processed {min(i + batch_size, total_files)} of {total_files} files...")

# Main execution
window_size = 10  # Just one iteration with window_size=10
# Use the model's simple name for the output file
model_simple_name = model_name.split('/')[-1]
output_csv = f"results/output_perp_{model_simple_name}_{method}_{window_size}.csv"
print(f"Using window size: {window_size}")

# Create the results directory if it doesn't exist
os.makedirs(os.path.dirname(output_csv), exist_ok=True)

process_folder(folder_path, output_csv, batch_size=batch_size)

print("Processing complete!")