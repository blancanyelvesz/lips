"""
#2025-04-30
This script calculates the perplexity of text files using the pre-trained language model Falcon available in huggingface 🤗 for contexts of sizes 10, 20, 30, 40, and 50.
This context consist of words or sentences depending on the --method used. 
The perplexity is calculated for each context and the results are saved to a CSV file in the 'results' folder.
Read README.md for more information. 
# TO DO: WRITE A READ ME. 


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
from transformers import AutoTokenizer, AutoModelForCausalLM 
from docopt import docopt

args = docopt(__doc__, version = '1.0 - Falcon')
method = args['--method']
folder_path = args['--folder']
batch_size = 10

device = torch.device("cuda:0" if torch.cuda.is_available() else "cpu")
print(f"Using device: {device}")
if device.type == "cpu":
    torch.set_num_threads(os.cpu_count())


model_name = "tiiuae/Falcon3-7B-Base"
model_simple_name = model_name.split('/')[-1]
tokenizer = AutoTokenizer.from_pretrained(model_name)
model = AutoModelForCausalLM.from_pretrained(
    model_name,
    device_map=device,
    torch_dtype=torch.float16 if device.type == "cuda" else torch.float32,
    trust_remote_code=True,
    #  cache_dir = "C:\\Users\\AlonsoGoncB\\music"
     )
tokenizer = tokenizer.to(device)
model = model.to(device)


# model = AutoModelForCausalLM.from_pretrained(model_name, torch_dtype="auto", device_map="auto")
# model = AutoModelForCausalLM.from_pretrained(model_name, device_map="auto", load_in_4bit=True)
# model = AutoModelForCausalLM.from_pretrained(model_name, use_auth_token=True).to(device)


def calculate_perplexity(text, model, tokenizer):
    if not text.strip():
        return float('nan')  # Return NaN for empty inputs
    inputs = tokenizer(text, return_tensors="pt").to(device)
    with torch.no_grad():
        outputs = model(**inputs, labels=inputs.input_ids)
    return torch.exp(outputs.loss).item()

def calculate_perplexity2(text, model, tokenizer):
    tokenize_input = tokenizer.tokenize(text)
    tensor_input = torch.tensor([tokenizer.convert_tokens_to_ids(tokenize_input)])
    tensor_input = tensor_input.to(device)
    with torch.no_grad():
        outputs = model(tensor_input, labels=tensor_input)
    loss, _ = outputs[:2]
    return torch.exp(loss).item()

def process_file(filepath):
    with open(filepath, 'r', encoding='utf-8') as file:
        lines = file.readlines()
    base_filename = os.path.basename(filepath)
    short_filename = '_'.join(base_filename.split('_')[:3])
    sentences = [unicodedata.normalize('NFKC', line.strip()) for line in lines if line.strip()]
    perplexities = []
        
    if method == 'sentence':
        for i in range(window_size - 1, len(sentences)):
            start_index = i - (window_size - 1)
            context = ' '.join(sentences[start_index:i+1])
            perplexities.append(calculate_perplexity(context, model, tokenizer))
            print(f"Perplexity for context: {context[:50]}... is {perplexities[-1]}")

    elif method == 'word':
        words = ' '.join(sentences).split()
        for i in range(window_size - 1, len(words)):
            start_index = i - (window_size - 1)
            context = ' '.join(words[start_index:i+1])
            perplexities.append(calculate_perplexity2(context, model, tokenizer))
            print(f"Perplexity for context: {context[:50]}... is {perplexities[-1]}")
        
    perplexities = [p for p in perplexities if not np.isnan(p)]

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


# for n in range(10, 51, 10): # changed to 11 so there is only one iteration
#     window_size = n
#     output_csv = f"results/output_perp_{model_simple_name}_{method}_{window_size}.csv"
#     print(f"Using window size: {window_size}")
#     process_folder(folder_path, output_csv, batch_size=batch_size)

window_size = 10
output_csv = f"results/output_perp0430_{model_simple_name}_{method}_{window_size}.csv"
print(f"Using window size: {window_size}")
process_folder(folder_path, output_csv, batch_size=batch_size)