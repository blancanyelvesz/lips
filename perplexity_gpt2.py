#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# pip install torch transformers pandas
# 2025-01-08

"""
Perplexity Calculation: Sentence-Level and Word-Level
Handles token-based window sizes for word-level perplexity
"""

import os
import numpy as np
import pandas as pd
import torch
from tqdm import tqdm
from transformers import GPT2Tokenizer, GPT2LMHeadModel

# Define the window size for word-level perplexity
WINDOW_SIZE = 10

# Load pre-trained GPT2 model and tokenizer
MODEL_NAME = 'gpt2'
device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
tokenizer = GPT2Tokenizer.from_pretrained(MODEL_NAME)
model = GPT2LMHeadModel.from_pretrained(MODEL_NAME).to(device)

def calculate_sentence_perplexity(sentence, model, tokenizer):
    encodings = tokenizer(sentence, return_tensors="pt").to(device)
    with torch.no_grad():
        outputs = model(**encodings, labels=encodings.input_ids)
        loss = outputs.loss
    return torch.exp(loss).item()

def calculate_word_level_perplexity(words, model, tokenizer, window_size):
    word_perplexities = []
    for i in tqdm(range(len(words)), desc="Calculating Word-Level Perplexity"):
        start_index = max(0, i - window_size)
        context = ' '.join(words[start_index:i+1])
        encodings = tokenizer(context, return_tensors="pt").to(device)
        input_ids = encodings.input_ids
        target_ids = input_ids.clone()
        target_ids[:, :-1] = -100
        
        with torch.no_grad():
            outputs = model(input_ids, labels=target_ids)
            loss = outputs.loss
        word_perplexities.append(torch.exp(loss).item())
    return word_perplexities

def process_file(filepath):
    with open(filepath, 'r', encoding='utf-8') as file:
        lines = file.readlines()
    
    sentences = [line.strip() for line in lines if line.strip()]
    sentence_perplexities = [calculate_sentence_perplexity(sentence, model, tokenizer) for sentence in sentences]
    
    words = ' '.join(sentences).split()
    word_perplexities = calculate_word_level_perplexity(words, model, tokenizer, WINDOW_SIZE)
    
    return {
        'filename': os.path.basename(filepath),
        'sentence_mean': np.mean(sentence_perplexities),
        #'sentence_std': np.std(sentence_perplexities),
        #'sentence_min': np.min(sentence_perplexities),
        #'sentence_max': np.max(sentence_perplexities),
        #'sentence_10th': np.percentile(sentence_perplexities, 10),
        #'sentence_90th': np.percentile(sentence_perplexities, 90),
        'word_mean': np.mean(word_perplexities),
        #'word_std': np.std(word_perplexities),
        #'word_min': np.min(word_perplexities),
        #'word_max': np.max(word_perplexities),
        #'word_10th': np.percentile(word_perplexities, 10),
        #'word_90th': np.percentile(word_perplexities, 90)
    }

def process_folder(folder_path):
    results = []
    for filename in os.listdir(folder_path):
        if filename.endswith(".txt"):
            filepath = os.path.join(folder_path, filename)
            #print(f"Processing {filepath}...")
            results.append(process_file(filepath))
    return results

def save_results_to_csv(results, output_csv):
    df = pd.DataFrame(results)
    df.to_csv(output_csv, index=False)

# Define folder path and output CSV filename
folder_path = "transcripts_for_students_small/no_questions"
output_csv = f"results/output_perplexities_0108_{MODEL_NAME}_window{WINDOW_SIZE}.csv"

# Process the folder and save results to CSV
if __name__ == "__main__":
    results = process_folder(folder_path)
    save_results_to_csv(results, output_csv)
    
