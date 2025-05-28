import os

filename = 'all_transcripts_v2.txt'
output_dir = 'transcripts1'
os.makedirs(output_dir, exist_ok = True)


recording = None
subject_id = None
current_lines = []
subject_list = []
saved_transcripts = []

def save_transcript(subject_id, recording, current_lines):
    if subject_id is not None and current_lines and recording == '1st':
        output_filename = os.path.join(output_dir, f'{subject_id}.txt')
        with open(output_filename, 'w', encoding='utf-8') as output_file:
            output_file.writelines(current_lines)
        print(f'Wrote {len(current_lines)} lines to {output_filename}')
        saved_transcripts.append(subject_id)


with open(filename, 'r', encoding='utf-8') as file:
    for line in file:
        if line.startswith('::: ./'):
            save_transcript(subject_id, recording, current_lines)
            
            subject_id = line.strip().split('/')[1]
            recording = line.strip().split('/')[2].split('_')[0]
            current_lines = [line]
            subject_list.append(subject_id)
        else:
            current_lines.append(line)

    save_transcript(subject_id, recording, current_lines)
        
    print(f'Processed {len(set(subject_list))} subjects in total')
    #print(set(subject_list))
    print(f'and saved {len(saved_transcripts)} transcripts for 1st recordings')

    # subjects_with_no_1st_recording = set(subject_list) - set(saved_subjects)
    # print(subjects_with_no_1st_recording)