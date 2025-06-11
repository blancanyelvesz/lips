import os

filename = 'all_transcripts_v2.txt'
controls_file = 'redcap_controls.txt'
patients_file = 'redcap_patients.txt'
controls_dir = 'transcripts_controls'
patients_dir = 'transcripts_patients'

os.makedirs(controls_dir, exist_ok=True)
os.makedirs(patients_dir, exist_ok=True)

with open(controls_file, 'r', encoding='utf-8') as file:
    controls_ids = set(line.strip() for line in file if line.strip())
with open(patients_file, 'r', encoding='utf-8') as file:
    patients_ids = set(line.strip() for line in file if line.strip())

recording = None
subject_id = None
current_lines = []
subject_list = []
saved_controls = []
saved_patients = []
not_relevant = []

def save_transcript(subject_id, recording, current_lines):
    if subject_id is not None and current_lines and recording == '1st':
        if subject_id in controls_ids:
            output_dir = controls_dir
            saved_controls.append(subject_id)
        elif subject_id in patients_ids:
            output_dir = patients_dir
            saved_patients.append(subject_id)
        else:
            not_relevant.append(subject_id)
            return
        output_filename = os.path.join(output_dir, f'{subject_id}.txt')
        with open(output_filename, 'w', encoding='utf-8') as output_file:
            output_file.writelines(current_lines[1:])
        print(f'Wrote {len(current_lines[1:])} lines to {output_filename}')


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

with open('saved_controls.txt', 'w', encoding='utf-8') as file:
    file.writelines(f"{subj}\n" for subj in sorted(set(saved_controls)))

with open('saved_patients.txt', 'w', encoding='utf-8') as file:
    file.writelines(f"{subj}\n" for subj in sorted(set(saved_patients)))
        
print(f"Total unique subjects: {len(set(subject_list))}")
print(f"Saved 1st recordings: {len(saved_controls) + len(saved_patients)} "
      f"(controls: {len(saved_controls)}, patients: {len(saved_patients)})")
print(f"Not relevant for the study: {len(set(not_relevant))}")
print(f"Subjects without 1st recording: {len(set(subject_list)) - (len(saved_controls) + len(saved_patients)) - len(set(not_relevant))}")