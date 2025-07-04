import os
import subprocess

# === CONFIGURATION ===
TASKS = {
    'task1': {'make': True, 'run': True, 'binary': 'szymanski', 'clean': True},
    'task2': {'make': True, 'run': True, 'binary': 'szymanski', 'clean': True},
    'task3': {'make': True, 'run': True, 'binary': 'mutex_template', 'clean': True},
    'task4': {'make': True, 'run': True, 'binary': 'mutex_template', 'clean': True},
}

ROOT_DIR = os.getcwd()
OUT_DIR = os.path.join(ROOT_DIR, 'out')
os.makedirs(OUT_DIR, exist_ok=True)

for task, config in TASKS.items():
    print("=========================")
    print(f'Processing {task} :')
    task_dir = os.path.join(ROOT_DIR, task)
    out_file = f'out{task[-1]}'
    out_file_path = os.path.join(task_dir, out_file)
    dest_file_path = os.path.join(OUT_DIR, out_file)

    if config['make']:
        print(f'Building {task}...')
        print("==========================")
        subprocess.run(['make'], cwd=task_dir, check=True)

    if config['run']:
        print("==========================")
        print(f'Running {task}...')
        with open(out_file_path, 'w') as outfile:
            subprocess.run([f'./{config["binary"]}'], cwd=task_dir, stdout=outfile, check=True)
        print(f'Output written to [{out_file_path}]')
        print(f'Copying output to [{dest_file_path}]')
        subprocess.run(['cp', out_file_path, dest_file_path], check=True)

    if config['clean']:
        print(f'Cleaning {task}...')
        subprocess.run(['make', 'clean'], cwd=task_dir, check=True)

print('✅ All tasks completed!')
