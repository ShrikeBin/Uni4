import os
import matplotlib.pyplot as plt
from pathlib import Path

# Create plots directory if it doesn't exist
Path("../plots").mkdir(parents=True, exist_ok=True)

def parse_file(filepath):
    try:
        with open(filepath, 'r') as f:
            content = f.read()
        
        blocks = content.strip().split('-----\n')
        blocks_data = []
        
        for block in blocks:
            if not block.strip():
                continue
            lines = block.strip().split('\n')
            n, time, comparisons, comp_per_n = None, None, None, None
            for line in lines:
                line = line.strip()
                if line.startswith('N: '):
                    n = int(line.split('N: ')[1].split()[0])
                elif line.startswith('Avg Time: '):
                    time = float(line.split('Avg Time: ')[1].split()[0])
                elif line.startswith('Comparisons: '):
                    comparisons = int(line.split('Comparisons: ')[1])
                elif line.startswith('Comparisons/n: '):
                    comp_per_n = float(line.split('Comparisons/n: ')[1])
            
            # Fixed the None check
            if all(v is not None for v in [n, time, comparisons, comp_per_n]):
                blocks_data.append({'n': n, 'time': time, 'comparisons': comparisons, 'comp_per_n': comp_per_n})
        
        blocks_data.sort(key=lambda x: x['n'])
        return {
            'n': [x['n'] for x in blocks_data],
            'time': [x['time'] for x in blocks_data],
            'comparisons': [x['comparisons'] for x in blocks_data],
            'comparisons_per_n': [x['comp_per_n'] for x in blocks_data]
        }
    except Exception as e:
        print(f"Error parsing {filepath}: {str(e)}")
        return None

def plot_metrics(groups, title, filename):
    metrics = ['time', 'comparisons', 'comparisons_per_n']
    labels = ['Average Time (sec)', 'Comparisons', 'Comparisons per n']
    
    fig, axs = plt.subplots(3, 1, figsize=(10, 15))
    for i, metric in enumerate(metrics):
        ax = axs[i]
        for (data, label) in groups:
            if data is None or metric not in data:
                continue
            ax.plot(data['n'], data[metric], label=label)
        ax.set_xlabel('N')
        ax.set_ylabel(labels[i])
        ax.legend()
        ax.grid(True)
    plt.suptitle(title)
    plt.tight_layout()
    plt.savefig(filename)
    plt.close()

# Read all data
data = {}
results_dir = Path('../../results/')

if not results_dir.exists():
    raise FileNotFoundError(f"Results directory not found: {results_dir}")

for filename in results_dir.glob('*.txt'):
    parts = filename.stem.split('_')
    algorithm = parts[0]
    variant, param = None, None
    
    try:
        if algorithm == 'paramselect' and len(parts) >= 3:
            variant = parts[1]
            param = int(parts[2])
        elif len(parts) >= 2:
            variant = parts[1]
    except (IndexError, ValueError) as e:
        print(f"Skipping {filename}: {str(e)}")
        continue
    
    key = (algorithm, variant, param if algorithm == 'paramselect' else None)
    parsed = parse_file(filename)
    if parsed:
        data[key] = parsed

# Plotting code with existence checks
# Plot 1: dpquick normal vs better
try:
    dpquick_normal = data.get(('dpquick', 'normal', None))
    dpquick_better = data.get(('dpquick', 'better', None))
    groups = [(dpquick_normal, 'Normal'), (dpquick_better, 'Better')]
    plot_metrics(groups, 'dpquick: Normal vs Better', '../plots/plot1.png')
except KeyError as e:
    print(f"Missing data for Plot 1: {str(e)}")

# Plot 2: quick normal vs better
try:
    quick_normal = data.get(('quick', 'normal', None))
    quick_better = data.get(('quick', 'better', None))
    groups = [(quick_normal, 'Normal'), (quick_better, 'Better')]
    plot_metrics(groups, 'quick: Normal vs Better', '../plots/plot2.png')
except KeyError as e:
    print(f"Missing data for Plot 2: {str(e)}")

# Plot 3: binsearch low vs mid vs top
try:
    bin_low = data.get(('binsearch', 'low', None))
    bin_mid = data.get(('binsearch', 'mid', None))
    bin_top = data.get(('binsearch', 'top', None))
    groups = [(bin_low, 'Low'), (bin_mid, 'Mid'), (bin_top, 'Top')]
    plot_metrics(groups, 'binsearch: Low vs Mid vs Top', '../plots/plot3.png')
except KeyError as e:
    print(f"Missing data for Plot 3: {str(e)}")

# Plot 4: randselect vs select (low, mid, top)
try:
    groups = []
    for variant in ['low', 'mid', 'top']:
        rand = data.get(('randselect', variant, None))
        sel = data.get(('select', variant, None))
        groups.append((rand, f'randselect {variant}'))
        groups.append((sel, f'select {variant}'))
    plot_metrics(groups, 'randselect vs Select (Low, Mid, Top)', '../plots/plot4.png')
except KeyError as e:
    print(f"Missing data for Plot 4: {str(e)}")

# Plot 5: paramselect for each variant (low, mid, top)
params = [3, 5, 10, 15, 30, 50, 80]
for variant in ['low', 'mid', 'top']:
    try:
        groups = []
        for p in params:
            key = ('paramselect', variant, p)
            if key in data:
                groups.append((data[key], f'k={p}'))
        if groups:  # Only plot if we have data
            plot_metrics(groups, f'paramselect {variant.capitalize()} (k variations)', f'../plots/plot5_{variant}.png')
    except KeyError as e:
        print(f"Missing data for Plot 5 ({variant}): {str(e)}")