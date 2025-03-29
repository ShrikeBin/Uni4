import os
import re
import matplotlib.pyplot as plt
import matplotlib.ticker as mticker

def parse_file(filepath):
    """Parses a sorting algorithm stats file and returns a dictionary of results."""
    data = {}
    with open(filepath, 'r') as file:
        content = file.read()
        matches = re.findall(r'N: (\d+) mode: (\w+)\nAvg Time: ([\d\.e-]+) sec.\nComparisons: (\d+)\nSwaps: (\d+)\nComparisons/n: ([\d\.e-]+)\nSwaps/n: ([\d\.e-]+)', content)
        
        for N, mode, time, comparisons, swaps, comparisons_per_n, swaps_per_n in matches:
            N = int(N)
            time = float(time)
            comparisons = int(comparisons)
            swaps = int(swaps)
            comparisons_per_n = float(comparisons_per_n)
            swaps_per_n = float(swaps_per_n)
            
            if N not in data:
                data[N] = {}
            data[N][mode] = {
                'time': time, 
                'comparisons': comparisons, 
                'swaps': swaps, 
                'comparisons_per_n': comparisons_per_n, 
                'swaps_per_n': swaps_per_n
            }
    
    return data

def plot_stats(sort_name, data):
    """Generates and saves plots for a sorting algorithm."""
    Ns = sorted(data.keys())
    modes = ['asc', 'desc', 'random']
    metrics = ['time', 'comparisons', 'swaps', 'comparisons_per_n', 'swaps_per_n']
    
    fig, axes = plt.subplots(len(metrics), 1, figsize=(10, 15))
    fig.suptitle(f'Sorting Performance: {sort_name}', fontsize=16)
    
    for i, metric in enumerate(metrics):
        ax = axes[i]
        for mode in modes:
            values = [data[N].get(mode, {}).get(metric, None) for N in Ns]
            if any(v is not None for v in values):  # Only plot if there's data
                ax.plot(Ns, values, label=mode, linewidth=2)
        ax.set_ylabel(metric.replace('_', ' ').capitalize())
        ax.set_xlabel('N')
        ax.legend()
        ax.grid()
        ax.yaxis.set_major_formatter(mticker.ScalarFormatter())
        ax.ticklabel_format(style='plain', axis='y')
    
    plt.tight_layout(rect=[0, 0, 1, 0.96])
    plt.savefig(f'../plots/{sort_name}.png')
    plt.close()

def plot_comparison(all_data, metric):
    """Generates a comparison plot for all sorting algorithms, excluding insertion sort."""
    fig, ax = plt.subplots(figsize=(10, 6))
    for sort_name, data in all_data.items():
        if sort_name.lower() == "insertion":
            continue  # Skip insertion sort in comparisons
        Ns = sorted(data.keys())
        values = [data[N].get('random', {}).get(metric, None) for N in Ns]
        if any(v is not None for v in values):
            ax.plot(Ns, values, label=sort_name, linewidth=2)
    
    ax.set_ylabel(metric.replace('_', ' ').capitalize())
    ax.set_xlabel('N')
    ax.legend()
    ax.grid()
    ax.yaxis.set_major_formatter(mticker.ScalarFormatter())
    ax.ticklabel_format(style='plain', axis='y')
    plt.title(f'Comparison of Sorting Algorithms: {metric.replace("_", " ").capitalize()}')
    plt.savefig(f'../plots/comparison_{metric}.png')
    plt.close()

def main():
    """Main function to process files and generate plots."""
    results_dir = '../results/'
    output_dir = '../plots/'
    os.makedirs(output_dir, exist_ok=True)
    
    all_data = {}
    
    for filename in os.listdir(results_dir):
        match = re.match(r'([a-zA-Z]+)_stats\d+\.txt', filename)
        if match:
            sort_name = match.group(1)
            filepath = os.path.join(results_dir, filename)
            print(f'Processing {filename}...')
            data = parse_file(filepath)
            all_data[sort_name] = data
            plot_stats(sort_name, data)
    
    for metric in ['time', 'comparisons', 'swaps', 'comparisons_per_n', 'swaps_per_n']:
        plot_comparison(all_data, metric)
    
    print('Plots saved in ../plots/')

if __name__ == "__main__":
    main()
