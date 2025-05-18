import re
import matplotlib.pyplot as plt
import os

# Metrics to extract
metrics = [
    "Avg comparisons", "Avg pointer reads", "Avg pointer writes",
    "Avg max height", "Avg operation cost", "Max operation cost"
]

def parse_file(filepath):
    with open(filepath, 'r') as f:
        content = f.read()

    tree_type = re.search(r"\[([^\]]+)\]", content).group(1)
    tests = {1: {}, 2: {}}

    blocks = re.split(r"\[.*?\] n = (\d+)", content)[1:]
    for i in range(0, len(blocks), 2):
        n = int(blocks[i])
        data = blocks[i+1]

        for test_num in [1, 2]:
            test_data = re.search(
                rf"Test {test_num}:.*?ran 20 times\.(.*?)(?=Test|\Z)",
                data, re.DOTALL
            )
            if test_data:
                lines = test_data.group(1).strip().splitlines()
                for line in lines:
                    for metric in metrics:
                        if metric in line:
                            value = int(re.search(r"(\d+)", line).group(1))
                            tests[test_num].setdefault(metric, []).append((n, value))
    return tree_type, tests

def collect_all_data(filepaths):
    all_data = {1: {}, 2: {}}

    for path in filepaths:
        tree_type, tests = parse_file(path)
        for test_num in [1, 2]:
            for metric in tests[test_num]:
                all_data[test_num].setdefault(metric, {})[tree_type] = sorted(tests[test_num][metric], key=lambda x: x[0])
    return all_data

def plot_all_metrics(all_data, out_dir):
    os.makedirs(out_dir, exist_ok=True)

    for test_num in [1, 2]:
        for metric in metrics:
            plt.figure()
            for tree_type, data in all_data[test_num].get(metric, {}).items():
                x_vals = [x for x, _ in data]
                y_vals = [y for _, y in data]
                plt.plot(x_vals, y_vals, marker='o', label=tree_type)

            plt.title(f"Test {test_num} - {metric}")
            plt.xlabel("n")
            plt.ylabel(metric)
            plt.legend()
            plt.grid(True)
            filename = f"Test{test_num}_{metric.replace(' ', '_')}.png"
            plt.savefig(os.path.join(out_dir, filename))
            plt.close()

def main(filepaths, out_dir="plots"):
    all_data = collect_all_data(filepaths)
    plot_all_metrics(all_data, out_dir)

if __name__ == "__main__":
    files = ["Binary.txt", "Splay.txt", "RBT.txt"]  # Replace with your actual file names
    main(files)
