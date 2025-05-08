import subprocess
import time
import matplotlib.pyplot as plt
import re

runs = 50
timeout_sec = 20

times = []
steps = []
heuristics = []
nodes_visited = []
timeouts = 0

for i in range(runs):
    print(f"Running iteration {i + 1}...")
    try:
        start = time.time()
        result = subprocess.run(
            ['../aStar/app/main'],
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            timeout=timeout_sec,
            text=True
        )
        end = time.time()
        output = result.stdout.strip()

        if "Goal found!" in output:
            match = re.search(
                r"Optimal steps: (\d+)\s+Time taken: (\d+) ms\s+Initial state heuristic: (\d+)\s+Nodes visited: (\d+)",
                output
            )
            if match:
                steps.append(int(match.group(1)))
                times.append(int(match.group(2)) / 1000)  # ms -> s
                heuristics.append(int(match.group(3)))
                nodes_visited.append(int(match.group(4)))
            else:
                print("Output format mismatch.")
                steps.append(-1)
                times.append(end - start)
                heuristics.append(-1)
                nodes_visited.append(-1)
        else:
            print("Goal not found!")
            steps.append(-1)
            times.append(end - start)
            heuristics.append(-1)
            nodes_visited.append(-1)

    except subprocess.TimeoutExpired:
        print("Run timed out.")
        steps.append(-1)
        times.append(timeout_sec)
        heuristics.append(-1)
        nodes_visited.append(-1)
        timeouts += 1

print(f"\nCompleted with {timeouts} timeouts.\n")

# --- Filter out timed-out or failed runs ---
valid_times = [t for t, s in zip(times, steps) if s != -1]
valid_steps = [s for s in steps if s != -1]
valid_nodes = [n for n in nodes_visited if n != -1]

# --- Binning Execution Time (in ms) ---
time_bins = ['<100ms', '100–300ms', '300–500ms', '500ms–1s', '1s–5s', '>5s']
time_counts = [0] * len(time_bins)
for t in valid_times:
    ms = t * 1000
    if ms < 100:
        time_counts[0] += 1
    elif ms < 300:
        time_counts[1] += 1
    elif ms < 500:
        time_counts[2] += 1
    elif ms < 1000:
        time_counts[3] += 1
    elif ms < 5000:
        time_counts[4] += 1
    else:
        time_counts[5] += 1

# --- Binning Steps ---
step_bins = [f"{i}-{i+10}" for i in range(0, 80, 10)]
step_counts = [0] * len(step_bins)
for s in valid_steps:
    index = min(s // 10, len(step_bins) - 1)
    step_counts[index] += 1

# --- Binning Nodes Visited ---
node_bins = ['<10k', '10k–100k', '100k–300k', '300k–600k', '600k–1000k', '>1000k']
node_counts = [0] * len(node_bins)
for n in valid_nodes:
    if n < 10_000:
        node_counts[0] += 1
    elif n < 100_000:
        node_counts[1] += 1
    elif n < 300_000:
        node_counts[2] += 1
    elif n < 600_000:
        node_counts[3] += 1
    elif n < 1_000_000:
        node_counts[4] += 1
    else:
        node_counts[5] += 1

# --- Plotting ---
plt.figure(figsize=(16, 10))
plt.suptitle(f"A* Search Run Stats (timeout = {timeout_sec}s)", fontsize=16)

# Plot Execution Time bins
plt.subplot(3, 1, 1)
plt.bar(time_bins, time_counts, color='skyblue')
plt.title("Execution Time Distribution")
plt.ylabel("Runs")
plt.xlabel("Execution Time")

# Plot Steps bins
plt.subplot(3, 1, 2)
plt.bar(step_bins, step_counts, color='orange')
plt.title("Optimal Steps Distribution")
plt.ylabel("Runs")
plt.xlabel("Steps")

# Plot Nodes Visited bins
plt.subplot(3, 1, 3)
plt.bar(node_bins, node_counts, color='purple')
plt.title("Nodes Visited Distribution")
plt.ylabel("Runs")
plt.xlabel("Visited Nodes")

plt.tight_layout(rect=[0, 0.03, 1, 0.95])
plt.savefig('aStar_run_stats.png')
plt.show()
