import subprocess
import time
import matplotlib.pyplot as plt
import re

runs = 1000
timeout_sec = 40

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
time_bins = ['<50ms','50-100ms', '100–300ms', '300–500ms', '500ms–1000ms', '1s–5s', '5s-30s','>30s','Timeout']
time_counts = [0] * len(time_bins)
time_counts[8] = timeouts  # Timeout count
for t in valid_times:
    ms = t * 1000
    if ms < 50:
        time_counts[0] += 1
    elif ms < 100:
        time_counts[1] += 1
    elif ms < 300:
        time_counts[2] += 1
    elif ms < 500:
        time_counts[3] += 1
    elif ms < 1000:
        time_counts[4] += 1
    elif ms < 5000:
        time_counts[5] += 1
    elif ms < 30000:
        time_counts[6] += 1
    else:
        time_counts[7] += 1

# --- Binning Steps ---
step_bins = [f"{i}-{i+5}" for i in range(0, 80, 5)]
step_counts = [0] * len(step_bins)
for s in valid_steps:
    index = min(s // 5, len(step_bins) - 1)
    step_counts[index] += 1

# --- Binning Nodes Visited ---
node_bins = ['<1k', '1k-10k', '10k–50k','50k-100k', '100k–200k', '200k–500k', '500k–1M', '1M-5M', '>5M']
node_counts = [0] * len(node_bins)
for n in valid_nodes:
    if n < 1_000:
        node_counts[0] += 1
    elif n < 10_000:
        node_counts[1] += 1
    elif n < 50_000:
        node_counts[2] += 1
    elif n < 100_000:
        node_counts[3] += 1
    elif n < 200_000:
        node_counts[4] += 1
    elif n < 500_000:
        node_counts[5] += 1
    elif n < 1_000_000:
        node_counts[6] += 1
    elif n < 5_000_000:
        node_counts[7] += 1
    else:
        node_counts[8] += 1

# --- Plotting ---
plt.figure(figsize=(16, 10))
plt.suptitle(f"A* 15 puzzle Search Run Stats\n (timeout at {timeout_sec}s)\n  ({runs} runs)", fontsize=16)

# Plot Execution Time bins
ax1 = plt.subplot(3, 1, 1)
ax1.bar(time_bins, time_counts, color='skyblue')
ax1.set_title("Execution Time Distribution")
ax1.set_ylabel("Runs")
ax1.set_xlabel("Execution Time")
ax1.grid(axis='y')

# Plot Steps bins
ax2 = plt.subplot(3, 1, 2)
ax2.bar(step_bins, step_counts, color='orange')
ax2.set_title("Optimal Steps Distribution")
ax2.set_ylabel("Runs")
ax2.set_xlabel("Steps")
ax2.grid(axis='y')

# Plot Nodes Visited bins
ax3 = plt.subplot(3, 1, 3)
ax3.bar(node_bins, node_counts, color='purple')
ax3.set_title("Nodes Visited Distribution")
ax3.set_ylabel("Runs")
ax3.set_xlabel("Visited Nodes")
ax3.grid(axis='y')

plt.tight_layout(rect=[0, 0.03, 1, 0.95])
plt.savefig(f"aStar_run_stats_{runs}_runs.png")
plt.show()
