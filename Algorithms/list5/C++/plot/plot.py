import re
from pathlib import Path
from statistics import mean
import matplotlib.pyplot as plt
import numpy as np
from collections import defaultdict

ROOT = Path(__file__).resolve().parent
RESULTS_DIR = ROOT / "../results"

# ──────────────────────────── helpers ──────────────────────────── #

def parse_mst(path: Path):
    """
    Parse *prim* or *kruskal* log.
    Returns {n: list[duration_sec]}  (floats)
    """
    by_n = {}
    n_re   = re.compile(r'^n\s*=\s*(\d+)\s*:', re.I)
    dur_re = re.compile(r'Duration:\s*([0-9.]+)')
    cur_n  = None

    for line in path.read_text().splitlines():
        m = n_re.match(line)
        if m:
            cur_n = int(m.group(1))
            by_n.setdefault(cur_n, [])
            continue

        m = dur_re.search(line)
        if m and cur_n is not None:
            by_n[cur_n].append(float(m.group(1)))

    return by_n


def parse_heap(path: Path):
    """
    Parse CSV-style lines from *heap_exp_results.txt*.

    Returns
    -------
    inserts  : dict[int, float]   # mean comparisons‑per‑insert (Heap 1) keyed by N
    extracts : dict[int, float]   # mean comparisons‑per‑extract        keyed by N
    """
    inserts = defaultdict(list)
    extracts = defaultdict(list)

    for line in path.read_text().splitlines():
        if not line.startswith("CSV,"):
            continue

        parts = line.strip().split(',')
        if len(parts) != 7:
            continue  # malformed

        _, n_str, _, avg_H1, _, _, avg_extract = parts
        n = int(n_str)
        inserts[n].append(float(avg_H1))
        extracts[n].append(float(avg_extract))

    inserts  = {n: mean(vals) for n, vals in inserts.items()}
    extracts = {n: mean(vals) for n, vals in extracts.items()}
    return inserts, extracts



def parse_sched(path: Path, keep=100):
    """
    Parse *scheduling_exp_results.txt*.
    Returns {n: list[min_rounds]} (first *keep* values each)
    """
    n_re   = re.compile(r'^n\s*=\s*(\d+)\s*:', re.I)
    val_re = re.compile(r'Min rounds:\s*(\d+)')
    by_n   = {}
    cur_n  = None

    for line in path.read_text().splitlines():
        m = n_re.match(line)
        if m:
            cur_n = int(m.group(1))
            by_n.setdefault(cur_n, [])
            continue
        m = val_re.search(line)
        if m and cur_n is not None and len(by_n[cur_n]) < keep:
            by_n[cur_n].append(int(m.group(1)))
    return by_n


# ──────────────────────────── parsing ──────────────────────────── #

prim_data  = parse_mst(RESULTS_DIR / 'prim_exp_results.txt')
krus_data  = parse_mst(RESULTS_DIR / 'kruskal_exp_results.txt')
ins, exts  = parse_heap(RESULTS_DIR / 'heap_exp_results.txt')
sched_data = parse_sched(RESULTS_DIR / 'scheduling_exp_results.txt')

# ──────────────────────────── Figure 1 ──────────────────────────── #
fig1 = plt.figure()
ax1  = fig1.add_subplot(1, 1, 1)

for label, data, style in [('Prim', prim_data, 'o-'),
                           ('Kruskal', krus_data, 's--')]:
    n_vals = sorted(data)
    means  = [mean(data[n]) for n in n_vals]
    ax1.plot(n_vals, means, style, label=f'{label} (mean of 5 trials)')

ax1.set_xlabel('n  (number of vertices)')
ax1.set_ylabel('Duration  (seconds)')
ax1.set_title('Prim vs. Kruskal — MST Construction Time')
ax1.legend()
ax1.grid(True)

# ──────────────────────────── Figure 2 ──────────────────────────── #
fig2 = plt.figure()
ax2  = fig2.add_subplot(1, 1, 1)

n_heap = sorted(ins.keys() & exts.keys())
ax2.plot(n_heap, [ins[n]  for n in n_heap], 'd-', label='Insert')
ax2.plot(n_heap, [exts[n] for n in n_heap], 'v--', label='Extract‑Min')

ax2.set_xlabel('N  (heap size)')
ax2.set_ylabel('Average comparisons per operation')
ax2.set_title('Binary‑Heap Performance')
ax2.legend()
ax2.grid(True)

# ──────────────────────────── Figure 3 ──────────────────────────── #
fig3 = plt.figure(constrained_layout=True)
ax3  = fig3.add_subplot(1, 1, 1)

n_sched = sorted(sched_data)
# scatter every individual value, jitter x slightly for visibility
for idx, n in enumerate(n_sched):
    vals = sched_data[n]
    x    = np.full_like(vals, idx) + (np.random.rand(len(vals)) - 0.5) * 0.2
    ax3.scatter(x, vals, alpha=0.35, s=10)

mins  = [min(sched_data[n]) for n in n_sched]
maxes = [max(sched_data[n]) for n in n_sched]
means = [mean(sched_data[n]) for n in n_sched]
idxs  = np.arange(len(n_sched))

ax3.plot(idxs, mins,  'k--', label='Lowest')
ax3.plot(idxs, maxes, 'k--', label='Highest')
ax3.plot(idxs, means, 'k-',  label='Average')

ax3.set_xticks(idxs)
ax3.set_xticklabels([str(n) for n in n_sched])
ax3.set_xlabel('N  (problem size)')
ax3.set_ylabel('“Min rounds” per trial')
ax3.set_title('Scheduling — dispersion of 100 trials per N')
ax3.legend()
ax3.grid(True, axis='y')

PLOT_DIR = ROOT / "plots"
PLOT_DIR.mkdir(exist_ok=True)

fig1.savefig(PLOT_DIR / "mst_durations.png",  dpi=300, bbox_inches="tight")
fig2.savefig(PLOT_DIR / "heap_comparisons.png", dpi=300, bbox_inches="tight")
fig3.savefig(PLOT_DIR / "scheduling_min_rounds.png", dpi=300, bbox_inches="tight")

plt.show()
