import numpy as np
from loader import load_images_uncompressed, load_labels_uncompressed
from dbscan import dbscan, pca           # the module you already have
from collections import Counter

# ---------- 1. load EMNIST (balanced split is small; digits split = 240k) ----------
X_raw = np.array(load_images_uncompressed('./data/images.idx3-ubyte'),
                 dtype=np.float32)
y_true = np.array(load_labels_uncompressed('./data/labels.idx1-ubyte'),
                  dtype=np.int8)
X_raw /= 255.0

# ---------- 2. (optional) dimensionality reduction ----------
X = pca(X_raw, n_components=50)          # ~50 dims usually enough

# ---------- 3. sweep eps/min_pts to find the best setting ----------
candidates = []

# Nonlinear eps steps: fine granularity up to 4, coarser after
eps_values = (
    [1.0 + 0.1 * i for i in range(20)] +   # 1.0 to 2.9 by 0.1 steps (fine)
    [3.0 + 0.25 * i for i in range(12)] +  # 3.0 to 5.75 by 0.25 steps
    [6.0 + 0.5 * i for i in range(5)]      # 6.0 to 8.0 by 0.5 steps
)

min_pts_values = list(range(1, 16))  # 1 to 15

for eps in eps_values:
    for min_pts in min_pts_values:
        candidates.append((eps, min_pts))


# ---------- 4. run DBSCAN for each candidate and evaluate ----------
best = None
best_stats = None
run = 1
for eps, min_pts in candidates:
    labels = dbscan(X, eps=eps, min_pts=min_pts)
    K = labels.max()          # number of clusters
    noise = np.sum(labels == -1)
    if not (10 <= K <= 30):   # hard constraint from spec
        continue

    # ----- per‑cluster majority vote -----
    majority = {}
    for k in range(1, K + 1):
        digits = y_true[labels == k]
        if digits.size:
            majority[k] = Counter(digits).most_common(1)[0][0]

    # ----- metrics -----
    correct_core = sum(
        (majority[k] == true) for k, true in zip(labels, y_true) if k != -1
    )
    total_core   = len(y_true) - noise
    acc          = correct_core / len(y_true)
    mis_in_core  = 1.0 - (correct_core / total_core) if total_core else 0.

    stats = (acc, noise / len(y_true), mis_in_core, K, eps, min_pts)
    print(f"Stats {run}:")
    run += 1
    print(f"eps={eps}, min_pts={min_pts}, clusters={K}, "
          f"acc={acc:6.2%}, noise_pct={noise / len(y_true):6.2%}, "
          f"mis_pct={mis_in_core:6.2%}")
    if best is None or acc > best_stats[0]:
        best = labels
        best_stats = stats

# ---------- 4. pretty‑print final numbers ----------
acc, noise_pct, mis_pct, K, eps, min_pts = best_stats
print(f"BEST PARAMS: eps={eps}, min_pts={min_pts}, clusters={K}")
print(f"Accuracy:          {acc:6.2%}")
print(f"Noise percentage:  {noise_pct:6.2%}")
print(f"Mis‑labels in clusters: {mis_pct:6.2%}")
