import numpy as np

def euclidean(a, b):
    # Compute Euclidean distances between vector a and array b
    return np.linalg.norm(a - b, axis=1)

def initialize_centroids_kmeanspp(data, k):
    n_samples = data.shape[0]
    centroids = [data[np.random.choice(n_samples)]]
    for _ in range(k - 1):
        distances = np.array([min(np.linalg.norm(x - c) for c in centroids) for x in data])
        probs = distances / distances.sum()
        cumulative_probs = np.cumsum(probs)
        r = np.random.rand()
        idx = np.searchsorted(cumulative_probs, r)
        centroids.append(data[idx])
    return np.array(centroids)

def kmeans(data, k, max_iter=1000, tol=1e-4):
    best_inertia = float('inf')
    best_centroids = None
    best_labels = None

    for attempt in range(5):
        print(f"Attempt {attempt + 1}")
        centroids = initialize_centroids_kmeanspp(data, k)
        for iter_num in range(max_iter):
            # Compute distances (n_samples x k)
            distances = np.linalg.norm(data[:, None] - centroids[None, :], axis=2)
            labels = np.argmin(distances, axis=1)

            new_centroids = np.array([
                data[labels == i].mean(axis=0) if np.any(labels == i) else centroids[i]
                for i in range(k)
            ])

            max_shift = np.linalg.norm(centroids - new_centroids, axis=1).max()
            print(f"  Iteration {iter_num + 1} - Max centroid shift: {max_shift:.6f}")

            if max_shift < tol:
                print("  Converged!")
                break
            centroids = new_centroids

        # Compute inertia
        inertia = np.sum((data - centroids[labels]) ** 2)
        print(f"Attempt {attempt + 1} inertia: {inertia:.2f}")

        if inertia < best_inertia:
            best_inertia = inertia
            best_centroids = centroids
            best_labels = labels

    # Group points by cluster
    clusters = [data[best_labels == i] for i in range(k)]

    return best_centroids, clusters
