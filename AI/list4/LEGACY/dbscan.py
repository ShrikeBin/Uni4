import numpy as np

# ----------- Optional PCA dimensionality reduction -----------
def pca(X, n_components=50):
    Xc = X - X.mean(axis=0, keepdims=True)
    cov = np.cov(Xc, rowvar=False, dtype=np.float32)
    eigvals, eigvecs = np.linalg.eigh(cov)
    top = eigvecs[:, eigvals.argsort()[::-1][:n_components]]
    return (Xc @ top).astype(np.float32)

# ----------- Core DBSCAN logic -----------
def region_query(X, idx, eps):
    diff = X - X[idx]
    dist2 = np.einsum('ij,ij->i', diff, diff)
    return np.flatnonzero(dist2 <= eps * eps)

def dbscan(X, eps=3.0, min_pts=5):
    n = X.shape[0]
    labels = np.zeros(n, dtype=np.int32)
    visited = np.zeros(n, dtype=bool)
    cluster = 0

    for p in range(n):
        if visited[p]:
            continue
        visited[p] = True
        nbrs = region_query(X, p, eps)

        if nbrs.size < min_pts:
            labels[p] = -1
            continue

        cluster += 1
        labels[p] = cluster
        queue = list(nbrs)
        while queue:
            q = queue.pop(0)
            if not visited[q]:
                visited[q] = True
                q_nbrs = region_query(X, q, eps)
                if q_nbrs.size >= min_pts:
                    queue.extend(q_nbrs.tolist())
            if labels[q] == 0:
                labels[q] = cluster
    return labels
