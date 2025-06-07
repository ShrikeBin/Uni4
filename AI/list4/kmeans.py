import random
import math

def euclidean(p1, p2):
    return math.sqrt(sum((a - b) ** 2 for a, b in zip(p1, p2)))

def initialize_centroids_kmeanspp(data, k):
    centroids = [random.choice(data)]
    for _ in range(k - 1):
        distances = [min(euclidean(x, c) for c in centroids) for x in data]
        total = sum(distances)
        probs = [d / total for d in distances]
        r = random.random()
        s = 0
        for i, p in enumerate(probs):
            s += p
            if r <= s:
                centroids.append(data[i])
                break
    return centroids

def kmeans(data, k, max_iter=50, tol=1e-4):
    best_inertia = float('inf')
    best_result = None

    for attempt in range(5):
        print(f"Attempt {attempt + 1}")
        centroids = initialize_centroids_kmeanspp(data, k)
        for iter_num in range(max_iter):
            print(f"  Iteration {iter_num + 1}")
            clusters = [[] for _ in range(k)]
            for x in data:
                idx = min(range(k), key=lambda i: euclidean(x, centroids[i]))
                clusters[idx].append(x)
            new_centroids = []
            for i in range(k):
                if clusters[i]:
                    new_c = [sum(pix) / len(clusters[i]) for pix in zip(*clusters[i])]
                else:
                    new_c = centroids[i]
                new_centroids.append(new_c)
            # Check for convergence
            diffs = [euclidean(c1, c2) for c1, c2 in zip(centroids, new_centroids)]
            print(f"    Max centroid shift: {max(diffs):.6f}")
            if max(diffs) < tol:
                print("    Converged!")
                break
            centroids = new_centroids

        inertia = sum(euclidean(x, centroids[min(range(k), key=lambda i: euclidean(x, centroids[i]))])**2 for x in data)
        print(f"  Inertia: {inertia:.2f}")
        if inertia < best_inertia:
            best_inertia = inertia
            best_result = (centroids, clusters)

    return best_result