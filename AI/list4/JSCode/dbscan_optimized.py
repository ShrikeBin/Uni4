import numpy as np
from collections import deque
from sklearn.neighbors import NearestNeighbors


class DBSCAN:
    def __init__(self, radius=0.5, num_close=4, random_state=None, verbose=0):
        self.radius = radius
        self.num_close = num_close
        self.random_state = random_state
        self.verbose = verbose
        self.core_points = None

    def fit_optimized(self, X: np.ndarray):

        if self.verbose >= 1:
            print('Fitting optimized DBSCAN...')

        n_samples = X.shape[0]

        # Use ball_tree for efficient radius queries
        nbrs = NearestNeighbors(
            radius=self.radius,
            algorithm="ball_tree",
            n_jobs=-1
        ).fit(X)

        # Get all neighbors at once (more efficient than individual queries)
        neighbors_list = nbrs.radius_neighbors(X, return_distance=False)

        # Vectorized core point detection
        core_mask = np.array([len(neighbors) >= self.num_close for neighbors in neighbors_list])

        # Initialize labels and visited arrays
        labels = np.full(n_samples, -1, dtype=int)
        visited = np.zeros(n_samples, dtype=bool)
        cluster_id = 0

        # Process only core points as potential cluster seeds
        core_indices = np.where(core_mask)[0]

        for i in core_indices:
            if visited[i]:
                continue

            if self.verbose >= 2 and cluster_id % 100 == 0:
                print(f'Processing cluster {cluster_id}')

            # Start new cluster
            visited[i] = True
            labels[i] = cluster_id

            # Use set for O(1) membership testing
            queue = deque(neighbors_list[i])
            seen = set(neighbors_list[i])
            seen.add(i)  # Include seed point

            while queue:
                j = queue.popleft()

                if not visited[j]:
                    visited[j] = True
                    labels[j] = cluster_id

                    # Only expand from core points
                    if core_mask[j]:
                        for neighbor in neighbors_list[j]:
                            if neighbor not in seen:
                                seen.add(neighbor)
                                queue.append(neighbor)

            cluster_id += 1

        self.labels_ = labels
        self.n_clusters_ = cluster_id
        return self

    def fit_memory_efficient(self, X: np.ndarray, batch_size=1000):

        if self.verbose >= 1:
            print('Fitting memory-efficient DBSCAN...')

        n_samples = X.shape[0]

        # Use kd_tree for better memory efficiency with high-dimensional data
        nbrs = NearestNeighbors(
            radius=self.radius,
            algorithm="auto",  # Let sklearn choose the best algorithm lmao
            n_jobs=-1
        ).fit(X)

        # Process in batches to reduce memory usage
        core_mask = np.zeros(n_samples, dtype=bool)
        neighbors_cache = {}

        counter = 0
        for start_idx in range(0, n_samples, batch_size):
            counter += 1
            if self.verbose >= 2 and counter % 10 == 0:
                print(f'initializing neighbors for samples {start_idx} out of {n_samples}')

            end_idx = min(start_idx + batch_size, n_samples)
            batch_indices = np.arange(start_idx, end_idx)

            # Get neighbors for current batch
            batch_neighbors = nbrs.radius_neighbors(
                X[batch_indices], return_distance=False
            )

            # Update core mask and cache neighbors
            for i, neighbors in enumerate(batch_neighbors):
                actual_idx = start_idx + i
                core_mask[actual_idx] = len(neighbors) >= self.num_close
                neighbors_cache[actual_idx] = neighbors

        # Clustering phase (same as optimized version)
        labels = np.full(n_samples, -1, dtype=int)
        visited = np.zeros(n_samples, dtype=bool)
        cluster_id = 0

        core_indices = np.where(core_mask)[0]

        counter = 0
        for i in core_indices:
            counter += 1
            if visited[i]:
                continue

            if self.verbose >= 2 and counter % 100 == 0:
                print(f'Processing cluster {cluster_id}, counter:{counter}')
            visited[i] = True
            labels[i] = cluster_id

            queue = deque(neighbors_cache[i])
            seen = set(neighbors_cache[i])
            seen.add(i)

            while queue:
                j = queue.popleft()

                if not visited[j]:
                    visited[j] = True
                    labels[j] = cluster_id

                    if core_mask[j]:
                        # Get neighbors on demand if not cached
                        if j not in neighbors_cache:
                            neighbors_cache[j] = nbrs.radius_neighbors(
                                X[j:j + 1], return_distance=False
                            )[0]

                        for neighbor in neighbors_cache[j]:
                            if neighbor not in seen:
                                seen.add(neighbor)
                                queue.append(neighbor)

            cluster_id += 1

        self.labels_ = labels
        self.n_clusters_ = cluster_id
        return self

    # Keep your original methods for compatibility
    def pairwise_distances(self, X, squared=False):
        """Original pairwise distance method (for small datasets only)."""
        norms_sq = np.sum(X * X, axis=1).reshape(-1, 1)
        G = X.dot(X.T)
        distances = norms_sq + norms_sq.T - 2 * G
        np.maximum(distances, 0, out=distances)

        if squared:
            return distances
        else:
            return np.sqrt(distances)

    def initialize_core_points(self, distances: np.ndarray):
        """Original core point initialization (for small datasets only)."""
        if self.verbose >= 1:
            print('Initializing core points...')
        numbers_of_neighbors = np.sum(distances <= self.radius ** 2, axis=1)
        core_points = numbers_of_neighbors >= self.num_close
        return core_points

    def fit(self, X: np.ndarray):
        n_samples = X.shape[0]

        if n_samples > 10000:
            if self.verbose >= 1:
                print(f'Large dataset ({n_samples} samples), using memory-efficient approach')
            return self.fit_memory_efficient(X)
        
        elif n_samples > 2000:
            if self.verbose >= 1:
                print(f'Medium dataset ({n_samples} samples), using optimized approach')
            return self.fit_optimized(X)
        
        else:
            if self.verbose >= 1:
                print(f'Small dataset ({n_samples} samples), using original approach')

            # For small datasets, use my implementation
            distances = self.pairwise_distances(X, True)
            core_points_mask = self.initialize_core_points(distances)

            labels = np.full(n_samples, -1, dtype=int)
            visited = np.zeros(n_samples, dtype=bool)
            cluster_id = 0

            for i in range(n_samples):
                if visited[i] or not core_points_mask[i]:
                    continue

                visited[i] = True
                labels[i] = cluster_id

                neighbors = np.where(distances[i] <= self.radius ** 2)[0]
                queue = deque(neighbors.tolist())
                seen = set(neighbors)

                while queue:
                    current = queue.popleft()
                    if not visited[current]:
                        visited[current] = True
                        labels[current] = cluster_id

                        if core_points_mask[current]:
                            new_neighbors = np.where(distances[current] <= self.radius ** 2)[0]
                            for neighbor in new_neighbors:
                                if neighbor not in seen:
                                    seen.add(neighbor)
                                    queue.append(neighbor)
                    elif labels[current] == -1:
                        labels[current] = cluster_id

                cluster_id += 1

            self.labels_ = labels
            self.n_clusters_ = cluster_id
            return self