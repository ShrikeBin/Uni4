import typing
import numpy as np
import math

class MyKMeans:
    def __init__(self, n_clusters: int, n_init: int=10, tolerance: float=1e-4, random_state: int=42, max_iterations: int=300, verbose=0):
        self.n_clusters = n_clusters
        self.num_iterations = n_init
        self.centroids = None
        self.labels = []
        self.inertia = None
        self.tolerance = tolerance
        self.random_state = random_state
        self.max_iterations = max_iterations
        self.verbose = verbose

    def initialize_centroids(self, X: np.ndarray):
        '''
        :param X: np array of shape (n_samples, n_features)
        :return: void
        '''
        if self.verbose > 0:
            print('Initializing centroids...')
        n_samples, n_features = X.shape
        centroids = np.zeros((self.n_clusters, n_features))
        rng = np.random.RandomState(self.random_state)

        first_idx = rng.randint(n_samples)
        centroids[0] = X[first_idx]
        dist_sq = np.full(n_samples, np.inf)
        for c in range(1, self.n_clusters):
            if self.verbose > 0:
                print(f'initializing centroid: {c}...')
            dist_to_new = np.sum((X - centroids[c-1])**2, axis=1)
            dist_sq = np.minimum(dist_sq, dist_to_new)
            probabilities = dist_sq / dist_sq.sum()
            next_idx = rng.choice(n_samples, p=probabilities)
            centroids[c] = X[next_idx]

        return centroids

    def caluculate_inertia(self, X: np.ndarray, labels: np.ndarray, centroids: np.array):
        '''
        :param X: np array of shape (n_samples, n_features)
        :param labels: np array of shape (n_samples)
        :return: inertia: int
        '''
        min_distances_from_centroids = np.sum((X - centroids[labels])**2, axis=1)
        return np.sum(min_distances_from_centroids)

    def fit(self, X: np.ndarray):
        '''
        :param X: np array of shape (n_samples, n_features)
        :return: self
        '''
        if self.verbose > 0:
            print('fitting...')

        best_centroids = None
        best_inertia = math.inf
        best_labels = None
        best_iter = 0

        rng = np.random.RandomState(self.random_state)
        X_sq = np.sum(X * X, axis=1).reshape(-1,1)


        for init_no in range(self.num_iterations):
            if self.verbose > 0:
                print(f'Iteration number: {init_no}/{self.num_iterations} of different centroids')
            centroids = self.initialize_centroids(X)

            for i in range(self.max_iterations):
                if self.verbose > 1:
                    print(f'iteration number: {i}/{self.max_iterations} of the same centroid')
                # calculate distances from centers, distances is a tensor shaped: (n_samples, n_clusters) distance of each sample to each cluster
                # distances = np.linalg.norm(X[:, np.newaxis, :] - centroids, axis=2)

                cent_sq = np.sum(centroids*centroids, axis=1).reshape(1,-1)
                cross = X.dot(centroids.T)
                distances = X_sq + cent_sq - 2*cross

                # determine labels of each sample: closest cluster
                labels = np.argmin(distances, axis=1)

                # calculate new centroids, shaped: (n_clusters, n_features)
                # new_clusters = np.array([np.mean(X[labels==j], axis=0) if np.any(labels == j) else centroids[j] for j in range(self.n_clusters)])

                sums = np.vstack([
                    np.bincount(labels, weights=X[:, f], minlength=self.n_clusters)
                    for f in range(X.shape[1])
                ]).T

                counts = np.bincount(labels, minlength=self.n_clusters)
                new_clusters = sums / counts[:, None]
                empties = counts == 0
                new_clusters[empties] = centroids[empties]


                # Check if convergence (shift of centroids less than tolerance)

                shift = np.linalg.norm(new_clusters - centroids, axis=1)
                if np.all(shift < self.tolerance):
                    centroids = new_clusters
                    break

                centroids = new_clusters

            # for each randomized centroids check inertia after convergence
            inertia = self.caluculate_inertia(X, labels, centroids)
            if inertia < best_inertia:
                best_inertia = inertia
                best_labels = labels.copy()
                best_iter = init_no + 1
                best_centroids = centroids.copy()

            self.centroids = best_centroids
            self.labels = best_labels
            self.inertia = best_inertia

        return self

    def predict(self, X: np.ndarray):
        '''
        :param X: np array of shape (n_samples, n_features)
        :return: np array of shape (n_samples, 1)
        '''

        distances = np.linalg.norm(X[:, np.newaxis] - self.centroids, axis=2)
        labels = np.argmin(distances, axis=1)
        return labels
