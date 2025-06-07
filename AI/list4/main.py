import numpy as np
from sklearn.metrics import confusion_matrix
import matplotlib.pyplot as plt
import seaborn as sns
from save_utils import save_pgm  # your save function
from kmeans import kmeans
from loader import load_images_uncompressed, load_labels_uncompressed

images = load_images_uncompressed('./data/images.idx3-ubyte')
labels = load_labels_uncompressed('./data/labels.idx1-ubyte')

images = np.array(images)
labels = np.array(labels)

print(f'Loaded {len(images)} images and {len(labels)} labels.')

print('Running K-means...')
k = 10
centroids, clusters = kmeans(images, k)
print(f'K-means completed with {k} clusters.')

print('Saving centroid images...')
for i, c in enumerate(centroids):
    save_pgm(f'centroid_{i}.pgm', c)

print('Building confusion matrix...')

predicted_labels = np.empty(len(images), dtype=int)
for cluster_id, cluster_points in enumerate(clusters):
    for point in cluster_points:
        idx = np.where(np.all(images == point, axis=1))[0][0]
        predicted_labels[idx] = cluster_id

cluster_to_digit = {}
for cluster_id, cluster_points in enumerate(clusters):
    counts = np.zeros(10, dtype=int)
    for point in cluster_points:
        idx = np.where(np.all(images == point, axis=1))[0][0]
        counts[labels[idx]] += 1
    cluster_to_digit[cluster_id] = np.argmax(counts)

predicted_digits = np.array([cluster_to_digit[cl] for cl in predicted_labels])

cm = confusion_matrix(labels, predicted_digits)

# Save confusion matrix as PNG
plt.figure(figsize=(10, 8))
sns.heatmap(cm, annot=True, fmt='d', cmap='Blues',
            xticklabels=[str(i) for i in range(10)],
            yticklabels=[str(i) for i in range(10)])
plt.xlabel('Predicted Label')
plt.ylabel('True Label')
plt.title('Confusion Matrix')
plt.savefig('confusion_matrix.png', dpi=300)
plt.show()

