from loader import load_images, load_labels, load_images_uncompressed, load_labels_uncompressed
from kmeans import kmeans
from save_utils import save_pgm, build_confusion_matrix, print_confusion_matrix

# Load data
images = load_images_uncompressed('./data/images.idx3-ubyte')
labels = load_labels_uncompressed('./data/labels.idx1-ubyte')
print(f'Loaded {len(images)} images and {len(labels)} labels.')

# Run K-means
print('Running K-means...')
k = 10
centroids, clusters = kmeans(images, k)
print(f'K-means completed with {k} clusters.')

# Save centroid images
print('Saving centroid images...')
for i, c in enumerate(centroids):
    save_pgm(f'centroid_{i}.pgm', c)

# Build & print confusion matrix
print('Building confusion matrix...')
total_per_digit = [labels.count(d) for d in range(10)]
label_map = build_confusion_matrix(clusters, labels, images, k)
print_confusion_matrix(label_map, total_per_digit)
