# loader.py
# This module provides functions to load MNIST dataset images and labels from gzip files.
import gzip
import struct

def load_images(path):
    with gzip.open(path, 'rb') as f:
        _, num_images, rows, cols = struct.unpack('>IIII', f.read(16))
        return [list(f.read(rows * cols)) for _ in range(num_images)]

def load_labels(path):
    with gzip.open(path, 'rb') as f:
        _, num_labels = struct.unpack('>II', f.read(8))
        return list(f.read(num_labels))
    
def load_images_uncompressed(path):
    with open(path, 'rb') as f:
        magic, num_images, rows, cols = struct.unpack('>IIII', f.read(16))
        data = f.read(rows * cols * num_images)
        images = [list(data[i * rows * cols:(i + 1) * rows * cols]) for i in range(num_images)]
    return images

def load_labels_uncompressed(path):
    with open(path, 'rb') as f:
        magic, num_labels = struct.unpack('>II', f.read(8))
        labels = list(f.read(num_labels))
    return labels

