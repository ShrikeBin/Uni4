def save_pgm(filename, data):
    with open(filename, 'wb') as f:
        f.write(f'P5\n28 28\n255\n'.encode())
        f.write(bytes(int(p) for p in data))

def build_confusion_matrix(clusters, labels, original_data, k):
    label_map = {}
    for i, cluster in enumerate(clusters):
        label_counts = [0] * 10
        for img in cluster:
            index = original_data.index(img)
            label_counts[labels[index]] += 1
        label_map[i] = label_counts
    return label_map

def print_confusion_matrix(label_map, total_per_digit):
    for digit in range(10):
        row = []
        for cluster_id in range(10):
            count = label_map[cluster_id][digit]
            percent = 100 * count / total_per_digit[digit] if total_per_digit[digit] else 0
            row.append(f"{percent:5.1f}%")
        print(" ".join(row))
