def save_pgm(filename, data):
    """Save a single 28x28 grayscale image (flat array) as PGM file."""
    with open(filename, 'wb') as f:
        f.write(b'P5\n28 28\n255\n')
        f.write(bytes(int(p) for p in data))
