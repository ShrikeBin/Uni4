import numpy as np

def generate_random_data(length, random_state=None):

    if random_state is not None:
        np.random.seed(random_state)

    samples = 2 * np.random.rand(2, length) - 1

    labels = (samples[0,:] * samples[1, :]) > 0
    labels = labels.astype(float).reshape(1,-1)

    return samples, labels