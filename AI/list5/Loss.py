import numpy as np


def Crossentropy(y_true, y_pred):
    EPSILON = 1e-8
    y_pred_clipped = np.clip(y_pred, EPSILON, 1 - EPSILON)
    loss = (-np.sum(y_true * np.log(y_pred_clipped), axis=0)).reshape(-1, 1)
    cost = np.sum(loss)
    return cost


def CrossentropyPrime(y_true, y_pred):
    EPSILON = 1e-8
    return -(np.divide(y_true, y_pred + EPSILON)).reshape(-1, 1)