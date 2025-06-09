import numpy as np
from collections import Counter
import matplotlib.pyplot as plt
from sklearn.metrics import confusion_matrix, ConfusionMatrixDisplay
from dbscan_optimized import DBSCAN
import os

def get_labels(cluster_labels, true_labels):
    '''
    Create a mapping between cluster index and labels which those clusters correspond to by taking a majority vote
    :param cluster_labels: array like with int values corresponding to cluster index
    :param true_labels: array like with true labels of the samples
    :return: dictionary with cluster indicies as keys and true labels as values
    '''

    cluster_labels = np.asarray(cluster_labels)
    true_labels = np.asarray(true_labels)

    mapping = {}
    for c in np.unique(cluster_labels):
        idx = np.where(cluster_labels == c)[0]
        if len(idx) == 0:
            mapping[c] = -1
        else:
            mapping[c] = Counter(true_labels[idx]).most_common(1)[0][0]

    return mapping

def make_confusion_matrix(y_true, y_pred, labels=range(10), title='Confusion matrix', cmap=plt.cm.Blues):
    '''
    :param y_true: array like with true labels
    :param y_pred: array like with predicted labels
    :param labels: labels to be displayed on the confusion matrix
    :param title: title
    :param cmap: plt.cm object
    :return: fig, ax matplotlib objects
    '''
    cm = confusion_matrix(y_true, y_pred, labels=range(10))
    disp = ConfusionMatrixDisplay(confusion_matrix=cm, display_labels=range(10))
    fig, ax = plt.subplots(figsize=(10, 10))
    disp.plot(ax=ax, cmap="Blues")
    return fig, ax


def batch_simulation_dbscan(data, labels, radius_list=(2,3,4,5), neighbors_list=(2,3,4,5), folder_path=".", appendix="", verbose=0):
    '''
    :param data: array like with samples shaped (n_samples, n_features)
    :param labels: true labels of the data
    :param radius_list: list of possible radius values to check in dbscan
    :param neighbors_list: list of possible neighbor values to check in dbscan
    :param folder_path: path to the folder to save the confusion matricies
    :param appendix: extra string to append to the end of the file name
    :param verbose: verbosity level
    :return: dictionary with (radius, neighbor_num) as keys and numeber of clusers as values
    '''

    data = np.asarray(data)
    labels = np.asarray(labels)

    lengths = {}
    for radius in radius_list:
        for neighbors in neighbors_list:
            if verbose:
                print(f'simulation for radius: {radius}, neighbors: {neighbors}')
            filename = f'R={radius}_MiN={neighbors}' + appendix + ".png"
            my_dbscan = DBSCAN(radius=radius, verbose=verbose, num_close=neighbors).fit(data)
            clusters_labels = my_dbscan.labels_

            mapping = get_labels(clusters_labels, labels)

            lengths[(radius, neighbors)] = len(mapping)
            pred = np.array([mapping[c] if c in mapping else -1 for c in clusters_labels])
            fig, ax = make_confusion_matrix(labels, pred)
            fig.savefig(os.path.join(folder_path, filename))
