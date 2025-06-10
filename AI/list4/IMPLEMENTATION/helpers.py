import numpy as np
from collections import Counter
import matplotlib.pyplot as plt
import matplotlib.gridspec as gridspec
from mpl_toolkits.axes_grid1 import make_axes_locatable
from sklearn.metrics import confusion_matrix, ConfusionMatrixDisplay, accuracy_score
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

    cm = confusion_matrix(y_true, y_pred, labels=labels)
    acc = accuracy_score(y_true, y_pred)

    fig = plt.figure(figsize=(10, 12))
    gs = gridspec.GridSpec(2, 1, height_ratios=[1, 10])

    ax_text = fig.add_subplot(gs[0])
    ax_text.axis('off')
    ax_text.text(0.5, 0.5, f"OVERALL ACCURACY = {acc:.3%}", fontsize=25, ha='center', va='center')

    ax_matrix = fig.add_subplot(gs[1])
    disp = ConfusionMatrixDisplay(confusion_matrix=cm, display_labels=labels)
    disp.plot(ax=ax_matrix, cmap=cmap, colorbar=False)

    # Match colorbar to matrix height
    im = ax_matrix.images[-1]
    divider = make_axes_locatable(ax_matrix)
    cax = divider.append_axes("right", size="5%", pad=0.1)
    fig.colorbar(im, cax=cax)

    ax_matrix.set_title(title, fontsize=20, pad=10)
    fig.tight_layout()

    return fig, (ax_text, ax_matrix), acc


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

    best_acc = 0
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
            fig, _, acc = make_confusion_matrix(labels, pred, title=f"DBSCAN   Radius: {radius} | Min Neighbors: {neighbors}\n" +               
                                                                    f'Nr. clusters: [{my_dbscan.n_clusters_}] CLUSTERS\n' +
                                                                    f"Nr. noise points: [{np.sum(my_dbscan.labels_ == -1)}] NOISE\n")
            if acc >= best_acc:
                best_acc = acc
                best_fig = fig
            if verbose:
                print("-----------------------------------------------")
                print(f'Number of clusters: {my_dbscan.n_clusters_}')
                print(f"Number of noise points: {np.sum(my_dbscan.labels_ == -1)}")
                print(f'Radius: {radius}, Neighbors: {neighbors}')
                print(f"Accuracy: {acc:.2%} | Current best: {best_acc:.2%}")
                print("-----------------------------------------------")
            fig.savefig(os.path.join(folder_path, filename))

    best_fig.savefig(os.path.join(folder_path, "[BEST]" + filename))
            
