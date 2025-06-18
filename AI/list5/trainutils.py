import numpy as np

def get_accuracy(y_true, y_pred):
    pred = np.zeros_like(y_pred)
    pred[y_pred.argmax(axis=0),np.arange(y_pred.shape[1])] = 1
    acc = np.sum(np.all(y_true==pred,axis=0))/y_pred.shape[1]
    return acc

def matrix_encode(y):
    y_encoded = np.zeros((y.size,int(y.max())+1), dtype=int)
    y_encoded[np.arange(y.size),y.astype(int)] = 1
    return y_encoded.T

# IN: [0, 2, 1, 0]
# OUT: [[1, 0, 0], 
#       [0, 0, 1], 
#       [0, 1, 0], 
#       [1, 0, 0]]


def train_test_split(X, y, train_size=0.8, random_state=42):
    np.random.seed(random_state)

    indices = np.arange(X.shape[1])
    np.random.shuffle(indices)
    X = X[:,indices]
    if y.ndim == 1:
        y = y.reshape(1, -1)
    y = y[:,indices]

    train_size = int(X.shape[1] * train_size)

    X_train, X_test = X[:,:train_size], X[:,train_size:]
    y_train, y_test = y[:,:train_size], y[:,train_size:]

    return X_train, X_test, y_train, y_test