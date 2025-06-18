import numpy as np


class Layer:
    def __init__(self):
        self.input = None
        self.output = None

    def forward(self, inputs):
        pass

    def backward(self, gradient, hparams):
        pass


class DenseLayer(Layer):
    def __init__(self, input_size, output_size):
        super().__init__()
        self.weights = np.random.randn(*(output_size, input_size)) * np.sqrt(2 / input_size)
        self.bias = np.zeros((output_size, 1))
        self.VdW = 0
        self.Vdb = 0

    def forward(self, inputs):
        self.input = inputs
        self.output = np.dot(self.weights, inputs) + self.bias
        return self.output

    def backward(self, gradient, hparams):
        dW = np.dot(gradient, self.input.T) / self.input.shape[1]
        # gradient = dLoss/dOutput (shape: output_size x batch_size)
        # self.input.T = input transposed (shape: batch_size x input_size)
        # dW = dLoss/dWeights (shape: output_size x input_size)
        # Calculates how loss changes with weights by combining gradient and input, averaged over batch

        db = (np.sum(gradient, axis=1) / self.input.shape[1]).reshape(-1, 1)
        # Sum gradient over batch axis (axis=1) to get total gradient for each output neuron bias
        # Divide by batch size to average
        # Reshape to column vector (output_size x 1)
        # db = dLoss/dBias


        # support for momentum
        if 'momentum' in hparams:
            momentum = hparams['momentum']
            self.VdW = momentum * self.VdW + (1-momentum)*dW
            self.Vdb = momentum * self.Vdb + (1-momentum)*db

            self.weights -= hparams['lr'] * self.VdW
            self.bias -= hparams['lr'] * self.Vdb
        # plain gradient descent
        else:
            self.weights -= hparams['lr'] * dW
            self.bias -= hparams['lr'] * db

        return np.dot(self.weights.T, gradient)


# Normlization layers
class L1(Layer):
    def __init__(self):
        super().__init__()

    def forward(self, inputs):
        self.input = inputs
        l1_norm = np.sum(np.abs(inputs), axis=0, keepdims=True)
        l1_norm = np.where(l1_norm == 0, 1e-10, l1_norm)
        self.output = inputs / l1_norm
        return self.output
    
    def backward(self, gradient, hparams):
        return gradient
    
class L2(Layer):
    def __init__(self):
        super().__init__()
    
    def forward(self, inputs):
        self.input = inputs
        l2_norm = np.sqrt(np.sum(inputs ** 2, axis=0, keepdims=True))
        l2_norm = np.where(l2_norm == 0, 1e-10, l2_norm)
        self.output = inputs / l2_norm
        return self.output
    
    def backward(self, gradient, hparams):
        return gradient