import numpy as np

class Perceptron:
    """ A basic Perceptron"""

    def __init__(self, inputs, targets):
        """ Constructor """
        # Set up network size
        if np.ndim(inputs) > 1:
            self.nIn = np.shape(inputs)[1]
        else: 
            self.nIn = 1

        if np.ndim(targets) > 1:
            self.nOut = np.shape(targets)[1]
        else:
            self.nOut = 1

        self.nData = np.shape(inputs)[0]
        
        # Initialize weights
        self.weights = np.random.rand(self.nIn + 1, self.nOut) * 0.1 - 0.05

    def pcntrain(self, inputs, targets, eta=0.25, nIterations=10):
        """ Train the perceptron """
        # Add the bias term to inputs
        inputs = np.concatenate((inputs, -np.ones((self.nData, 1))), axis=1)

        for n in range(nIterations):
            activations = self.pcnfwd(inputs)
            self.weights -= eta * np.dot(inputs.T, activations - targets)

            # Optional: Shuffle the data to improve training
            indices = np.arange(self.nData)
            np.random.shuffle(indices)
            inputs, targets = inputs[indices], targets[indices]

            # Optional: Monitor error (e.g., mean squared error)
            error = np.mean((activations - targets) ** 2)
            print(f"Iteration {n+1}, Error: {error}")

    def pcnfwd(self, inputs):
        """ Run the network forward """
        # Compute activations
        activations = np.dot(inputs, self.weights)

        # Threshold the activations
        return np.where(activations > 0, 1, 0)

    def confmat(self, inputs, targets):
        """Confusion matrix"""
        # Actualizar el tamaño del conjunto # CAMBIO 1
        current_nData = inputs.shape[0]
        
        # Agregar el término de bias con la dimensión correcta
        
        inputs = np.concatenate((inputs, -np.ones((current_nData, 1))), axis=1)
        outputs = self.pcnfwd(inputs)
        nClasses = 2 if targets.shape[1] == 1 else np.shape(targets)[1]

        if nClasses == 2:
            outputs = np.where(outputs > 0, 1, 0)
        else:
            outputs = np.argmax(outputs, axis=1)
            targets = np.argmax(targets, axis=1)

        cm = np.zeros((nClasses, nClasses), dtype=int)
        for i in range(nClasses):
            for j in range(nClasses):
                cm[i, j] = np.sum((outputs == i) & (targets == j))

        print("Confusion Matrix:")
        print(cm)
        accuracy = np.trace(cm) / np.sum(cm)
        print(f"Accuracy: {accuracy:.2f}")

# Ejemplo de uso para problemas de lógica
def logic():
    """ Run AND and XOR logic functions"""
    a = np.array([[0, 0, 0], [0, 1, 0], [1, 0, 0], [1, 1, 1]])
    b = np.array([[0, 0, 0], [0, 1, 1], [1, 0, 1], [1, 1, 0]])

    p = Perceptron(a[:, :2], a[:, 2:])
    p.pcntrain(a[:, :2], a[:, 2:], eta=0.25, nIterations=10)
    p.confmat(a[:, :2], a[:, 2:])

    q = Perceptron(b[:, :2], b[:, 2:])
    q.pcntrain(b[:, :2], b[:, 2:], eta=0.25, nIterations=10)
    q.confmat(b[:, :2], b[:, 2:])
