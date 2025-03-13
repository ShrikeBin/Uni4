import tensorflow as tf
from tensorflow import keras
import numpy as np
from sklearn.metrics import confusion_matrix
# Load Mnist
(X_train, y_train), (X_test, y_test) = keras.datasets.mnist.load_data()

# Normalize
X_train = X_train / 255.0
X_test = X_test / 255.0

# Model Creation
model = keras.Sequential([
    keras.layers.Flatten(input_shape=(28, 28)),
    keras.layers.Dense(128, activation="relu"),
    keras.layers.Dense(10, activation="softmax") # Softmax so it adds up to 100%
])

model.compile(optimizer="adam",
              loss="sparse_categorical_crossentropy",
              metrics=["accuracy"])

# TRAINING
model.fit(X_train, y_train, epochs=10, validation_data=(X_test, y_test))

# Make predictions
y_pred = model.predict(X_test)
y_pred_classes = np.argmax(y_pred, axis=1)

cm = confusion_matrix(y_test, y_pred)

accuracy = clf.score(X_test, y_test)
precision = cm.diagonal() / cm.sum(axis=0) 
sensitivity = cm.diagonal() / cm.sum(axis=1)

# FP FN TN TP
fp = cm.sum(axis=0) - cm.diagonal() 
fn = cm.sum(axis=1) - cm.diagonal()
tn = cm.sum() - (fp + fn + cm.diagonal()) 
tp = cm.diagonal()

total_samples = len(y_test)

fp_percentage = (fp / total_samples) * 100
fn_percentage = (fn / total_samples) * 100
tn_percentage = (tn / total_samples) * 100
tp_percentage = (tp / total_samples) * 100

precision_percentage = (precision * 100)
sensitivity_percentage = (sensitivity * 100)

# Save
with open("metrics.txt", "w") as file:
    file.write(f"Accuracy: {accuracy * 100:.2f}%\n\n")
    
    file.write("Precision for each class:\n")
    for i, p in enumerate(precision * 100):
        file.write(f"Class {i}: {p:.2f}%\n")
    
    file.write("\nSensitivity for each class:\n")
    for i, s in enumerate(sensitivity * 100):
        file.write(f"Class {i}: {s:.2f}%\n")
    
    file.write("\nFalse Positives (FP) percentage for each class:\n")
    for i, fp_val in enumerate(fp_percentage):
        file.write(f"Class {i}: {fp_val:.2f}%\n")
    
    file.write("\nFalse Negatives (FN) percentage for each class:\n")
    for i, fn_val in enumerate(fn_percentage):
        file.write(f"Class {i}: {fn_val:.2f}%\n")
    
    file.write("\nTrue Negatives (TN) percentage for each class:\n")
    for i, tn_val in enumerate(tn_percentage):
        file.write(f"Class {i}: {tn_val:.2f}%\n")
    
    file.write("\nTrue Positives (TP) percentage for each class:\n")
    for i, tp_val in enumerate(tp_percentage):
        file.write(f"Class {i}: {tp_val:.2f}%\n")
