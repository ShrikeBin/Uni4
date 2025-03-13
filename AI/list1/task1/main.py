import tensorflow as tf
from tensorflow import keras
import numpy as np

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

# True Positive etc.
TP = FP = TN = FN = 0

for true_label, pred_label in zip(y_test, y_pred_classes):
    if true_label == pred_label:
        if true_label == 0:
            TN += 1
        else:
            TP += 1
    else:
        if true_label == 0:
            FN += 1
        else:
            FP += 1

total = len(y_test) 
TP_percentage = (TP / total) * 100
TN_percentage = (TN / total) * 100
FP_percentage = (FP / total) * 100
FN_percentage = (FN / total) * 100

accuracy = (TP + TN) / total
sensitivity = TP / (TP + FN)
precision = TP / (TP + FP)

# Save to file
with open("Data_Percentage.txt", "w") as f:
    f.write(f"(for '0') True Positive: {TP_percentage:.2f}%\n")
    f.write(f"(for '0') True Negative: {TN_percentage:.2f}%\n")
    f.write(f"(for '0') False Positive: {FP_percentage:.2f}%\n")
    f.write(f"(for '0') False Negative: {FN_percentage:.2f}%\n")
    f.write(f"Accuracy: {accuracy*100:.2f}%\n")
    f.write(f"Sensitivity: {sensitivity:.4f}\n")
    f.write(f"Precision: {precision:.4f}\n")
