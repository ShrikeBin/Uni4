import numpy as np
from tensorflow import keras
from sklearn.ensemble import RandomForestClassifier
from sklearn.metrics import classification_report, confusion_matrix

# Load MNIST with Keras
(X_train, y_train), (X_test, y_test) = keras.datasets.mnist.load_data()

# Normalize etc
X_train = X_train.reshape(-1, 28*28) / 255.0
X_test = X_test.reshape(-1, 28*28) / 255.0

# Train 
clf = RandomForestClassifier(n_estimators=100, random_state=42)
clf.fit(X_train, y_train)

# Make predictions
y_pred = clf.predict(X_test)

# Confusion matrix 
# How many specified as what -> in that case 10x10 matrix, 
# (0,0) guessed to be 0,
# (0,1) 0 guessed to be 1 
# (i,i) is True Positve for i
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

