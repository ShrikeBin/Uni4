import os
import numpy as np
from PIL import Image
from tensorflow import keras

# Load model from task1
model = keras.models.load_model("../task1/mnist_model.keras")

image_folder = "./myMNIST"

y_true = []
y_pred = []

# Open file to save results
with open("metrics_handrawn.txt", "w") as file:
    file.write("\nPrediction details:\n")
    
    # Loop through images in folder
    for filename in os.listdir(image_folder):
        if filename.endswith(".png"):
            # filename = g5-123.png -> label = 5
            label = int(filename.split('.')[0][1:])
            
            img_path = os.path.join(image_folder, filename)
            img = Image.open(img_path).convert("L") # Open as grayscale
            img = img.resize((28, 28))
            img_array = np.array(img) / 255.0  # Normalize
            
            # Add a batch dimension since the model expects it (1, 28, 28)
            img_array = np.expand_dims(img_array, axis=-1)
            img_array = np.expand_dims(img_array, axis=0)
            
            prediction = model.predict(img_array)
            predicted_class = np.argmax(prediction, axis=1)[0]
            
            y_true.append(label)
            y_pred.append(predicted_class)

            is_correct = "TRUE" if label == predicted_class else "FALSE"
            file.write(f"{filename}: True Label = {label}, Predicted = {predicted_class} = {is_correct}\n")
    
    y_true = np.array(y_true)
    y_pred = np.array(y_pred)

    accuracy = np.sum(y_true == y_pred) / len(y_true)

    file.write(f"\nAccuracy: {accuracy * 100:.2f}%\n")
