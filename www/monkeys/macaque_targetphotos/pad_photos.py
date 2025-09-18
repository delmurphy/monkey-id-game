#script to pad the macaque photos with black space to the size needed for the game

import cv2 as cv
import os
import numpy as np

# First, get the list of images in the folder
list_of_original_images = [f for f in os.listdir() if f.endswith(".jpg") or f.endswith(".JPG")]

# Create a new directory to store the padded images
os.makedirs("Padded_Images", exist_ok=True)

# Iterate through the image list
for image_path in list_of_original_images:
    image_array = cv.imread(image_path)  # Here we load image using OpenCV
    height, width, _ = image_array.shape  # Here we get the height, width, and color channels

    # Calculate the aspect ratio of the original image
    aspect_ratio = width / height

    # New image dimensions should be 3:2 width:height
    if aspect_ratio < 3/2:
        new_image_height = height
        new_image_width = int(3/2 * height)
    elif aspect_ratio >3/2:
        new_image_width = width
        new_image_height = int(2/3 * width)
    else:
        new_image_width = width + int(1/50 * width)
        new_image_height = height + int(1/50 * height)

    # Compute padding dimensions
    x_pad = (new_image_width - width) // 2
    y_pad = (new_image_height - height) // 2

    # Create a white canvas of the desired size
    padded_image = np.ones((new_image_height, new_image_width, 3), dtype=np.uint8) * 255

    # Paste the original image onto the canvas with padding
    padded_image[y_pad:y_pad + height, x_pad:x_pad + width] = image_array

    # Write padded image to Padded_Images folder
    cv.imwrite(f"Padded_Images/{image_path}", padded_image)