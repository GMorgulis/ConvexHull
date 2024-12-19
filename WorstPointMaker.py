import numpy as np
import random

# George Morgulis(gm3138)
# Henry Lin(hkl2127)
# Functions to create edge cases that look like donuts (use jitter=0 for perfect circle)
# Based on this stack overflow post:
# https://stackoverflow.com/questions/8487893/generate-all-the-points-on-the-circumference-of-a-circle
def generate_circle_points(radius, num_points, jitter):
    angles = np.linspace(0, 2 * np.pi, num_points, endpoint=False)
    x_rad = []
    y_rad = []
    for i in range(num_points):
        x_rad.append(radius + random.randrange(-jitter,jitter))
        y_rad.append(radius + random.randrange(-jitter, jitter))
    x_coords = np.multiply(np.array(x_rad), np.cos(angles))
    y_coords = np.multiply(np.array(y_rad) ,np.sin(angles))
    return x_coords, y_coords

def write_points_to_file(filename, x_coords, y_coords):
    with open(filename, 'w') as file:
        for x, y in zip(x_coords, y_coords):
            file.write(f"{x},{y}\n")

radius = 50000000
num_points = 4000000
jitter = 5
x_coords, y_coords = generate_circle_points(radius, num_points,jitter)

filename = "d4m5j.txt"
write_points_to_file(filename, x_coords, y_coords)

print(f"Points have been written to {filename}")
