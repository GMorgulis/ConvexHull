import numpy as np

def generate_circle_points(radius, num_points):
    angles = np.linspace(0, 2 * np.pi, num_points, endpoint=False)
    x_coords = radius * np.cos(angles)
    y_coords = radius * np.sin(angles)
    return x_coords, y_coords

def write_points_to_file(filename, x_coords, y_coords):
    with open(filename, 'w') as file:
        for x, y in zip(x_coords, y_coords):
            file.write(f"{x},{y}\n")

radius = 50000000
num_points = 10000000
x_coords, y_coords = generate_circle_points(radius, num_points)

# Write points to file
filename = "random_points2.txt"
write_points_to_file(filename, x_coords, y_coords)

print(f"Points have been written to {filename}")
