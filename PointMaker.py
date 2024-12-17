import random

def generate_random_points(num_points, x_range, y_range):
    points = []
    for _ in range(num_points):
        x = random.uniform(*x_range)
        y = random.uniform(*y_range)
        points.append((x, y))
    return points

def write_points_to_file(points, filename):
    with open(filename, 'w') as file:
        for point in points:
            file.write(f'{point[0]}, {point[1]}\n')

num_points = 4000000
x_range = (-50000000, 50000000)  
y_range = (-50000000, 50000000)  

random_points = generate_random_points(num_points, x_range, y_range)
write_points_to_file(random_points, 'r4m.txt')
