import math
import random

def fruchterman_reingold(nodes, edges, area, iterations):
    k = math.sqrt(area / len(nodes))

    positions = {node: (random.random(), random.random()) for node in nodes}

    for _ in range(iterations):
        displacements = {node: [0, 0] for node in nodes}

        for node in nodes:
            for other in nodes:
                if node != other:
                    x1, y1 = positions[node]
                    x2, y2 = positions[other]
                    dx = x2 - x1
                    dy = y2 - y1
                    distance = math.sqrt(dx*dx + dy*dy)

                    if distance > 0:
                        repulsive_force = (k * k) / distance
                        displacements[node][0] -= dx / distance * repulsive_force
                        displacements[node][1] -= dy / distance * repulsive_force

        for u, v in edges:
            x1, y1 = positions[u]
            x2, y2 = positions[v]
            dx = x2 - x1
            dy = y2 - y1
            distance = math.sqrt(dx*dx + dy*dy)

            if distance > 0:
                attractive_force = (distance * distance) / k
                displacements[u][0] += dx / distance * attractive_force
                displacements[u][1] += dy / distance * attractive_force
                displacements[v][0] -= dx / distance * attractive_force
                displacements[v][1] -= dy / distance * attractive_force

        for node in nodes:
            x, y = positions[node]
            displacement_x, displacement_y = displacements[node]
            displacement = math.sqrt(displacement_x*displacement_x + displacement_y*displacement_y)
            if displacement > 0:
                x += displacement_x / displacement * min(displacement, temperature)
                y += displacement_y / displacement * min(displacement, temperature)
                x = max(0, min(1, x))
                y = max(0, min(1, y))
                positions[node] = (x, y)

        temperature *= cooling_factor

    return positions


def read_graph(filename):
    with open(filename, 'r') as file:
        lines = file.readlines()

    n = int(lines[0].strip().split('=')[1].strip())
    nodes = list(range(n))
    edges = []

    for line in lines[1:]:
        if line.strip():
            u, v = map(int, line.strip().split(','))
            edges.append((u, v))

    return nodes, edges


def write_coordinates(filename, positions):
    with open(filename, 'w') as file:
        for node, (x, y) in positions.items():
            file.write(f"{node}: ({x}, {y})\n")

if __name__ == '__main__':
    graph_filename = 'graph.txt'
    output_filename = 'output.txt'
    area = 1.0
    iterations = 1000
    temperature = 1.0
    cooling_factor = 0.99

    nodes, edges = read_graph(graph_filename)
    positions = fruchterman_reingold(nodes, edges, area, iterations)
    write_coordinates(output_filename, positions)

