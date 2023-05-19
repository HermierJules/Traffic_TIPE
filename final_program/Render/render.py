import networkx as nx
import matplotlib.pyplot as plt

# Read the graph from a file
def read_graph_from_file(file_path):
    graph = nx.Graph()
    with open(file_path, 'r') as file:
        for line in file:
            source, target, thickness = line.strip().split(',')
            thickness = float(thickness)
            graph.add_edge(source, target, thickness=thickness)
    return graph

# Apply force-directed algorithm to position the nodes
def apply_force_directed_layout(graph):
    pos = nx.spring_layout(graph, seed=42)
    return pos

# Visualize the graph
def visualize_graph(graph):
    pos = apply_force_directed_layout(graph)

    plt.figure(figsize=(8, 8))
    for (source, target, data) in graph.edges(data=True):
        thickness = data['thickness']
        color = (1 - thickness, 0, 0)  # Redder for values closer to 1, blacker for values closer to 0
        nx.draw_networkx_edges(graph, pos, edgelist=[(source, target)], width=5 * thickness, edge_color=color)

    nx.draw_networkx_nodes(graph, pos, node_size=300, node_color='steelblue')
    nx.draw_networkx_labels(graph, pos, font_size=12, font_color='white')
    plt.axis('off')
    plt.show()

# Main execution
if __name__ == '__main__':
    file_path = 'graph_data.txt'  # Replace with your file path
    graph = read_graph_from_file(file_path)
    visualize_graph(graph)

