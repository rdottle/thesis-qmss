import osmnx as ox

G = ox.graph_from_place('Los Angeles, California, USA', network_type='drive')

# project the network to an appropriate UTM (automatically determined)
G_projected = ox.project_graph(G)

# you can also plot/save figures as SVGs to work with in Illustrator later
ox.save_graph_shapefile(G_projected, filename='network-losanegeles')
