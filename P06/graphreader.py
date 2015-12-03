# coding=utf-8
import json 
from Vertex import Vertex
from Edges import Edges
from Graph import Graph 


class GraphReader:

		with open("ejemplos/graph.json") as json_file:
			json_data = json.load(json_file)

			grafica=Graph(json_data["direct"],json_data["vertices"],json_data["edges"])
		

		print("graph.json")
		print(grafica.vertices())
		print(grafica.edges())
		print(grafica.has_Cycles())