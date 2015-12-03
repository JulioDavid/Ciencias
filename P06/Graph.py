# coding=utf-8
from Edges import Edges
from Vertex import Vertex 

class Graph(object):

	direct=0
	vert=[]
	aris=[]

	def __init__(self,direct,vertices,aristas):
		self.direct=direct
		self.vert=vertices
		self.aris=aristas
	#regresa un booleano, true si la gráfica es dirigida y false en caso contrario.
	def directed(self):
		if self.direct==0:
			return "false"
		elif self.direct==1:
			return "true"
		else:
			return "Error"

	#regresa un arreglo con todos los vértices de la gráfica.
	def vertices(self):
		return self.vert

    #regresa todas las aristas de la gráfica.
	def edges(self):
		return self.aris

    #regrese true si la gráfica tiene un ciclo o false en caso contrario.
	def has_Cycles(self):
		pass


