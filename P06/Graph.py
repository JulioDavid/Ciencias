# coding=utf-8
from Edges import Edges
from Vertex import Vertex 

class Graph(Edges,Vertex):

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
		for v in self.vert:
			print v

    #regresa todas las aristas de la gráfica.
	def edges(self):
		for e in self.aris:
			print e


	def addVx(self, elem):
		v=Vertex(elem)
		if(v not in self.vert):
			self.vert.append(v)
		

	def addEd(self, origen, destino, peso):
		ed=Edges(origen,destino,peso)
		if(ed not in self.aris):
			self.aris.append(ed)

	def addDirect(self,dir1):
		self.direct=dir1

