# coding=utf-8
class Vertex(object):

	elem=""
	adyacentes=[]

	def __init__(self,elem,adyacentes):
		self.elem=elem
		self.adyacentes=adyacentes

	#regresa los vertices adyacentes del vértice dado.
	def neighbours(self):
		return self.adyacentes
    
    #regresa el grado del vértice.
	def degree(self):
		return len(self.adyacentes)