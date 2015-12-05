# coding=utf-8
class Vertex(object):

	elem=''
	adyacentes=[]

	#def __init__(self,elem,adyacentes):
	#	self.elem=elem
	#	self.adyacentes=adyacentes

	def __init__(self,elem):
		self.elem=elem

	def __str__(self):
		return "Vertice %s " % (self.elem)


	def getElemento(self):
		return self.elem

	#regresa los vertices adyacentes del vértice dado.
	def neighbours(self):
		for v in self.adyacentes:
			print v

    #regresa el grado del vértice.
	def degree(self):
		return len(self.adyacentes)

