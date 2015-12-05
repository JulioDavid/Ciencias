# coding=utf-8
class Edges(object):

	origen=""
	objetivo=""
	peso=0

	def __init__(self,origen,objetivo, peso):
		self.origen=origen
		self.objetivo=objetivo
		self.peso=peso

	def __str__(self):
		return "Origen %s , Objetivo %s, Peso %s" % (self.origen, self.objetivo, self.peso)

	def getElemento(self):
		return self.origen, self.objetivo

	#regresa el vértice origen de la arista.
	def svertex(self):
		return self.origen

	#regresa el vértice destino de la arista.
	def tvertex(self):
		return self.objetivo

	#regresa el peso de la arista.
	def weight(self):
		return self.peso

	
	