# coding=utf-8
class Edges(object):

	origen=""
	objetivo=""
	peso=0

	def __init__(self,origen,objetivo, peso):
		self.origen=origen
		self.objetivo=objetivo
		self.peso=peso

	def svertex(self):
		return self.origen

	def tvertex(self):
		return self.objetivo

	def weight(self):
		return self.peso