class vertice(object):

	elem=""
	adyacentes=[]

	def __init__(self,elem,adyacentes):
		self.elem=elem
		self.adyacentes=adyacentes

	def neighbours(self):
		return self.adyacentes

	def degree(self):
		return self.len(adyacentes)