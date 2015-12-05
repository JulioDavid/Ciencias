# coding=utf-8
import json 
import csv 
from xml.dom.minidom import parse, parseString	
from Vertex import Vertex
from Edges import Edges
from Graph import Graph 


class GraphReader:


	archivo=''


	def __init__(self, archivo):
		self.archivo=archivo;


	def ext(self):	

			if self.archivo== 'graph.json' or  self.archivo== 'petersen.json':
				return self.leeJSON()

			elif self.archivo=='graph.csv' or self.archivo=='petersen.csv':
				return self.leeCSV()
			else: 
				return self.leeXML()

	def leeJSON(self):
		gr=Graph(0,[],[])

		with open(self.archivo) as json_file:
				json_data = json.load(json_file)					

				dir1=json_data["direct"]
				vert1=json_data["vertices"]
				edge1=json_data["edges"]

				gr.addDirect(dir1)

				for v in vert1:
					gr.addVx(v)

				for e in edge1:
					gr.addEd(e[0],e[1],e[2])
		return gr
	
	def leeCSV(self):
			grafica = Graph(0,[],[])
			with open(self.archivo) as csv_file:
				csv_data=csv.reader(csv_file, delimiter=',')

				if( next(csv_data)[0]== 'direct=1'):
					grafica.addDirect(1)

				for l in csv_data:
					grafica.addVx(l[0])
					grafica.addEd(l[0],l[1],l[2])

			return grafica



	def leeXML (self):
		graficax=Graph(0,[],[])

		line = parse(self.archivo)
  		dirc = line.getElementsByTagName('graph')
  		
  		if dirc[0].attributes['direct'] == 1:
  			graficax.addDirect(1)

		vert = line.getElementsByTagName('vertex')

  		for v in vert:
  			graficax.addVx(v.attributes['label'].value)

		ari = line.getElementsByTagName('edge')
		for a in ari:
		 	ori = a.attributes['source'].value
		 	des	= a.attributes['target'].value
		 	peso = a.attributes['weight'].value
			graficax.addEd(ori,des,peso)
		
		return graficax




gj=GraphReader('graph.json')
print("graph.json")
print('Es dirigida: '+ gj.ext().directed() )
print('Vertices: ')
(gj.ext().vertices())
print('Aristas: ')
(gj.ext().edges())

gpj=GraphReader('petersen.json')
print("petersen.json")
print('Es dirigida: '+ gpj.ext().directed() )
print('Vertices: ')
(gpj.ext().vertices())
print('Aristas: ')
(gpj.ext().edges())

gc=GraphReader('graph.csv')
print("graph.csv")
print('Es dirigida: '+ gc.ext().directed() )
print('Vertices: ')
gc.ext().vertices()
print('Aristas: ')
gc.ext().edges()

gpc=GraphReader('petersen.csv')
print("petersen.csv")
print('Es dirigida: '+ gpc.ext().directed() )
print('Vertices: ')
gpc.ext().vertices()
print('Aristas: ')
gpc.ext().edges()

gx=GraphReader('graph.xml')
print("graph.xml")
print('Es dirigida: '+ gx.ext().directed() )
print('Vertices: ')
gx.ext().vertices()
print('Aristas: ')
gx.ext().edges()


ggx=GraphReader('grafica.xml')
print("grafica.xml")
print('Es dirigida: '+ ggx.ext().directed() )
print('Vertices: ')
ggx.ext().vertices()
print('Aristas: ')
ggx.ext().edges()


gpx=GraphReader('petersen.xml')
print("petersen.xml")
print('Es dirigida: '+ gpx.ext().directed() )
print('Vertices: ')
gpx.ext().vertices()
print('Aristas: ')
gpx.ext().edges()












		

	




