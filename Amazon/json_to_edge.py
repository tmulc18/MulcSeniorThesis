import json
import gzip
import csv

def parse(path):
	g = gzip.open(path,'r')
	for l in g:
		yield eval(l)

#convert data to edgelist in csv format
def create_edge_list(zip_name,csv_name):
	out = csv.writer(open(csv_name,"w"), delimiter=',',quoting=csv.QUOTE_ALL)
	data = parse(zip_name) #82912 items that have a related items field of which 44338 have an also bought field
	for l in data:
		current_item = l['asin']
		if 'related' in l.keys():
			if 'also_bought' in l['related'].keys():
				for related_item in l['related']['also_bought']:
					out.writerow([current_item,related_item])


zip_name = 'meta_Baby.json.gz'
csv_name = "babyEdges.csv"
create_edge_list(zip_name,csv_name)