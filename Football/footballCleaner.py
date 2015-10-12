import csv
def nameParser(name):
	if(name.find('(') == 0):
		return name[name.find(')')+2:]
	else:
		return name
out = csv.writer(open("clean.csv","w"), delimiter=',',quoting=csv.QUOTE_ALL)
with open('2014football_dirty.csv','rb') as csvfile:
	reader = csv.reader(csvfile,delimiter=',', quotechar='|')
	for row in reader:
		print(nameParser(row[5]),nameParser(row[8]))
		out.writerow([nameParser(row[5]),nameParser(row[8])])

