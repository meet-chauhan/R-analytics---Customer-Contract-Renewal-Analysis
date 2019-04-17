"""
import csv
from collections import OrderedDict

with open('project2.conversion.5.csv', 'rb') as f:
    r = csv.reader(f)
    dict2 = {row[0]: row[1:] for row in r}

with open('project2.incident.5.csv', 'rb') as f:
    r = csv.reader(f)
    dict1 = OrderedDict((row[0], row[1:]) for row in r)

result = OrderedDict()
for d in (dict1, dict2):
    for key, value in d.iteritems():
         result.setdefault(key, []).extend(value)

with open('ab_combined.csv', 'wb') as f:
    w = csv.writer(f)
    for key, value in result.iteritems():
        w.writerow([key] + value)
        
      """  
        
import pandas
vms = pandas.read_csv('project2.conversion.5.csv')
users = pandas.read_csv('project2.incident.5.csv')

output = pandas.merge(vms, users)
output.to_csv('output.tsv')