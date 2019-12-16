#image classification, with simple provided measurements

from matplotlib import pyplot as plt
from sklearn.datasets import load_iris
import numpy as np

dat = load_iris()
data = dat.data
feature_names = dat.feature_names
target = dat.target

print(feature_names)
labels = dat.target_names[target]

fig,axes = plt.subplots(2,3)
pairs = [(0, 1), (0, 2), (0, 3), (1, 2), (1, 3), (2, 3)]
# triangles are iris setosa
color_markets = [
        ('r', '>'),
        ('g', 'o'),
        ('b', 'x'),
        ]
		
for i, (p0, p1) in enumerate(pairs):
	ax = axes.flat[i]
	for t in range(3):
		c, marker = color_markets[t]
		ax.scatter(data[target == t, p0], data[target == t, p1], marker=marker, c=c)
		ax.set_xlabel(feature_names[p0])
		ax.set_ylabel(feature_names[p1])
		ax.set_xticks([])
		ax.set_yticks([])

fig.tight_layout()
#plt.show()

#cutoff to sperate flowers(iris setosa), from the book
plength = data[:, 2]
is_setosa = (labels == 'setosa') #remember that this is an array of booleans!
print("type: %s" % type(is_setosa))
max_setosa = plength[is_setosa].max()
min_non_setosa = plength[~is_setosa].min()
print('max: %g' % max_setosa)
print('min non: %g' % min_non_setosa)

print("confirmed setosas:")
print(data[:,2] < 2)

# determine accurancy of setosas for the remaining data. Note the exclusion of setosas in the data
non_setosa_data = data[~is_setosa]
non_setosa_labels = labels[~is_setosa]
virginica = (labels == 'virginica')
print (data.shape)

best_acc = -1.0
for fi in range(data.shape[1]):
	thresh = data[:,fi].copy()
	thresh.sort()
	for t in thresh:
		pred = (data[:, fi] > t)
		acc = (pred == virginica).mean()
		if acc > best_acc:
			best_acc = acc
			best_fi = fi
			best_t = t
			
print("best acc: %f" % best_acc)
print("best featurei: %f" % best_fi)
print("best threshold: %f" % best_t)

####### DECISION TREES using graphviz
from sklearn import tree

tr = tree.DecisionTreeClassifier(min_samples_leaf=10)
tr.fit(data, labels)

import graphviz
tree.export_graphviz(tr, feature_names=feature_names, rounded=True, out_file='decision.dot')

##display doesnt work, need to point to graphviz exes
#from IPython.display import SVG
#graph = graphviz.Source(open('decision.dot').read())
#SVG(graph.pipe(format='svg'))

prediction = tr.predict(data)
print("Accuracy: {:.1%}".format(np.mean(prediction == labels)))

predictions = []
for i in range(len(data)):
    train_features = np.delete(data, i, axis=0)
    train_labels = np.delete(labels, i, axis=0)
    tr.fit(train_features, train_labels)
    predictions.append(tr.predict([data[i]]))
predictions = np.array(predictions)

#prediction by leaving one training sample out
from sklearn import model_selection
predictions = model_selection.cross_val_predict(
    tr,
    data,
    labels,
    cv=model_selection.LeaveOneOut()) 
print(np.mean(predictions == labels))

