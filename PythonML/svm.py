
import numpy as np
import matplotlib.pyplot as plt
import mpl_toolkits.mplot3d as m3d
from sklearn import svm

data = np.array([ [0, 0.1], [1, 1.8], [2, 4.4], [3,6.5], [4, 8.6]])
class_labels = np.array([1, 1, 2, 2, 3])
#z2 = np.array([0, 4, 8, 12, 18])

#https://scikit-learn.org/stable/modules/generated/sklearn.svm.SVC.html#sklearn.svm.SVC.fit
					  
#model = svm.SVC(kernel='linear', C=1.0, gamma=1) 
model = svm.SVC(kernel='rbf', C=1.0, gamma=1) 
model.fit(data, class_labels)

print("num support vectors (size = numclasses): ", model.n_support_)
print(model.support_vectors_)

data2 = np.array([ [0, 0.6], [1, 1.4], [2, 4.7], [3,8.2], [4, 10.6]])

score = model.score(data2, class_labels)
print("score: ", score)

#Predict Output
x_test = np.array([[1, 1.2]])
predicted= model.predict(x_test)
print("predicted class label: ", predicted)

print("plotting..")
#plotting
# create a mesh to plot in
#x_min, x_max = data[:, 0].min() - 1, data[:, 0].max() + 1
#y_min, y_max = data[:, 1].min() - 1, data[:, 1].max() + 1
x_min = 0
x_max = 10
y_min = 0
y_max = 10
h = 1
xx, yy = np.meshgrid(np.arange(x_min, x_max, h), np.arange(y_min, y_max, h))
print(xx)
print(yy)
 
plt.subplot(1, 1, 1)
Z = model.predict(np.c_[xx.ravel(), yy.ravel()])
Z = Z.reshape(xx.shape)
plt.contourf(xx, yy, Z, cmap=plt.cm.Paired, alpha=0.8)

plt.scatter(data[:, 0], data[:, 1], c=class_labels, cmap=plt.cm.Paired)
plt.xlabel('X')
plt.ylabel('Y')
plt.xlim(xx.min(), xx.max())
plt.title('SVC with linear kernel')
plt.show()