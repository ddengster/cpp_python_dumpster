

import numpy as np
import matplotlib.pyplot as plt
import mpl_toolkits.mplot3d as m3d
#usage for singular value decomposition

##3d plot setup

x2 = np.array([0, 1, 2, 3, 4])
y2 = np.array([0, 2, 4, 6, 8])
z2 = np.array([0, 4, 8, 12, 18])

data = np.concatenate((x2[:, np.newaxis], 
                       y2[:, np.newaxis], 
                       z2[:, np.newaxis]), 
                      axis=1)

print("data:")					  
print(data)

# Calculate the mean of the points, i.e. the 'center' of the cloud
datamean = data.mean(axis=0)
print("mean: ", datamean)

# Do an SVD on the mean-centered data.
#tries to factorize the matrix into an initial rotation, a diagonal-only nonzero matrix (squashed to singular values), and another rotation
#https://docs.scipy.org/doc/numpy/reference/generated/numpy.linalg.svd.html
#https://en.wikipedia.org/wiki/Singular_value_decomposition
uu, ss, vv = np.linalg.svd(data - datamean)
print("u:")
print(uu)
print("s:")
print(ss)
print("v:")
print(vv)

# I use -7, 7 since the spread of the data is roughly 14
# and we want it to have mean 0 (like the points we did
# the svd on). Also, it's a straight line, so we only need 2 points.
linepts = vv[0] * np.mgrid[-10:20:2j][:, np.newaxis]

# shift by the mean to get the line in the right place
linepts += datamean
print("\nlinepts:")
print(linepts)

# Verify that everything looks right.
ax = m3d.Axes3D(plt.figure())
ax.scatter3D(*data.T)
ax.plot3D(*linepts.T)

'''
#orthnormal axis of the roation mtx
linepts = vv[1] * np.mgrid[-10:20:2j][:, np.newaxis]
linepts += datamean
ax.plot3D(*linepts.T)

linepts = vv[2] * np.mgrid[-10:20:2j][:, np.newaxis]
linepts += datamean
ax.plot3D(*linepts.T)
'''
plt.show()