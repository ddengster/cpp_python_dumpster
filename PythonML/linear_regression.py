

import numpy as np
import matplotlib.pyplot as plt


x = np.array([0, 1, 2, 3])
y = np.array([-1, 0.2, 0.9, 2.1])

#setup matrix
A = np.vstack([x, np.ones(len(x))]).T
print(A)

#least squares
m, c = np.linalg.lstsq(A, y, rcond=None)[0]

plt.plot(x, y, 'o')
plt.plot(x, m*x + c, 'r')
#plt.show()

##3d plot setup

x2 = np.array([0, 1, 2, 3, 4])
y2 = np.array([0, 2, 4, 6, 8])
z2 = np.array([0, 4, 8, 12, 16])


#'''
### good rendering of a 3d plotted line, but has no good function to fit in values
print("\n==============\n")
from mpl_toolkits.mplot3d import Axes3D

#fig = plt.figure()
#ax = fig.add_subplot(111,projection='3d')

def randrange(n, vmin, vmax):
	return (vmax - vmin)*np.random.rand(n) + vmin

# this will find the slope and x-intercept of a plane parallel to the y-axis that best fits the data
A_xz = np.vstack((x2, np.ones(len(x2)))).T
print("A_xz:")
print(A_xz)
m_xz, c_xz = np.linalg.lstsq(A_xz, z2)[0]
print("xz coeff: {0} intercept: {1}\n".format(m_xz, c_xz))

# again for a plane parallel to the x-axis
A_yz = np.vstack((y2, np.ones(len(y2)))).T
print("A_yz:")
print(A_yz)
m_yz, c_yz = np.linalg.lstsq(A_yz, z2)[0]
print("xz coeff: {0} intercept: {1}\n".format(m_yz, c_yz))

#do plotting of implicit eqn
def lin(z):
    x = (z - c_xz)/m_xz
    y = (z - c_yz)/m_yz
    return x,y
	
#verifying:
fig = plt.figure()
ax = Axes3D(fig)

ax.set_xlabel('X')
ax.set_ylabel('Y')
ax.set_zlabel('Z')

ax.scatter(x2, y2, z2)


zz = np.linspace(0,30) #define an array of outputs for z
#print(zz)
xx,yy = lin(zz) #do algebra of the 2 eqns that we found to get our list of inputs
ax.plot(xx,yy,zz)

#ax.plot(x2,y2,m_xz * x2 + m_yz * y2 + (c_xz + c_yz))

#todo: fitting?
plt.show()

#'''

'''
#bad fitting example showing how  not to use LinearRegression to fit 3d data points
from mpl_toolkits.mplot3d import Axes3D

from sklearn.linear_model import LinearRegression

#x2_with_constant = np.vstack([x2, np.ones(len(x2))]).T
#print("x2_with_constant: ")
#print(x2_with_constant)
#lr = LinearRegression(fit_intercept=False)
#lr.fit(x2_with_constant, y2)
#print("coef: %f intercept: %f" % (lr.coef_[0], lr.intercept_))

xy2 = np.vstack([x2, y2]).T
print("xy2:")
print(xy2)

onez = np.vstack(np.ones(len(x2)))
print(onez) 
#xy2_with_constant = np.hstack([xy2, np.ones(len(x2))]).T
xy2_with_constant = np.concatenate((xy2, onez), axis=1)
#xy2_with_constant = np.append(xy2, onez)
print("xy2_with_constant: ")
print(xy2_with_constant)
lr = LinearRegression(fit_intercept=False)
lr.fit(xy2_with_constant, z2)
print("coef: {0} intercept: {1}".format(lr.coef_, lr.intercept_))

fig = plt.figure()
ax = fig.add_subplot(111,projection='3d')
ax.set_xlabel('X')
ax.set_ylabel('Y')
ax.set_zlabel('Z')

ax.scatter3D(xs=x2, ys=y2, zs=z2, c=None)

lx = np.linspace(0., 14)
ly = np.linspace(0., 14)
ax.plot(lx, ly, lr.coef_[0] * lx + lr.coef_[1] * ly + lr.coef_[2], [1])  #implicit equation plotting
#ax.plot_surface(A,B, z, Z)
#ax.plot3D(
plt.show()

'''