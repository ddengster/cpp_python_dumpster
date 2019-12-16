import scipy as sp
import matplotlib.pyplot as plt

data = sp.genfromtxt("web_traffic.tsv", delimiter="\t")

x = data[:,0]
y = data[:,1]

x = x[~sp.isnan(y)]
y = y[~sp.isnan(y)]

plt.scatter(x,y)
plt.title("web traffic")
plt.xlabel("time")
plt.ylabel("Hits/hour")
plt.xticks([w*7*24 for w in range(10)], ['week %i'%w for w in range(10)])
plt.autoscale(tight=True)
plt.grid()
#plt.show()

def error(f, x, y):
	return sp.sum((f(x) - y) ** 2) #** is the power operator
	
#fit a first degree polymial onto the dataset
fp1, residuals, rank, sv, rcond = sp.polyfit(x, y, 1, full=True)
print("Model params: %s" % fp1) #gives coefficients of a 1-degree polynomial (constant + coeff of x^1)
print(residuals)

f1 = sp.poly1d(fp1) #make a polyline
print("1deg error: %f" % error(f1, x, y)) 

fx =sp.linspace(0, x[-1], 1000)
plt.plot(fx, f1(fx), linewidth=3, color='red')
#plt.legend(["d=%i" % f1.order], loc="upper left")
#plt.show()

#fit a second degree polymial onto the dataset
f2p =sp.polyfit(x,y,2)
print(f2p)

f2 = sp.poly1d(f2p)
print("2deg error: %f" % error(f2, x, y))

fx = sp.linspace(0, x[-1], 1000)
plt.plot(fx, f2(fx), linewidth=3, color='green')
plt.legend(["d=%i" % f1.order, "d=%i" % f2.order], loc="upper left")
#plt.show()	

############ Attempt at fitting multiple lines at an inflection pt
inflection = int(3.5 * 7 * 24) # calculate inflection point in hours
xa = x[:inflection] #data before
ya = y[:inflection]
xb = x[inflection:] #data after
yb = y[inflection:]

fa = sp.poly1d(sp.polyfit(xa, ya, 1))
fb = sp.poly1d(sp.polyfit(xb, yb, 1))

fa_err = error(fa, xa, ya)
fb_err = error(fb, xb, yb)
print("error inflection= %f" % (fa_err + fb_err))
#plt.show()

## based on our 2d polynomial, when will we reach 100000 requests?
print(f2 - 1000000)

from scipy.optimize import fsolve
reached_max = fsolve(f2 - 100000, 800)/(7*24)
print("100k hits/hour expected at week %f" % reached_max[0])



