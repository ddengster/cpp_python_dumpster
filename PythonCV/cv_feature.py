
import cv2
import numpy as np
from matplotlib import pyplot as plt

#img = cv2.imread('SimpleImageDataset/building05.jpg') #reads in an imageas BGR by default
img = cv2.imread('checkered2.png')
gray = cv2.cvtColor(img,cv2.COLOR_BGR2GRAY)

#harris corner detection (rotation invariant)
#https://opencv-python-tutroals.readthedocs.io/en/latest/py_tutorials/py_feature2d/py_features_harris/py_features_harris.html
#https://youtu.be/WyrVzTRZuXA?t=509
gray = np.float32(gray)
cornerimg = cv2.cornerHarris(gray,2,3,0.04)

#result is dilated for marking the corners, not important
dilated = cv2.dilate(cornerimg,None)
ret, dst = cv2.threshold(dilated,0.01*dilated.max(),255,0)
dst = np.uint8(dst)


# find centroids
ret, labels, stats, centroids = cv2.connectedComponentsWithStats(dst)

# define the criteria to stop and refine the corners
criteria = (cv2.TERM_CRITERIA_EPS + cv2.TERM_CRITERIA_MAX_ITER, 100, 0.001)
corners = cv2.cornerSubPix(gray,np.float32(centroids),(5,5),(-1,-1),criteria)

# Now draw them
res = np.hstack((centroids,corners))
res = np.int0(res)
img[res[:,1],res[:,0]]=[255,0,0]
img[res[:,3],res[:,2]] = [0,255,0]

#plt.subplot(131),plt.imshow(img, cmap='gray'),plt.title('Original')
#plt.subplot(132),plt.imshow(cornerimg, cmap='gray'),plt.title('Cornered1')
#plt.subplot(133),plt.imshow(dilated, cmap='gray'),plt.title('Cornered2')


# Initiate FAST object with default values
fast = cv2.FastFeatureDetector_create()
# find and draw the keypoints
kp = fast.detect(img,None)
img2 = cv2.drawKeypoints(img, kp, color=(255,0,0), outImage=gray)
# Print all default params
print("Threshold: ", fast.getThreshold())
print("nonmaxSuppression: ", fast.getNonmaxSuppression())
print("neighborhood: ", fast.getType())
print("Total Keypoints with nonmaxSuppression: ", len(kp))
plt.imshow(img2)

plt.show()

#cv2.imshow('image',img2)
#cv2.waitKey(0)
#cv2.destroyAllWindows()