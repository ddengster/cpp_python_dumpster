
import cv2
import numpy as np
from matplotlib import pyplot as plt

queryimg = cv2.imread('SimpleImageDataset/building05.jpg',cv2.COLOR_BGR2RGB) #reads in an imageas BGR by default
searchimg = cv2.imread('building05_cropped.jpg',cv2.COLOR_BGR2RGB) # trainImage


# Initiate SIFT detector
orb = cv2.ORB_create()

# find the keypoints and descriptors with SIFT
kp1, des1 = orb.detectAndCompute(queryimg,None)
kp2, des2 = orb.detectAndCompute(searchimg,None)

# create BFMatcher object
bf = cv2.BFMatcher(cv2.NORM_L1, crossCheck=False)

# Match descriptors.
clusters = np.array([des1])
bf.add(clusters)
bf.train()

matches = bf.match(des2)
# Sort them in the order of their distance.
matches = sorted(matches, key = lambda x:x.distance)
print(len(matches))

# Draw first 10 matches.
imgz = 0
img3 = cv2.drawMatches(searchimg,kp2,queryimg,kp1,matches[:10], flags=2, outImg=imgz)

#plt.subplot(131),plt.imshow(img, cmap='gray'),plt.title('Original')
#plt.subplot(132),plt.imshow(cornerimg, cmap='gray'),plt.title('Cornered1')
#plt.subplot(133),plt.imshow(dilated, cmap='gray'),plt.title('Cornered2')

plt.imshow(img3)
#plt.imshow(des2)

plt.show()

#cv2.imshow('image',img2)
#cv2.waitKey(0)
#cv2.destroyAllWindows()