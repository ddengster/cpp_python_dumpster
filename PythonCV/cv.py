import cv2
import numpy as np
from matplotlib import pyplot as plt

img = cv2.imread('lenna.png') #reads in an imageas BGR by default

b,g,r = cv2.split(img)       # get b,g,r
rgb_img = cv2.merge([r,g,b])     # switch it to rgb

#rgb_img = rgb_img - rgb_img.mean()
#print(img._type)

##threshold
img2 = cv2.imread('SimpleImageDataset/building05.jpg', cv2.IMREAD_GRAYSCALE) #reads in an imageas BGR by default
ret,thresholded = cv2.threshold(img2,164,255,cv2.THRESH_BINARY)

##convolution/gaussian blur
kernel = np.ones((5,5), np.float32) / (5 * 5)
print ("kernel:")
print (kernel)
blurred = cv2.filter2D(rgb_img,-1,kernel)
gblurred = cv2.GaussianBlur(rgb_img,(5,5), 1, 1)


plt.subplot(131),plt.imshow(rgb_img),plt.title('Original')
plt.subplot(132),plt.imshow(blurred),plt.title('Blurred')
plt.subplot(133),plt.imshow(gblurred),plt.title('GBlurred')

#plt.imshow(img2, cmap='gray', vmin=0, vmax=255)
#plt.imshow(thresholded, cmap='gray', vmin=0, vmax=255)
plt.show()

#cv2.imshow('image',img2)
#cv2.waitKey(0)
#cv2.destroyAllWindows()