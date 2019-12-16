
#reference: https://opencv-python-tutroals.readthedocs.io/en/latest/py_tutorials/py_calib3d/py_calibration/py_calibration.html#calibration
#Calibration (with math): https://www.youtube.com/watch?v=4-thTdR7Blg
#Technique is also known as plane based calibration

import numpy as np
import cv2
import glob


# termination criteria
criteria = (cv2.TERM_CRITERIA_EPS + cv2.TERM_CRITERIA_MAX_ITER, 30, 0.001)

# prepare object points, like (0,0,0), (1,0,0), (2,0,0) ....,(6,5,0)
objp = np.zeros((6*7,3), np.float32)
objp[:,:2] = np.mgrid[0:7,0:6].T.reshape(-1,2)
print(objp)

# Arrays to store object points and image points from all the images.
objpoints = [] # 3d point in real world space
imgpoints = [] # 2d points in image plane.


images = glob.glob('calibration/*.jpg')
#print(images)

#do visual confirmation that all the dots line up correctly
for fname in images:
	img = cv2.imread(fname)
	gray = cv2.cvtColor(img,cv2.COLOR_BGR2GRAY)

	# Find the chess board corners
	ret, corners = cv2.findChessboardCorners(gray, (7,6),None)
	
	# If found, add object points, image points (after refining them)
	if ret == True:
		objpoints.append(objp)
		#https://docs.opencv.org/2.4/modules/imgproc/doc/feature_detection.html?highlight=cornersubpix#cornersubpix
		corners2 = cv2.cornerSubPix(gray,corners,(11,11),(-1,-1),criteria)
		imgpoints.append(corners2)

		# Draw and display the corners
		img = cv2.drawChessboardCorners(img, (7,6), corners2,ret)
		cv2.imshow('img',img)
		cv2.waitKey(0)
		
#use the points found to build up our distance coeff and camera mtx
ret, cammtx, distort_coeff, rvecs, tvecs = cv2.calibrateCamera(objpoints, imgpoints, gray.shape[::-1],None,None)
print("camera mtx:")
print(cammtx)
print("distance coeff:")
print(distort_coeff)

img = cv2.imread('calibration/left12.jpg')
h,  w = img.shape[:2]

#undistortion operation our 
newcameramtx, roi= cv2.getOptimalNewCameraMatrix(cammtx,distort_coeff,(w,h),1,(w,h))
print("new optimal cam mtx")
print(newcameramtx)
print("roi:")
print(roi)

##method 1: use cv2.undistort
# undistort
dst = cv2.undistort(img, cammtx, distort_coeff, None, newcameramtx)

# crop the image
x,y,w,h = roi
dst = dst[y:y+h, x:x+w]
cv2.imwrite('calibresult.png',dst)

##method 2: remapping
# undistort
mapx,mapy = cv2.initUndistortRectifyMap(cammtx,distort_coeff,None,newcameramtx,(w,h),5)
dst = cv2.remap(img,mapx,mapy,cv2.INTER_LINEAR)

# crop the image
x,y,w,h = roi
dst = dst[y:y+h, x:x+w]
cv2.imwrite('calibresult2.png',dst) #results should be the same as calibresult.png

##reprojection error calculation, should be as close to 0 as possible
mean_error = 0
i = 0
for k in objpoints:
    imgpoints2, _ = cv2.projectPoints(objpoints[i], rvecs[i], tvecs[i], cammtx, distort_coeff) #transform objpoint to imagepoint
    error = cv2.norm(imgpoints[i],imgpoints2, cv2.NORM_L2)/len(imgpoints2) #compute the error to the corresponding imagepoint found earlier
    mean_error += error
    i += 1

print("total error: ", mean_error/len(objpoints))