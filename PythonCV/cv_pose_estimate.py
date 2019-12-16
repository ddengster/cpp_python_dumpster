
#reference: https://opencv-python-tutroals.readthedocs.io/en/latest/py_tutorials/py_calib3d/py_pose/py_pose.html

#Calibration background (with math): https://www.youtube.com/watch?v=4-thTdR7Blg
#https://dsp.stackexchange.com/questions/2736/step-by-step-camera-pose-estimation-for-visual-tracking-and-planar-markers
import numpy as np
import cv2
import glob


# termination criteria
criteria = (cv2.TERM_CRITERIA_EPS + cv2.TERM_CRITERIA_MAX_ITER, 30, 0.001)

# prepare object points, like (0,0,0), (1,0,0), (2,0,0) ....,(6,5,0)
objp = np.zeros((6*7,3), np.float32)
objp[:,:2] = np.mgrid[0:7,0:6].T.reshape(-1,2)
#print(objp)

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
		#img = cv2.drawChessboardCorners(img, (7,6), corners2,ret)
		#cv2.imshow('img',img)
		#cv2.waitKey(0)
		
#use the points found to build up our distance coeff and camera mtx
ret, cammtx, distort_coeff, rotransvecs, transvecs = cv2.calibrateCamera(objpoints, imgpoints, gray.shape[::-1],None,None)
print("camera mtx:")
print(cammtx)
print("distance coeff:")
print(distort_coeff)


def drawAxis(img, corners, imgpts):
	corner = tuple(corners[0].ravel())
	img = cv2.line(img, corner, tuple(imgpts[0].ravel()), (255,0,0), 5)
	img = cv2.line(img, corner, tuple(imgpts[1].ravel()), (0,255,0), 5)	
	img = cv2.line(img, corner, tuple(imgpts[2].ravel()), (0,0,255), 5)
	return img

	
points = np.float32([[3,0,0], [0,3,0], [0,0,-3]]).reshape(-1,3)

for fname in images:
	img = cv2.imread(fname)
	gray = cv2.cvtColor(img,cv2.COLOR_BGR2GRAY)
	
	# Find the chess board corners
	ret, corners = cv2.findChessboardCorners(gray, (7,6),None)
	
	# If found, add object points, image points (after refining them)
	if ret == True:
		corners2 = cv2.cornerSubPix(gray,corners,(11,11),(-1,-1),criteria)
		# Find the rotation and translation vectors.
		result, rotransvecs2, transvecs2, inliers = cv2.solvePnPRansac(objp, corners2, cammtx, distort_coeff)
		# project 3D points to image (2d) space
		imgpts, jac = cv2.projectPoints(points, rotransvecs2, transvecs2, cammtx, distort_coeff)
		
		img = drawAxis(img,corners2,imgpts)
		cv2.imshow('img',img)
		k = cv2.waitKey(0) & 0xff
		if k == 's':
			cv2.imwrite(fname[:6]+'.png', img)

		
		
cv2.destroyAllWindows()
