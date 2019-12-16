import pandas
from pinterest_scraper import scraper as s

ph = s.Pinterest_Helper("liangdeng.fan@gmail.com" , "mrscraper1234")
#images = ph.runme_alt("https://www.pinterest.com/pin/437341813813302414/", threshold=20)
#images = ph.runme("https://www.pinterest.com/search/pins/?q=game%20icon&rs=typed&term_meta[]=game%7Ctyped&term_meta[]=icon%7Ctyped", threshold=20)
#images = ph.runme_alt("https://www.pinterest.com/search/pins/?q=game%20icon&rs=typed&term_meta[]=game%7Ctyped&term_meta[]=icon%7Ctyped", threshold=20)

#print(images)

#s.download(images, "./images")
cmd = ''
while cmd != '/quit' :
	cmd = input("waiting for pinterest url: ")
	try:
		if cmd != '/quit':
			images = ph.runme_alt(cmd, threshold=500)
			print("downloading {0} images " % len(images))
			s.download(images, "./images")
	except Exception as e:
		print("type error: " + str(e))
print("done")