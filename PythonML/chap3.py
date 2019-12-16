#image classification, with simple provided measurements

from matplotlib import pyplot as plt
from sklearn.feature_extraction.text import CountVectorizer

#########
# vectorizer get the number of occurances of each word in each part of the content array
vectorizer = CountVectorizer(min_df=1)
#print(vectorizer)

content = ["How to format asd asd", "asd ar format problems"]
X = vectorizer.fit_transform(content)
print(vectorizer.get_feature_names()) #prints out all unique words

#print(X)
print(X.toarray().transpose())
#example, the word asd is found twice in content[0] and once content[1], so its representation is (2, 1)

#https://scikit-learn.org/stable/modules/feature_extraction.html#text-feature-extraction

########

import os

dir = "./toy/"
posts = [open(os.path.join(dir, f)).read() for f in os.listdir(dir)]
print(posts)

X_train = vectorizer.fit_transform(posts)

numsamples, numfeatures = X_train.shape
print("samples: %d, features: %d" %( numsamples, numfeatures))

print(vectorizer.get_feature_names()) #prints out all unique words

new_post = "imaging databases"
new_post_vec = vectorizer.transform([new_post])

#print(new_post_vec)
print(new_post_vec.toarray()) #prints out the number of occurances of words(features) in the vectorizer

import sys
import scipy as sp
def dist_raw(v1, v2):
	delta = v1 - v2
	return sp.linalg.norm(delta.toarray())
	
def dist_norm(v1, v2):
    v1_normalized = v1 / sp.linalg.norm(v1.toarray())
    v2_normalized = v2 / sp.linalg.norm(v2.toarray())

    delta = v1_normalized - v2_normalized

    return sp.linalg.norm(delta.toarray())

distfunc = dist_norm

best_dist = sys.maxsize
best_i = None

for i in range(0, numsamples):
    post = posts[i]
    if post == new_post:
        continue
    post_vec = X_train.getrow(i)
    d = distfunc(post_vec, new_post_vec)

    print("=== Post %i with dist=%.2f: %s" % (i, d, post))

    if d < best_dist:
        best_dist = d
        best_i = i

print("Best post is %i with dist=%.2f" % (best_i, best_dist))
