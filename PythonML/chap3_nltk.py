import nltk
import scipy as sp

## Probably showing off a bunch of text pre-processing techniques
# The concept of 'stem' is a bunch of terms that are without their morphological affixes. The process is known as stemming
#https://www.nltk.org/api/nltk.stem.html#module-nltk.stem.api


import nltk.stem

s = nltk.stem.SnowballStemmer('english') #https://www.nltk.org/_modules/nltk/stem/snowball.html#SnowballStemmer
print(s.stem("graphics")) #remove the plurality of graphics

#other examples.. note stemming does not result in valid english words
print(s.stem("imaging")) 
print(s.stem("images")) 

print(s.stem("imagination")) 
print(s.stem("imagine")) 

#these do not mean the same to the stemmer though
print(s.stem("buys")) 
print(s.stem("bought")) 

####### Import some data and try to fit it into the Vectorizer, 
#we'll train it with some data via fit_transform and fit some more new data into it via .transform
import sklearn.datasets
from sklearn.feature_extraction.text import TfidfVectorizer
#We'll use the termfrequency - inverse document frequency (TF-IDF) vectorizer
#https://scikit-learn.org/stable/modules/feature_extraction.html#tfidf-term-weighting

english_stemmer = nltk.stem.SnowballStemmer('english')
class StemmedCountVectorizer (TfidfVectorizer):
	def build_analyzer(self):
		analyzer = super(TfidfVectorizer, self).build_analyzer()
		return lambda doc: (english_stemmer.stem(w) for w in analyzer(doc))
		
vectorizer = StemmedCountVectorizer(min_df=10, max_df=0.5, stop_words='english', decode_error='ignore')

all_data = sklearn.datasets.fetch_20newsgroups(subset="all")
print("Number of total posts: %i" % len(all_data.filenames))
# Number of total posts: 18846

groups = [
    'comp.graphics', 'comp.os.ms-windows.misc', 'comp.sys.ibm.pc.hardware',
    'comp.sys.mac.hardware', 'comp.windows.x', 'sci.space']
train_data = sklearn.datasets.fetch_20newsgroups(subset="train",
                                                 categories=groups)
print("Number of training posts in tech groups:", len(train_data.filenames))
# Number of training posts in tech groups: 3529

labels = train_data.target
num_clusters = 50  # sp.unique(labels).shape[0]
#print(train_data.data[0])
tfidf_matrix = vectorizer.fit_transform(train_data.data) #form a matrix of terms(words/features) to documents
num_samples, num_features = tfidf_matrix.shape
print("#samples: %d, #features: %d" % (num_samples, num_features))


new_post = \
    """Disk drive problems. Hi, I have a problem with my hard disk.
After 1 year it is working only sporadically now.
I tried to format it, but now it doesn't boot any more.
Any ideas? Thanks.
"""
print("%s"% vectorizer.get_feature_names()[1481])
new_post_vec = vectorizer.transform([new_post])
print(new_post_vec)

####### Plug stuff into KMeans algo, detect similar posts so we can cluster them together
from sklearn.cluster import KMeans

km = KMeans(n_clusters=num_clusters, n_init=1, verbose=1, random_state=3)
clustered = km.fit(tfidf_matrix)

print("km.labels_=%s" % km.labels_)
# km.labels_=[ 6 34 22 ...,  2 21 26]

print("km.labels_.shape=%s" % km.labels_.shape)
# km.labels_.shape=3529

from sklearn import metrics
print("Homogeneity: %0.3f" % metrics.homogeneity_score(labels, km.labels_))
# Homogeneity: 0.400
print("Completeness: %0.3f" % metrics.completeness_score(labels, km.labels_))
# Completeness: 0.206
print("V-measure: %0.3f" % metrics.v_measure_score(labels, km.labels_))
# V-measure: 0.272
print("Adjusted Rand Index: %0.3f" %
      metrics.adjusted_rand_score(labels, km.labels_))
# Adjusted Rand Index: 0.064
print("Adjusted Mutual Information: %0.3f" %
      metrics.adjusted_mutual_info_score(labels, km.labels_))
# Adjusted Mutual Information: 0.197
print(("Silhouette Coefficient: %0.3f" %
       metrics.silhouette_score(tfidf_matrix, labels, sample_size=1000)))
# Silhouette Coefficient: 0.006

new_post_label = km.predict(new_post_vec)[0]

similar_indices = (km.labels_ == new_post_label).nonzero()[0]

similar = []
for i in similar_indices:
    dist = sp.linalg.norm((new_post_vec - tfidf_matrix[i]).toarray())
    similar.append((dist, train_data.data[i]))

similar = sorted(similar)
print("Count similar: %i" % len(similar))

show_at_1 = similar[0]
show_at_2 = similar[int(len(similar) / 10)]
show_at_3 = similar[int(len(similar) / 2)]

print("=== #1 ===")
print(show_at_1)
print()

print("=== #2 ===")
print(show_at_2)
print()

print("=== #3 ===")
print(show_at_3)

## Plotting kmeans clustering stuff, http://brandonrose.org/clustering
import matplotlib.pyplot as plt
import pandas as pd

from sklearn.metrics.pairwise import cosine_similarity
dist = 1 - cosine_similarity(tfidf_matrix)

from sklearn.manifold import MDS
MDS()

# convert two components as we're plotting points in a two-dimensional plane
# "precomputed" because we provide a distance matrix
# we will also specify `random_state` so the plot is reproducible.
mds = MDS(n_components=2, dissimilarity="precomputed", random_state=1)
print("fitting mds..")
pos = mds.fit_transform(dist)  # shape (n_components, n_samples)
xs, ys = pos[:, 0], pos[:, 1]


cluster_names = vectorizer.get_feature_names()
#set up colors per clusters using a dict
cluster_colors = {0: '#1b9e77', 1: '#d95f02', 2: '#7570b3', 3: '#e7298a', 4: '#66a61e'}

df = pd.DataFrame(dict(x=xs, y=ys, label=clusters, title=titles)) 
groups = df.groupby('label')
centers = km.cluster_centers_
i = 0
for name, group in groups:
	plt.plot(group.x, group.y, marker='o', linestyle='', ms=12, label=cluster_names[name], color=cluster_colors[name], mec='none')

#for n in cluster_names:
#	plt.plot(, label=cluster_names[i], marker='o', color=cluster_colors[i%5])
#++i
#plt.scatter(centers[:,0], centers[:,1], c=km.labels_,s=20, alpha=0.5)
plt.show()

