
import codecs
import os
import pprint

import nltk
import gensim
import gensim.models.word2vec as w2v
import sklearn.manifold
import numpy as np
import pandas as pd
from nltk.corpus import stopwords
import matplotlib.pyplot as plt

from gensim.utils import simple_preprocess
import csv

#nltk.download('punkt')
#nltk.download('stopwords')

doc_list = []
with open('./daily_2015.csv') as csvfile: 
	csvreader = csv.reader(csvfile, delimiter='\t')
	line_count = 0
	for row in csvreader:
		
		if line_count == 0:
			print("-".join(row))
			print("")
		elif len(row) > 5:
			if line_count < 64:
#				print("%d %s" % (line_count, row[5]))
				doc_list.append(row[5])
			elif line_count > 64:
#				print("%d %s" % (line_count, row[4]))
				doc_list.append(row[4])
		line_count += 1;
		
stop_words = stopwords.words('english')
stop_words.extend(['iteration','total','work','wip','added'])

def sent_to_words(sentences):
	for sentence in sentences:
		yield(gensim.utils.simple_preprocess(str(sentence), deacc=True))
		
#convert documents to lists of words
data_words = list(sent_to_words(doc_list))

num_features = 15
min_word_count = 2
num_workers = 1
context_size = 3
downsampling = 1e-3
seed = 1

daily2vec = w2v.Word2Vec(sg=1, seed=seed, workers=num_workers, size=num_features, min_count = min_word_count,
						 window=context_size,sample=downsampling)
						 
daily2vec.build_vocab(data_words)
print("Vocab count: ", len(daily2vec.wv.vocab));
print("Shape: ", daily2vec.wv.vectors.shape);
#for word in daily2vec.wv.vocab:
#	print("wrd:", word);

daily2vec.train(data_words, total_examples=300, epochs=1)
daily2vec.wv.save("./daily2vec.w2v")
#can load the file again and train it..
#daily2vec = w2v.Word2Vec.load("./daily2vec.w2v").wv

#compress word vectors into 2d space and plot them
#TSNE means t-distributed Stochastic Neighbor Embedding. Dimensionality reduction tool
#reference:https://scikit-learn.org/stable/modules/generated/sklearn.manifold.TSNE.html
tsne = sklearn.manifold.TSNE(n_components=2, random_state = 0)

all_word_vectors_matrix = daily2vec.wv.vectors #weight matrix for input, https://stackoverflow.com/questions/41162876/get-weight-matrices-from-gensim-word2vec

all_word_vectors_matrix_2d = tsne.fit_transform(all_word_vectors_matrix)

points = pd.DataFrame(
	[
		(word, coords[0], coords[1])
		for word, coords in [
			(word, all_word_vectors_matrix_2d[daily2vec.wv.vocab[word].index])
			for word in daily2vec.wv.vocab
		]
	],
	columns=["word","x","y"]
)

print(points.head(10))

#sns.set_context("poster")
#points.plot.scatter("x", "y", s=10, figsize=(20, 12))



def plot_region(x_bounds, y_bounds):
    slice = points[
        (x_bounds[0] <= points.x) &
        (points.x <= x_bounds[1]) & 
        (y_bounds[0] <= points.y) &
        (points.y <= y_bounds[1])
    ]
    
    ax = slice.plot.scatter("x", "y", s=35, figsize=(10, 8))
    for i, point in slice.iterrows():
        ax.text(point.x + 0.005, point.y + 0.005, point.word, fontsize=8)



plot_region(x_bounds=(-20.0, 20.2), y_bounds=(-20, 20.))
		
plt.show()

#get list of most similar words
print(daily2vec.wv.most_similar("navmesh"))
print()
print(daily2vec.wv.most_similar("bugfix"))
print()

#distance, similarity, and ranking using dotp similarity
def nearest_similarity_cosmul(start1, end1, end2):
#answers the question given start1 and end1, what is a term similar to end2?
    similarities = daily2vec.most_similar_cosmul(
        positive=[end2, start1],
        negative=[end1]
    )
    start2 = similarities[0][0]
    print("{start1} is related to {end1}, as {start2} is related to {end2}".format(**locals()))
    return start2

print(nearest_similarity_cosmul("navmesh", "constraint", "skeletal"))

