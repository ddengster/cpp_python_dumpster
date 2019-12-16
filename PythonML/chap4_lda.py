
import time
from gensim import corpora, models, similarities
import numpy as np

corpus = corpora.BleiCorpus('./data/ap/ap.dat', './data/ap/vocab.txt')

start = time.time()
model = models.ldamodel.LdaModel(corpus, num_topics=100, id2word=corpus.id2word, random_state=55)
end = time.time()
print("training time:%f seconds" % (end -start))

topics = [model[c] for c in corpus]
print(type(topics)) #list
print(len(topics))
print(type(topics[0])) 
print(topics[0])

dimcount = 100
mtx_topics = np.zeros((len(topics), dimcount), float) #reduce dimensions to 100 for faster computation
for i, t in enumerate(topics): #enumerate(topic) gives a list with tuples (iteration_idx, list[iteration_idx])
	for j, v in t:
		mtx_topics[i, j] = v
		
print(mtx_topics[0])		

from scipy.spatial import distance

pairwise_distances = distance.squareform(distance.pdist(mtx_topics))
largest = pairwise_distances.max()
for i in range(len(topics)):
	pairwise_distances[i, i] = largest + 1

print ("largest dist: %f" % largest)
print ("pairwise_distances:")
print (pairwise_distances)

#define a closest_to function which determines which topic the document is closest to
def closest_to(doc_id):
	return pairwise_distances[doc_id].argmin() #the minimum of the list

search_doc_id = 1
print("closest topic for the following:")
print(corpus[search_doc_id])
print("is this : ")
print(closest_to(search_doc_id))
print(corpus[closest_to(search_doc_id)])
