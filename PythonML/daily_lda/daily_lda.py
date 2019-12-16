
#reference: https://www.machinelearningplus.com/nlp/topic-modeling-gensim-python/

import csv
import gensim
from gensim import corpora, models, similarities
from gensim.utils import simple_preprocess
import numpy as np
import os
from nltk.corpus import stopwords

if __name__ == "__main__": #coherence model uses some multiprocessing stuff, so we have to put this in

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

	#idx = 0
	#for r in doc_list:
	#	print(r)
	#	idx += 1

	######## DO SOME PREPROCESSSING

	stop_words = stopwords.words('english')
	stop_words.extend(['iteration','total','work','wip','added'])

	def sent_to_words(sentences):
		for sentence in sentences:
			yield(gensim.utils.simple_preprocess(str(sentence), deacc=True))
			
	#convert documents to lists of words
	data_words = list(sent_to_words(doc_list))
	#print(data_words)


	# Build the bigram and trigram models
	bigram = gensim.models.Phrases(data_words, min_count=5, threshold=10) # higher threshold fewer phrases.
	trigram = gensim.models.Phrases(bigram[data_words], threshold=10)  
	bigram_mod = gensim.models.phrases.Phraser(bigram)
	trigram_mod = gensim.models.phrases.Phraser(trigram)

	#print trigram
	print(bigram_mod[data_words[125]])
	print(trigram_mod[bigram_mod[data_words[125]]])

	#remove stopwords
	def remove_stopwords(texts):
		return [[word for word in simple_preprocess(str(doc)) if word not in stop_words] for doc in texts]

	def make_bigrams(texts):
		return [bigram_mod[doc] for doc in texts]

	def make_trigrams(texts):
		return [trigram_mod[bigram_mod[doc]] for doc in texts]

	data_words_nostops = remove_stopwords(data_words)

	# Form Bigrams
	data_words_bigrams = make_bigrams(data_words_nostops)
	print()
	#print(data_words_bigrams)
	print()

	#create
	id2word = corpora.Dictionary(data_words_bigrams)

	corpus = [id2word.doc2bow(text) for text in data_words_bigrams] 
	#https://kite.com/python/docs/gensim.corpora.dictionary.Dictionary.doc2bow

	print()
	print(id2word) #dictionary of words found
	print(id2word[60])
	#print(corpus) # corpus is a list of a list of with tuples(word index, frequency)
	print()

	# Human readable format of corpus (term-frequency)
	print()
	[[print(id2word[id], freq) for id, freq in cp] for cp in corpus[:10]]
	print()

	#do the LDA
	NUM_TOPICS = 20

	# Build LDA model
	lda_model = gensim.models.ldamodel.LdaModel(corpus=corpus,
											   id2word=id2word,
											   num_topics=NUM_TOPICS, 
											   random_state=100,
											   update_every=1,
											   chunksize=100,
											   passes=10,
											   alpha='auto',
											   per_word_topics=True)
											   
	# Print the Keywords and weightage in the list of 20 topics. From the words in each topic you can then infer what things are talked about
	print(lda_model.print_topics())
	
	#Evaluation methods for your topic modelling; compare with different models using the same evaluations (scores and topic results)
	#see https://www.youtube.com/watch?v=UkmIljRIG_M

	# Compute Perplexity
	print('\nPerplexity: ', lda_model.log_perplexity(corpus))  # a measure of how good the model is. lower the better.
	print()

	from gensim.models.coherencemodel import CoherenceModel

	# Compute Coherence Score; uses word subsets to help determine coherence
	coherence_model_lda = CoherenceModel(model=lda_model, texts=data_words_bigrams, dictionary=id2word, coherence='c_v')
	coherence_lda = coherence_model_lda.get_coherence()
	print('\nCoherence Score: ', coherence_lda)

	## visualization
	import pyLDAvis
	import pyLDAvis.gensim  # don't skip this

	#pyLDAvis.enable_notebook()
	#vis = pyLDAvis.gensim.prepare(lda_model, corpus, id2word)
	#pyLDAvis.save_html(vis, 'visualization.html')
	
	###LDA Mallet model
	from gensim.models.wrappers import LdaMallet
	
	os.environ['MALLET_HOME'] = 'C:\\Users\\Administrator\\Desktop\\test\\PythonML\\daily_lda\\mallet-2.0.8\\'
	mallet_path = 'C:\\Users\\Administrator\\Desktop\\test\\PythonML\\daily_lda\\mallet-2.0.8\\bin\\mallet' # update this path
	ldamallet = gensim.models.wrappers.LdaMallet(mallet_path, corpus=corpus, num_topics=NUM_TOPICS, id2word=id2word)

	# Show Topics
	print(ldamallet.show_topics(formatted=False))
	
	# Compute Coherence Score
	coherence_model_ldamallet = CoherenceModel(model = ldamallet, texts=data_words_bigrams, dictionary=id2word, coherence='c_v')
	coherence_ldamallet = coherence_model_ldamallet.get_coherence()
	print('\nCoherence Score (Mallet): ', coherence_ldamallet)
	
	#model = gensim.models.wrappers.ldamallet.malletmodel2ldamodel(ldamallet)
	#vis = pyLDAvis.gensim.prepare(model, corpus, id2word)
	#pyLDAvis.save_html(vis, 'visualization2.html')
	
	### Test with different topic counts to obtain the best choherence score
	def compute_coherence_values(dictionary, corpus, texts, limit, start=2, step=3):
		"""
		Compute c_v coherence for various number of topics

		Parameters:
		----------
		dictionary : Gensim dictionary
		corpus : Gensim corpus
		texts : List of input texts
		limit : Max num of topics

		Returns:
		-------
		model_list : List of LDA topic models
		coherence_values : Coherence values corresponding to the LDA model with respective number of topics
		"""
		coherence_values = []
		model_list = []
		for num_topics in range(start, limit, step):
			model = gensim.models.wrappers.LdaMallet(mallet_path, corpus=corpus, num_topics=num_topics, id2word=id2word)
			model_list.append(model)
			coherencemodel = CoherenceModel(model=model, texts=texts, dictionary=dictionary, coherence='c_v')
			coherence_values.append(coherencemodel.get_coherence())

		return model_list, coherence_values
	
	model_list, coherence_values = compute_coherence_values(dictionary=id2word, corpus=corpus, texts=data_words_bigrams, start=2, limit=15, step=3)
	
	# Show graph
	import matplotlib.pyplot as plt
	
	limit=15; start=2; step=3;
	x = range(start, limit, step)
	plt.plot(x, coherence_values)
	plt.xlabel("Num Topics")
	plt.ylabel("Coherence score")
	plt.legend(("coherence_values"), loc='best')
	#plt.show()
	
	# Print the coherence scores
	for m, cv in zip(x, coherence_values):
		print("Num Topics =", m, " has Coherence Value of", round(cv, 4))

	# Select the model and print the topics
	optimal_model = model_list[3]
	model_topics = optimal_model.show_topics(formatted=False)
	print(optimal_model.print_topics(num_words=10))
	
	import pandas as pd
	
	def format_topics_sentences(ldamodel=lda_model, corpus=corpus, texts=data_words_bigrams):
		# Init output
		sent_topics_df = pd.DataFrame()

		# Get main topic in each document
		for i, row in enumerate(ldamodel[corpus]):
			row = sorted(row, key=lambda x: (x[1]), reverse=True)
			# Get the Dominant topic, Perc Contribution and Keywords for each document
			for j, (topic_num, prop_topic) in enumerate(row):
				if j == 0:  # => dominant topic
					wp = ldamodel.show_topic(topic_num)
					topic_keywords = ", ".join([word for word, prop in wp])
					sent_topics_df = sent_topics_df.append(pd.Series([int(topic_num), round(prop_topic,4), topic_keywords]), ignore_index=True)
				else:
					break
		sent_topics_df.columns = ['Dominant_Topic', 'Perc_Contribution', 'Topic_Keywords']

		# Add original text to the end of the output
		contents = pd.Series(texts)
		sent_topics_df = pd.concat([sent_topics_df, contents], axis=1)
		return(sent_topics_df)


	df_topic_sents_keywords = format_topics_sentences(ldamodel=optimal_model, corpus=corpus, texts=data_words_bigrams)

	# Format
	df_dominant_topic = df_topic_sents_keywords.reset_index()
	df_dominant_topic.columns = ['Document_No', 'Dominant_Topic', 'Topic_Perc_Contrib', 'Keywords', 'Text']

	# Show
	print("Finding the dominant topic in each sentence:")
	print(df_dominant_topic.head(30))
	
	# Group top 5 sentences under each topic
	sent_topics_sorteddf_mallet = pd.DataFrame()

	sent_topics_outdf_grpd = df_topic_sents_keywords.groupby('Dominant_Topic')

	for i, grp in sent_topics_outdf_grpd:
		sent_topics_sorteddf_mallet = pd.concat([sent_topics_sorteddf_mallet, 
												 grp.sort_values(['Perc_Contribution'], ascending=[0]).head(1)], 
												axis=0)

	# Reset Index    
	sent_topics_sorteddf_mallet.reset_index(drop=True, inplace=True)

	# Format
	sent_topics_sorteddf_mallet.columns = ['Topic_Num', "Topic_Perc_Contrib", "Keywords", "Text"]

	# Show
	print("Find the most representative document for each topic:")
	print(sent_topics_sorteddf_mallet.head(10))
	
	##Topic distribution across documents
	# Number of Documents for Each Topic
	topic_counts = df_topic_sents_keywords['Dominant_Topic'].value_counts()

	# Percentage of Documents for Each Topic
	topic_contribution = round(topic_counts/topic_counts.sum(), 4)

	# Topic Number and Keywords
	topic_num_keywords = df_topic_sents_keywords[['Dominant_Topic', 'Topic_Keywords']]

	# Concatenate Column wise
	df_dominant_topics = pd.concat([topic_num_keywords, topic_counts, topic_contribution], axis=1)

	# Change Column names
	df_dominant_topics.columns = ['Dominant_Topic', 'Topic_Keywords', 'Num_Documents', 'Perc_Documents']

	# Show
	print("Topic distribution across documents")
	print(df_dominant_topics)
	