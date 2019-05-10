def identifyKeywords(languages, source):
	keywords = {}
	scores = {}

	#parse source
	num_words = 0.0
	wordlist = []
	translationTable = '                                                                 ABCDEFGHIJKLMNOPQRSTUVWXYZ      abcdefghijklmnopqrstuvwxyz                                                                                                                                     '


	for line in source:
		if line.strip() == '':
			continue
		temp = line.translate(translationTable).strip()
		
		reading = False
		start = 0
		for i in range(len(temp)):
			if reading == False and temp[i] != ' ':
				start = i
				reading = True
			elif reading == True and temp[i] == ' ':
				num_words += 1
				wordlist.append(temp[start:i])
				reading = False
		if reading == True:
			num_words += 1
			wordlist.append(temp[start:])

	for word in wordlist:
		if keywords.has_key(word):
			keywords[word] += 1
		else:
			keywords[word] = 1


	for lang in languages:
		#read database for each language into memory
		databasefile = open('./database/'+lang+'/keywords.txt', 'r')
		lines = databasefile.readlines()
		database_keywords = []
		
		#database_operator[i] = [frequency of occurence, operator]
		for line in lines:
			database_keywords.append([])
			database_keywords[-1].append(int(line.strip().split(" ")[1]))
			database_keywords[-1].append(line.split(" ")[0])
		database_keywords.sort()

		summed = 0.0
		for i in database_keywords:
			summed += i[0]

		#lang_score = 1/sum((%freq of keyword in source - %freq of keyword in database)^2) where sum is for the 5 most common characters in the language
		i = -1
		lang_score = 0
		while i > -1 - keywordNum and i*-1 <= len(database_keywords):
			if summed == 0:
				summed = 0.00000000001
			if num_words == 0:
				num_words = 0.00000000001
			if keywords.has_key(database_keywords[i][1]):
				lang_score += ((database_keywords[i][0]/summed - keywords[database_keywords[i][1]]/num_words)/(database_keywords[i][0]/summed))**2
			else:
				lang_score += 1
			i -= 1		
		if keywordNum > len(database_keywords):
			lang_score *= keywordNum/len(database_keywords)
		if lang_score == 0:
			lang_score = 0.000000000000000000000001
		lang_score = 1 / lang_score
		scores[lang] = lang_score
		databasefile.close()

	summed_scores = 0
	for lang in languages:
		summed_scores += scores[lang]

	for lang in languages:
		try:
			scores[lang] /= summed_scores
		except ZeroDivisionError:
			scores[lang] = 0

	return scores