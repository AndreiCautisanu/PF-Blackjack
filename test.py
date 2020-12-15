import io
import nltk
from nltk.corpus import stopwords
import json
import re

stop_words = set(stopwords.words('romanian'))
eminescu = open("eminescu.txt")
eminescu_stripped = open("eminescu_stripped.txt", 'w')

lines = eminescu.readlines()
print(len(lines))


word_frequencies = dict()

i = 0
for line in lines:
    words = re.split(r'\W+', line)
    words = list(filter(lambda w: w != '', words))
    if 30 < i < 50:
        print(words)
    for word in words:
        if not word.lower() in stop_words:
            eminescu_stripped.write(" " + word)

            if word.lower() in word_frequencies:
                word_frequencies[word.lower()] += 1
            else:
                word_frequencies[word.lower()] = 1
        
    eminescu_stripped.write('\n')
    i += 1

word_frequencies_sorted = {k: v for k, v in sorted(word_frequencies.items(), key=lambda item: item[1], reverse=True)}
print(len(word_frequencies.items()))

fo = open("dict.txt", 'w')
for k, v in word_frequencies_sorted.items():
    fo.write(str(k) + ': ' + str(v) + '\n')

fo.close()

unique_words = len(word_frequencies.items())
language_model = {k: round((v / unique_words * 1000000), 3) for k, v in word_frequencies_sorted.items()}

fo = open("lang_model.txt", 'w')
for k, v in language_model.items():
    fo.write(str(k) + ': ' + str(v) + '\n')
