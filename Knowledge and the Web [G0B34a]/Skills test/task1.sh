#!/bin/bash
directory=$1  # directory location
numberOfWords=$2 # Number of words

# It can be optimized, by storing the number of times a word occurs per document. 
# And when linking documents, we can sum the words. So we do not always have to count again.

# Get cycle reference in dataset. -> Go only 1 level deep
function wordsDocRef {
	_doc=$1

	words+="${docToWords[$doc]} "
	# No reference
	if [ -z ${docRef[$_doc]} ]; then
		echo "$_doc  - No Link"
	# Found reference
	else
		echo "$_doc - ${docRef[$_doc]}"
		wordsDocRef ${docRef[$_doc]}
	fi
}

for fileLocation in $directory/*.kb; do

	declare -A docToWords
	declare -A docRef
	declare -A classDoc

# ------------ Parse data ------------
	
	echo "-------------------- file: ${fileLocation##*/} -------------------- "

	while read line
	do
		#Has_X(Y) states that a document Y has a word X in it,
	if [[ $line =~ ^"Has_"(.*)"("(.*)")" ]]; then 
	    word=${BASH_REMATCH[1]}
		wordToDoc=${BASH_REMATCH[2]}
		# Optimize and count here the words.
		docToWords[$wordToDoc]+="$word "
	
	# LinkedTo(V, W) states that document V refers to the document W
	elif  [[ $line =~ ^"LinkTo("(.*)","(.*)","(.*)")" ]] ; then
		doc=${BASH_REMATCH[2]}
		linkedToDoc=${BASH_REMATCH[3]}
		docRef[$doc]="$linkedToDoc "
	
	# Error
	else
		echo "Error predicate unknown!"
	fi

	done <"$fileLocation"
	
	if [ -f "${fileLocation%.*}.labels" ]; then
    	while read line
		do
			if [[ $line =~ ^(.*)"("(.*)")" ]] ; then
				class=${BASH_REMATCH[1]}
				doc=${BASH_REMATCH[2]}
				classDoc[$class]+="$doc "
			fi

		done <"${fileLocation%.*}.labels"
	else
    	echo "${fileLocation%.*}.labels not found!"
    	exit 1
	fi

# ------------ Process data ------------	

	for class in ${!classDoc[@]}; do
		echo -e "\n-------------------- class: $class -------------------- "

	   	words=""
	   	for doc in ${classDoc[${class}]} 
		do
			# Get cycle reference in dataset. -> Go only 1 level deep
		   	# wordsDocRef $doc

		   	words+="${docToWords[$doc]} "

		   	if [[ -n ${docRef[$doc]} ]]; then
				words+="${docToWords[${docRef[$doc]} ]} "
			fi
		done
		
# ------------ Write data ------------	

	   	# Write to file, in the format of [file name]-[document class].res
	   	# Write only to file
	  	# echo "$(echo $words | tr [:space:] '\n' | sort | uniq -c | sort -nr | head -$numberOfWords | sed -E 's/^ *[0-9]+ //g')" > "${fileLocation%.*}-${class}.res"
	  	# Write to console and file (probably there are better ways)
	  	echo -n "$(echo $words | tr [:space:] '\n' | sort | uniq -c | sort -nr | head -$numberOfWords | sed -E 's/^ *[0-9]+ //g')" | tee "${fileLocation%.*}-${class}.res"

	done
done