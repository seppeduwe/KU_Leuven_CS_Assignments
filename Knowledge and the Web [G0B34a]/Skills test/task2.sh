#!/bin/bash
fileLocation=$1  # file location
numberOfGroups=$2 # Number of groups

# Speed optimalisation, don't store the Molecule and Atom predicates.
# Generate dictionary from predicate Mol2atm, afterwards link with Predicate Bond

# Possible optimalisation: without any parsing of the dataset.
# read the whole file in memory, search for Molecule(moleculeName), 
# then all words with "moleculeName_*" and save to the groupfiles. 

declare -A moleculeToAtom
declare -A boundAtomsType
numberMolecules=0

# ------------ Parse data ------------
while read line
	do
		# Mol2atm(Y;W) states that atom W is a part of the molecule Y
		if [[ $line =~ ^"Mol2atm("(.*)","(.*)")" ]] ; then
			molecule=${BASH_REMATCH[1]}
			atom=${BASH_REMATCH[2]}
			moleculeToAtom[$molecule]+="$atom "
		# Bond(F;G;H) states that atoms F and G are bonded with a bond of type H.	
		elif  [[ $line =~ ^"Bond("(.*)","(.*)","(.*)")" ]] ; then
			atom1=${BASH_REMATCH[1]}
			atom2=${BASH_REMATCH[2]}
			bondOfType=${BASH_REMATCH[3]}
			boundAtomsType[$atom1-$atom2]=$bondOfType
			# Faster to search on other direction
			# boundAtomsType[$atom2-$atom1]=$bondOfType
	    # Atom(Z) states that Z represents an atom
		elif  [[ $line =~ ^"Atom("(.*)")" ]] ; then :
		# Molecule(X) stats that X represents a molecule,
		elif [[ $line =~ ^"Molecule("(.*)")" ]]; then 
			# Get number of molecules for dividing into groups.
			((numberMolecules++))
		# Error
		else
			echo "Error predicate unknown!"
		fi

done <"$fileLocation"

i=0
# ------------ Process data ------------	
for molecule in "${!moleculeToAtom[@]}"; do
	# Each group contains approximately the same number of molecules.
	groupNumber=$((i%(($numberMolecules+$numberOfGroups-1)/$numberOfGroups)))
	((i++))
# ------------ Write data ------------	
 	echo "-------------------- molecule: $molecule in group $groupNumber -------------------- " | tee -a "${fileLocation%.*}.groupNumber${groupNumber}"

 	atoms=${moleculeToAtom[$molecule]}
	for atom1 in $atoms ; do
		echo "Atom($atom1)" | tee -a "${fileLocation%.*}.groupNumber${groupNumber}"
		for atom2 in $atoms ; do
			if [[ -n ${boundAtomsType[$atom1-$atom2]} ]]; then
				echo "$atom1-$atom2 : ${boundAtomsType[$atom1-$atom2]} "| tee -a "${fileLocation%.*}.groupNumber${groupNumber}"
			fi
		done
	done
done