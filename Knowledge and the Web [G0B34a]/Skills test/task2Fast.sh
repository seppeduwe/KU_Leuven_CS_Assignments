#!/bin/bash
Stext=$(<$1)
i=0
molecules=$(echo "$Stext" | grep -P -o '(?<=\Molecule\()D[0-9]*(?=\))')
 while read -r line ; do
  	groupNumber=$((i%(($(echo "$molecules" | wc -l) +$2-1) / $2)))
  	echo "$(echo "$Stext" | grep "$line[^0-9]")" > "${1%.*}.groupNumber${groupNumber}"
	((i++))
done <<< "$molecules"