#!/bin/sh

command="$1"
shift

for i in "$@"
do
	echo
	echo
	echo "<div class='chapter'>"
	echo "<div class='content'>"
	echo
	sed 's/^\\startmode.*//;s/^\\stopmode.*//' "$i" | ./resources/scripts/cache.sh "$command"
	echo
	echo "</div>"
	echo "</div>"
	echo
	echo
done
