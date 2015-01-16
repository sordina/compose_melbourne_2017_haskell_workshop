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
	sed 's/^\\startmode.*//;s/^\\stopmode.*//' "$i" | "$command"
	echo
	echo "</div>"
	echo "</div>"
	echo
	echo
done
