#  Generate everything we know about everybody
#
C=python /home/calvins/swap/cwm1.82/cwm.py

all : gedcom-all.n3 conclusions.n3 conclusions2.n3
	touch all

gedcom-all.n3 : gedcom-facts.n3 gedcom-relations.n3
	$C gedcom-facts.n3 gedcom-relations.n3 > gedcom-all.n3

conclusions.n3 : gedcom-all.n3
	$C gedcom-all.n3 -think > conclusions.n3

conclusions2.n3 : gedcom-all.n3 gedcom-damlex.n3
	$C gedcom-all.n3 gedcom-damlex.n3 -think > conclusions2.n3

#ends
