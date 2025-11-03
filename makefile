cranes: src/main.pl
	swipl --stand_alone=true -o cranes -c src/main.pl

run: cranes
	./cranes	