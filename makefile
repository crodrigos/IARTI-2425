cranes: src/main.pl
	swipl --stand_alone=true -o cranes -c src/main.pl

report:
	swipl src/testCranes.pl > reports/reports.txt

run: cranes
	./cranes	