

run-server: src/server.pl
	swipl src/server.pl

debug-server: src/server.pl
	swipl src/server.pl -g devmode