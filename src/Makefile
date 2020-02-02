run: main
	./main

main: main.adb lambda.adb lambda.ads
	gnat compile main lambda
	gnat bind main lambda
	gnat link main

test: main
	cat REPL_Tests | ./main

fred: fred.adb
	gnat compile fred
	gnat bind fred
	gnat link fred
