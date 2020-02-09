run: unbounded
	./unbounded

unbounded: unbounded.adb lambda.adb lambda.ads
	gnatmake unbounded

test: unbounded
	cat REPL_Tests | ./unbounded > REPL_Tests.out
	diff REPL_Tests.out REPL_Tests.ref

clean: 
	rm *.o *.ali
