run: unbounded
	./unbounded

unbounded: unbounded.adb lambda.adb lambda.ads lambda_repl.adb lambda_repl.ads lambda_parser.adb lambda_parser.ads lambda_reducer.adb lambda_reducer.ads
	gnatmake unbounded

test: unbounded
	cat REPL_Tests | ./unbounded > REPL_Tests.out
	diff REPL_Tests.out REPL_Tests.ref

clean: 
	rm -f *.o *.ali *.stdout *.stderr

save: saved

saved: REPL_Tests REPL_Tests.ref unbounded.adb lambda.adb lambda.ads lambda_repl.adb lambda_repl.ads lambda_parser.adb lambda_parser.ads lambda_reducer.adb lambda_reducer.ads
	cp REPL_Tests saved
	cp REPL_Tests.ref saved
	cp unbounded.adb saved
	cp lambda.adb saved
	cp lambda.ads saved
	cp lambda_repl.adb saved
	cp lambda_repl.ads saved
	cp lambda_parser.adb saved
	cp lambda_parser.ads saved
	cp lambda_reducer.adb saved
	cp lambda_reducer.ads saved

