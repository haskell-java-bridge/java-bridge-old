build:
	cabal configure -f OSX_FRAMEWORK
	cabal build
	cabal haddock

dist: hfunc
	cabal configure
	./dist/setup/setup sdist

debug:
	rm -rf dist
	cabal install -f DEBUG

clean:
	rm -rf dist bindings *.class *.o *.hi
	rm -rf examples/bin examples/*.o examples/*.hi

hfunc:
	javac -source 1.6 -target 1.6 HFunction.java
	javac -source 1.6 -target 1.6 ReadClass.java
	java ReadClass

minimal:
	./dist/build/j2hs/j2hs -XF -Pjava.lang

se:
	./dist/build/j2hs/j2hs -XF -Pjava.io,java.lang,java.lang.annotation,java.lang.instrument,java.lang.ref,java.lang.reflect,java.math,java.net,java.nio,java.nio.channels,java.nio.channels.spi,java.nio.charset,java.nio.charset.spi,java.text,java.text.spi,java.util,java.util.concurrent,java.util.concurrent.atomic,java.util.concurrent.locks,java.util.jar,java.util.logging,java.util.prefs,java.util.regex,java.util.spi,java.util.zip

bindings:
	rm -rf bindings
	cabal build
	./dist/build/j2hs/j2hs -XF

install:
	cabal install -f OSX_FRAMEWORK

.PHONY: clean bindings hfunc dist


