FILES=Main.hs Listener.hs IRC.hs Common.hs
OPTS=

build: $(FILES)
	ghc $^ -o ircbot

debug: $(FILES)
	ghc $^ -prof -fprof-auto -fprof-cafs -g -rtsopts -o ircbot-debug

debug-run: debug
	./ircbot-debug $(OPTS) +RTS -xc

clean:
	rm -f *.o *.hi ircbot ircbot-debug
