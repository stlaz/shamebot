FILES=Main.hs Listener.hs IRC.hs
OPTS=

all:
	ghc $(FILES) -o ircbot

debug:
	ghc $(FILES) -prof -fprof-auto -fprof-cafs -g -rtsopts -o ircbot-debug

debug-run: debug
	./ircbot-debug $(OPTS) +RTS -xc
