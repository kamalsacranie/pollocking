make: build clean

build:
	ghc -Wall -Werror -o Main Main.hs

clean-all: clean
	rm -f Main

clean:
	find . | grep -e '.*\.\(o\|hi\)$$' | xargs rm -f

