all:
	ghc -O2 -Wall -fno-spec-constr-count -threaded --make auca

prod:
	ghc -O2 -Wall -Werror -fno-spec-constr-count -threaded --make auca

prof:
	ghc -rtsopts -prof -auto-all -O2 -v -Wall -Werror --make auca
	./auca +RTS -p

srclist:
	$(shell find -type f -regex ".*\.lhs" > srclist)

clean:
	$(RM) auca
	find -type f -iregex ".+\.\(aux\|o\|hi\|hp\|mix\|ps\|tix\|html\)" -exec rm -v {} \;
