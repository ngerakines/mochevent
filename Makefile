all:
	mkdir -p ./ebin ./bin
	(cd c;$(MAKE))
	(cd src;$(MAKE))	

test: all
	prove t/*.t

clean:
	(cd src;$(MAKE) clean)
	(cd c;$(MAKE) clean)
	rm -rf *.boot *.rel *.script *.dump *.tgz ebin/*.app
