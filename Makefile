LIBDIR=`erl -eval 'io:format("~s~n", [code:lib_dir()])' -s init stop -noshell`
VERSION=0.1
PKGNAME=mochevent

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

package: clean
	@mkdir $(PKGNAME)-$(VERSION)/ && cp -rf ebin priv src c t support foo.bin Makefile README.markdown $(PKGNAME)-$(VERSION)
	@COPYFILE_DISABLE=true tar zcf $(PKGNAME)-$(VERSION).tgz $(PKGNAME)-$(VERSION)
	@rm -rf $(PKGNAME)-$(VERSION)/

install: all
	mkdir -p $(prefix)/$(LIBDIR)/$(PKGNAME)-$(VERSION)/{ebin,priv}
	for i in ebin/* priv/*; do install $$i $(prefix)/$(LIBDIR)/$(PKGNAME)-$(VERSION)/$$i ; done
