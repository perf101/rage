CONFIG=/usr/groups/perfeng/rage/config
PROGRAM=rage
RUN_CMD=OCAMLRUNPARAM='b1' ./$(PROGRAM) /etc/rage_passwd
WWW_DIR=/var/www/testing
CGI_SCRIPT=index.cgi
STATIC_DIR=static
DISTRO_DIR=distro
MODE=775
INSTALL=install -m $(MODE)

.PHONY: build clean distro install log
build:
	dune build --profile=release @install

clean:
	dune clean

distro: build
	rsync -avpL $(STATIC_DIR)/ _build/install/default/bin/$(PROGRAM) $(DISTRO_DIR)
	printf '#!/bin/bash\n\n$(RUN_CMD)\n' > $(DISTRO_DIR)/$(CGI_SCRIPT)
	chmod $(MODE) $(DISTRO_DIR)/$(CGI_SCRIPT)

install: distro
	cp -rv $(DISTRO_DIR)/* $(WWW_DIR)

log:
	sudo tail -F /var/log/apache2/error.log
