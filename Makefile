CONFIG=/usr/groups/perfeng/rage/config
RAGE_DB=$(shell grep "^rage_db=" $(CONFIG) | awk -F '=' '{print $$2}')
RAGE_HOST=$(shell grep "^rage_host=" $(CONFIG) | awk -F '=' '{print $$2}')
RAGE_USER=$(shell grep "^rage_user=" $(CONFIG) | awk -F '=' '{print $$2}')
RAGE_PASS=$(shell grep "^rage_pass=" $(CONFIG) | awk -F '=' '{print $$2}')
SETTINGS=host=$(RAGE_HOST) user=$(RAGE_USER) password=$(RAGE_PASS) dbname=$(RAGE_DB)
PROGRAM=rage
RUN_CMD=OCAMLRUNPARAM='b1' ./$(PROGRAM) "$(SETTINGS)" /etc/rage_passwd
WWW_DIR=/var/www
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
	cp $(DISTRO_DIR)/* $(WWW_DIR)

log:
	sudo tail -F /var/log/apache2/error.log
