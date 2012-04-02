SHELL=/bin/bash

TARGET=uml-schema.png

.PHONY: default
default: $(TARGET)

%.png: %.dia
	@dia $*.dia -e $*.png

.PHONY: clean
clean:
	@rm -f $(TARGET)
