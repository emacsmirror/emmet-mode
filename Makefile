DST=zencoding-mode.el

all:	zencoding-mode.el

zencoding-mode.el:	src/*
	echo > $(DST)
	cat src/comments.el >> $(DST)
	cat src/init.el >> $(DST)
	cat src/xml-abbrev.el >> $(DST)
	cat src/css-abbrev.el >> $(DST)
	cat src/mode-def.el >> $(DST)

clean:
	rm zencoding-mode.el

test:
	/usr/bin/env emacs --script src/test.el

docs:
	echo docs

.PHONY: all test docs clean