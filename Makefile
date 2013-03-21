DST=zencoding-mode.el

all:	zencoding-mode.el

zencoding-mode.el: src/snippets.el src/*
	rm -f $(DST)
	touch $(DST)
	cat src/comments.el >> $(DST)
	cat src/init.el >> $(DST)
	cat src/snippets.el >> $(DST)
	cat src/html-abbrev.el >> $(DST)
	cat src/css-abbrev.el >> $(DST)
	cat src/mode-def.el >> $(DST)

src/snippets.el: conf/snippets.json
	tools/json2hash conf/snippets.json -o src/snippets.el --defvar 'zencoding-snippets'

clean:
	rm -f zencoding-mode.el src/snippets.el

test:
	/usr/bin/env emacs --script src/test.el

docs:
	echo docs

.PHONY: all test docs clean