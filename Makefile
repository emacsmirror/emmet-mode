DST=zencoding-mode.el

all:	zencoding-mode.el

zencoding-mode.el: src/snippets.el src/preferences.el src/*
	rm -f $(DST)
	touch $(DST)
	cat src/comments.el >> $(DST)
	cat src/init.el >> $(DST)
	cat src/snippets.el >> $(DST)
	cat src/preferences.el >> $(DST)
	cat src/html-abbrev.el >> $(DST)
	cat src/css-abbrev.el >> $(DST)
	cat src/mode-def.el >> $(DST)

src/snippets.el: conf/snippets.json
	tools/json2hash conf/snippets.json -o src/snippets.el --defvar 'zencoding-snippets'

src/preferences.el: conf/preferences.json
	tools/json2hash conf/preferences.json -o src/preferences.el --defvar 'zencoding-preferences'

clean:
	rm -f zencoding-mode.el src/snippets.el src/preferences.el

test:
	/usr/bin/env emacs --script src/test.el

docs:
	echo docs

.PHONY: all test docs clean