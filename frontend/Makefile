DIST_BASE:="./dist/global"
WEBPACK:="node_modules/.bin/webpack"

.PHONY: dist watch serve install

watch:
	$(WEBPACK) \
	  --colors \
	  --verbose \
	  --progress \
	  --bail \
		--watch

dist:
	$(WEBPACK) \
	  --colors \
	  --verbose \
	  --progress \
	  --bail

serve:
	http-server dist

install:
	npm install
	bower install
