
# dev setup

.PHONY: dev
dev: 
	$(MAKE) setup
	$(MAKE) setup.tools
	docker-compose down
	docker-compose -f docker-compose.dev.yml up -d --remove-orphans
	$(MAKE) watch

.PHONY: setup
setup:
	stack setup
	stack build -j 1 Cabal
	stack build \
		--dependencies-only \
		--test \
		--no-run-tests

.PHONY: setup.tools
setup.tools:
	stack build \
	  intero \
	  hlint \
	  stylish-haskell

.PHONY: build
build: 
	stack build --pedantic --test --no-run-tests

.PHONY: watch
watch:
	stack build --fast --pedantic --test --file-watch --exec 'sh -c "pkill shortener-exe; stack exec shortener-exe"'
	#  --exec "sh -c "stack exec shortener-exe""
	#  --exec "sh -c "pkill shortener-exe; stack exec shortener-exe""

# prod setup

.PHONY: prod
prod: 
	docker-compose -f docker-compose.yml up