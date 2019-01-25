PGDB=monaddb
DBURL="postgres://monaddb:monaddb@localhost:5432/$(PGDB)?sslmode=disable"

STACK=stack
GHC_OPTIONS=--ghc-options="-Werror -Wunused-imports -Wunused-binds -Wincomplete-patterns -j"
GHC_OPTIONS_OPTIMIZED=--ghc-options="-O1"
HLINT=hlint

MIGRATE += migrate -path migrations/ -database $(DBURL)

docker:
	sudo -E docker-compose up -d
docker-stop:
	sudo -E docker-compose down
psql:
	sudo -E docker exec -it monaddb-test psql -U monaddb

migrate-up:
	$(MIGRATE) up
migrate-down:
	$(MIGRATE) down
migrate-down-hard:
	psql $(DBURL) -Atc "drop schema public cascade; create schema public;"

test: setup migrate-down-hard migrate-up
	$(STACK) test

ghci:
	$(STACK) ghci monaddb:lib monaddb:test:monaddb-test
