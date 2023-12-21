.PHONY: setup setup.dev setup.test dev.up dev.down test.up test.down test.run test.console


DEV_PROJECT := reddit10-dev
DEV_NW := reddit10_dev_nw


DOCKER_BASE_IMG := reddit10-base
DOCKER_DEV_IMG := reddit10-dev
DOCKER_TEST_IMG := reddit10-test


TEST_PROJECT := reddit10-test
TEST_NW := reddit10_test_nw

setup: setup.docker setup.dev setup.test

setup.docker:
	docker image build -t $(DOCKER_BASE_IMG) -f Dockerfile.base .
	docker run --rm -v $(CURDIR):/reddit/ --entrypoint qlot $(DOCKER_BASE_IMG) install

setup.dev:
	test -z "$$(docker network ls --filter name=$(DEV_NW) --format '{{ .ID }}')"
	docker network create -d bridge $(DEV_NW)


setup.test:
	test -z "$$(docker network ls --filter name=$(TEST_NW) --format '{{ .ID }}')"
	docker network create -d bridge $(TEST_NW)

dev.up:
	docker-compose -f script/docker/postgresql.dev.yml -p $(DEV_PROJECT) up -d
	docker-compose -f script/docker/memcached.dev.yml -p $(DEV_PROJECT) up -d
	docker-compose -f script/docker/sendmail.dev.yml -p $(DEV_PROJECT) up -d
	docker-compose -f script/docker/reddit.dev.yml -p $(DEV_PROJECT) up -d

dev.down:
	docker-compose -f script/docker/reddit.dev.yml -p $(DEV_PROJECT) down
	docker-compose -f script/docker/sendmail.dev.yml -p $(DEV_PROJECT) down
	docker-compose -f script/docker/memcached.dev.yml -p $(DEV_PROJECT) down
	docker-compose -f script/docker/postgresql.dev.yml -p $(DEV_PROJECT) down

test.up:
	docker-compose -f script/docker/postgresql.test.yml -p $(TEST_PROJECT) up -d
	docker-compose -f script/docker/memcached.test.yml -p $(TEST_PROJECT) up -d
	docker-compose -f script/docker/sendmail.test.yml -p $(TEST_PROJECT) up -d

test.run:
	docker image build -t reddit10-test -f script/docker/Dockerfile.test script/docker/
	docker run --network $(TEST_NW) --rm -v $(CURDIR):/reddit/ reddit10-test


test.down:
	docker-compose -f script/docker/sendmail.test.yml -p $(TEST_PROJECT) down
	docker-compose -f script/docker/memcached.test.yml -p $(TEST_PROJECT) down
	docker-compose -f script/docker/postgresql.test.yml -p $(TEST_PROJECT) down

test.console:
	docker run -it --network $(TEST_NW) --rm -v $(CURDIR):/reddit/ --entrypoint /bin/bash reddit10-test


