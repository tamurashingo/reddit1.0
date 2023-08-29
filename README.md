# Reddit 1.0

This version is an easier version to develop using docker.


## for Development

## Require

- GNU Make
- Docker Compose

### Run

Run docker.

```sh
# for create docker network
make setup

make dev.up
```

Connect Swank, for example from emacs.

```sh
M-x slime-connect localhost 4005
```


### setting

```lisp
;; add reddit project directory to quicklisp project directory
(push #P"/reddit/" ql:*local-project-directories*)

;; load reddit
(ql:quickload :reddit)

;; connect database
(reddit.main::connect-database)

;; migrate database
(ql:quickload :reddit-db)
(reddit.db.migration:up)

;; disconnect database
(reddit.main::disconnect-database)
```

### start

```lisp
;; set configuration
(reddit.config:set-docker-config)
;; run
(reddit:startup-reddit)
```

open http://localhost:8000/


### stop

shutdown reddit application.

```lisp
(reddit:shutdown-reddit)
```


shutdown docker containers.

```sh
make dev.down
```

### run tests

```sh
# startup docker containers
make test.up

# run test
make test.run

# shutdown docker containers
make test.down
```


## hint


### configuration

Variables such as database user name can be overridden by environment variables.


| environment | type      | name        | value         | environment vriable        |
| ----------- | --------- | ----------- | ------------- | -------------------------- |
| docker      | database  | server      | `db`          | `REDDIT_DATABASE_SERVER`   |
|             |           | port        | `5432`        | `REDDIT_DATABASE_PORT`     |
|             |           | database    | `reddit`      | `REDDIT_DATABASE_DATABASE` |
|             |           | username    | `pgsql`       | `REDDIT_DATABASE_USERNAME` |
|             |           | password    | `pgcwip42:`   | `REDDIT_DATABASE_PASSWORD` |
|             | memcached | server      | `memcached`   | `REDDIT_MEMCACHED_SERVER`  |
|             |           | port        | `11211`       | `REDDIT_MEMCACHED_PORT`    |
|             | mail      | server      | `mail`        | `REDDIT_MAIL_SERVER`       |
|             |           | port        | `25`          | `REDDIT_MAIL_PORT`         |
|             |           | username    | `username`    | `REDDIT_MAIL_USERNAME`     |
|             |           | password    | `password`    | `REDDIT_MAIL_PASSWORD`     |
|             | logger    | logger-name | `stdout`      |                            |
| development | database  | server      | `127.0.0.1`   | `REDDIT_DATABASE_SERVER`   |
|             |           | port        | `5432`        | `REDDIT_DATABASE_PORT`     |
|             |           | database    | `reddit`      | `REDDIT_DATABASE_DATABASE` |
|             |           | username    | `pgsql`       | `REDDIT_DATABASE_USERNAME` |
|             |           | password    | `pgcwip42:`   | `REDDIT_DATABASE_PASSWORD` |
|             | memcached | server      | `127.0.0.1`   | `REDDIT_MEMCACHED_SERVER`  |
|             |           | port        | `11211`       | `REDDIT_MEMCACHED_PORT`    |
|             | mail      | server      | `127.0.0.1`   | `REDDIT_MAIL_SERVER`       |
|             |           | port        | `25`          | `REDDIT_MAIL_PORT`         |
|             |           | username    | `username`    | `REDDIT_MAIL_USERNAME`     |
|             |           | password    | `password`    | `REDDIT_MAIL_PASSWORD`     |
|             | logger    | logger-name | `stdout`      |                            |
| test        | database  | server      | `db`          | `REDDIT_DATABASE_SERVER`   |
|             |           | port        | `5432`        | `REDDIT_DATABASE_PORT`     |
|             |           | database    | `reddit_test` | `REDDIT_DATABASE_DATABASE` |
|             |           | username    | `pgsql`       | `REDDIT_DATABASE_USERNAME` |
|             |           | password    | `pgcwip42:`   | `REDDIT_DATABASE_PASSWORD` |
|             | memcached | server      | `memcached`   | `REDDIT_MEMCACHED_SERVER`  |
|             |           | port        | `11211`       | `REDDIT_MEMCACHED_PORT`    |
|             | mail      | server      | `mail`        | `REDDIT_MAIL_SERVER`       |
|             |           | port        | `25`          | `REDDIT_MAIL_PORT`         |
|             |           | username    | `username`    | `REDDIT_MAIL_USERNAME`     |
|             |           | password    | `password`    | `REDDIT_MAIL_PASSWORD`     |
|             | logger    | logger-name | `stdout`      |                            |


it's defined on src/config.lisp


### routing

it's defined on src/main.lisp at `initialize-dispatch-table`.

### mail server

To read email, open 'http://localhost:8025' .

- username: username
- password: password


To regenerate password, run this

```sh
docker-compse -f script/docker/sendmail.dev.yml run mailserver bcrypt newpassword
```

and read it.

https://github.com/mailhog/MailHog/blob/master/docs/Auth.md



## for Production ...?

(help...)


## notes

...

---
- original Copyright 2018 Reddit, Inc.
- refactored Copyright 2018, 2023 tamura shingo
