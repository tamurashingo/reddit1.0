# Reddit 1.0

This version is an easier version to develop using docker.

## Quickstart

create docker network and base image.

```sh
make setup
```

startup swank server and db, sendmail, etc...

```sh
make dev.up
```

connect swank by using emacs

```elisp
(slime-connect "localhost" 4005)
```

in slime, add reddit project directory to quicklisp project directory

```lisp
(push #P"/reddit/" ql:*local-project-directories*)
```


load reddit

```lisp
(ql:quickload :reddit)
```

migration (require once)

```lisp
;; connect to database
(reddit.config:set-docker-config)
(reddit.main::connect-database)

;; load migration code
(ql:quickload :reddit-db)

;; migration
(reddit.db.migration:up)

;; disconnect database
(reddit.main::disconnect-database)
```

start up reddit application


```lisp
(reddit:startup-reddit)
```


open http://localhost:8000/browse

for shutting down

```lisp
(reddi:shutdown-reddit)
```


shutting down develop server

```sh
make dev.down
```


## for test

startup test server

```sh
make test.up
```

run tests

```sh
make test.run
```

shutdown test server

```sh
make tet.down
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
- refactored Copyright 2018-2025 tamura shingo
