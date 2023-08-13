# Reddit 1.0

This version is an easier version to develop using docker.


## for Development

## Require

- Docker Compose

### Run

Run docker.

```sh
docker-compose up
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

```lisp
(reddit:shutdown-reddit)
```

## hint

### databsae

- database-name: `reddit`
- database-server: `db`
- username: `pgsql`
- password: `pgcwip42:`

it's defined on src/data.lisp.

### routing

it's defined on src/main.lisp at `initialize-dispatch-table`.


## for Production ...?

(help...)


## notes

Tables

```sql
reddit=> \d
          List of relations
 Schema |    Name     | Type  | Owner
--------+-------------+-------+-------
 public | alias       | table | pgsql
 public | articles    | table | pgsql
 public | articles_sn | table | pgsql
 public | clicks      | table | pgsql
 public | like_site   | table | pgsql
 public | mod_article | table | pgsql
 public | mod_user    | table | pgsql
 public | neuter      | table | pgsql
 public | options     | table | pgsql
 public | users       | table | pgsql
 public | wtf         | table | pgsql
(11 rows)

reddit=> \d alias
                    Table "public.alias"
 Column |       Type        | Collation | Nullable | Default
--------+-------------------+-----------+----------+---------
 userid | integer           |           | not null |
 name   | character varying |           | not null |
 val    | character varying |           |          |
Indexes:
    "alias_pk" PRIMARY KEY, btree (userid, name)

reddit=> \d articles
                         Table "public.articles"
  Column   |            Type             | Collation | Nullable | Default
-----------+-----------------------------+-----------+----------+---------
 id        | integer                     |           | not null |
 url       | character varying           |           |          |
 title     | character varying           |           |          |
 date      | timestamp without time zone |           |          |
 submitter | integer                     |           |          |
 pop       | integer                     |           |          |
Indexes:
    "articles_pk" PRIMARY KEY, btree (id)

reddit=> \d articles_sn
                        Table "public.articles_sn"
   Column   |            Type             | Collation | Nullable | Default
------------+-----------------------------+-----------+----------+---------
 screenname | character varying           |           |          |
 id         | integer                     |           | not null |
 url        | character varying           |           |          |
 title      | character varying           |           |          |
 date       | timestamp without time zone |           |          |
 submitter  | integer                     |           |          |
 pop        | integer                     |           |          |
Indexes:
    "articles_sn_pk" PRIMARY KEY, btree (id)

reddit=> \d clicks
                         Table "public.clicks"
 Column  |            Type             | Collation | Nullable | Default
---------+-----------------------------+-----------+----------+---------
 userid  | integer                     |           |          |
 article | integer                     |           |          |
 date    | timestamp without time zone |           |          |
 ip      | character varying           |           |          |

reddit=> \d like_site
                        Table "public.like_site"
 Column  |            Type             | Collation | Nullable | Default
---------+-----------------------------+-----------+----------+---------
 userid  | integer                     |           | not null |
 article | integer                     |           | not null |
 date    | timestamp without time zone |           |          |
 liked   | boolean                     |           |          |
Indexes:
    "like_site_pk" PRIMARY KEY, btree (userid, article)

reddit=> \d mod_article
                       Table "public.mod_article"
 Column  |            Type             | Collation | Nullable | Default
---------+-----------------------------+-----------+----------+---------
 userid  | integer                     |           | not null |
 article | integer                     |           | not null |
 date    | timestamp without time zone |           |          |
 ip      | character varying           |           |          |
 amount  | integer                     |           |          |
Indexes:
    "mod_article_pk" PRIMARY KEY, btree (userid, article)

reddit=> \d mod_user
                        Table "public.mod_user"
 Column  |            Type             | Collation | Nullable | Default
---------+-----------------------------+-----------+----------+---------
 userid  | integer                     |           | not null |
 article | integer                     |           | not null |
 target  | integer                     |           | not null |
 date    | timestamp without time zone |           |          |
 ip      | character varying           |           |          |
 amount  | integer                     |           |          |
Indexes:
    "mod_user_pk" PRIMARY KEY, btree (userid, article, target)

reddit=> \d neuter
                      Table "public.neuter"
 Column |          Type          | Collation | Nullable | Default
--------+------------------------+-----------+----------+---------
 userid | integer                |           |          |
 ip     | character varying(255) |           |          |

reddit=> \d options
               Table "public.options"
  Column  |  Type   | Collation | Nullable | Default
----------+---------+-----------+----------+---------
 userid   | integer |           | not null |
 numsites | integer |           |          |
 promoted | boolean |           |          |
 demoted  | boolean |           |          |
 visible  | boolean |           |          |
 frame    | boolean |           |          |
Indexes:
    "options_pk" PRIMARY KEY, btree (userid)

reddit=> \d users
                           Table "public.users"
   Column   |            Type             | Collation | Nullable | Default
------------+-----------------------------+-----------+----------+---------
 id         | integer                     |           | not null |
 screenname | character varying           |           |          |
 email      | character varying           |           |          |
 karma      | integer                     |           |          |
 signupdate | timestamp without time zone |           |          |
 ip         | character varying           |           |          |
Indexes:
    "users_pk" PRIMARY KEY, btree (id)

reddit=> \d wtf
                           Table "public.wtf"
 Column  |            Type             | Collation | Nullable | Default
---------+-----------------------------+-----------+----------+---------
 userid  | integer                     |           | not null |
 article | integer                     |           | not null |
 reason  | character(250)              |           |          |
 date    | timestamp without time zone |           |          |
Indexes:
    "wtf_pk" PRIMARY KEY, btree (userid, article)
```


---
- original Copyright 2018 Reddit, Inc.
- refactored Copyright 2018, 2023 tamura shingo
