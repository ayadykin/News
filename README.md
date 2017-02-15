[![Build Status](https://travis-ci.org/ayadykin/News.svg?branch=master)](https://travis-ci.org/ayadykin/News)

# News webserver

=====

## Installation instructions

1. Make sure you have added rebar in $PATH 

1. Download dependencies `rebar get-deps`

2. compile `rebar compile`

3. run erl -pa ebin _build/default/lib/*/ebin -s news_webserver;

open your browser on http://localhost:8080/static

## API

Supported methods:

*   `GET /`: read all news

*   `POST 

*   `PUT 

*   `DELETE /<news_id>`: delete news
