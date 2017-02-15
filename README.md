[![Build Status](https://travis-ci.org/ayadykin/News.svg?branch=master)](https://travis-ci.org/ayadykin/News)

# News webserver

=====

## Installation instructions

1. Make sure you have added rebar in $PATH 

2. Download dependencies `rebar get-deps`

3. compile `rebar compile`

4. run erl -pa ebin _build/default/lib/*/ebin -s news_webserver;

5. open your browser on http://localhost:8080/static

## API

Supported methods:

*   'GET /': read all news

*   'GET /<nws_id>': get news by id

*   'POST/' : create news

*   'PUT /<nws_id>' : update news

*   'DELETE /<news_id>' : delete news
