[![Build Status](https://travis-ci.org/ayadykin/News.svg?branch=master)](https://travis-ci.org/ayadykin/News)

# News REST API

=====
## Installation with erlang.mk

1. Download dependencies run `make deps`

2. Compile application run `make app`

3. Make release run 'make rel'

4. Run application  '_rel/news_webserver/news_webserver.cmd'

## Installation with rebar3

1. Make sure you have added rebar3 in $PATH 

2. Download dependencies `rebar3 get-deps`

3. compile `rebar3 compile`

4. run `erl -pa ebin _build/default/lib/*/ebin -s news_webserver`

5. open your browser on `http://localhost:8080/static`

## API

Supported methods:

*   `GET /` : read all news
  
    Responce : [{"id":1,"content":"\<html>\<head>\</head>\<body>\<div>\</div>\</body>\</html>"}]
  
*   `GET /:news_id` : get news by id

    Responce : [{"id":1,"content":"\<html>\<head>\</head>\<body>\<div>\</div>\</body>\</html>"}]

*   `POST/` : create news

    Request : {"content":"\<html>\<head>\</head>\<body>\<div>\</div>\</body>\</html>"}
    
    Responce : {"response":"ok"}
    
*   `PUT /:news_id` : update news

    Request : {"content":"\<html>\<head>\</head>\<body>\<div>\</div>\</body>\</html>"}

    Responce : {"response":"ok"}

*   `DELETE /:news_id` : delete news

    Responce : {"response":"ok"}
