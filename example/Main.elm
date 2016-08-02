module Main exposing (..)

import Generator exposing (generate)
import String


json : String
json =
    "{\"swagger\":\"2.0\",\"info\":{\"version\":\"0.0.1\",\"title\":\"Omni Article Api\"},\"basePath\":\"/v3\",\"schemes\":[\"http\",\"https\"],\"consumes\":[\"application/json\"],\"produces\":[\"application/json\"],\"paths\":{\"/articles/{article_id}\":{\"get\":{\"description\":\"Get an article by id\",\"parameters\":[{\"name\":\"article_id\",\"in\":\"path\",\"description\":\"Article id\",\"required\":true,\"type\":\"string\"}],\"responses\":{\"200\":{\"description\":\"Success\",\"schema\":{\"$ref\":\"#/definitions/Article\"}},\"default\":{\"description\":\"Error\",\"schema\":{\"$ref\":\"#/definitions/ErrorResponse\"}}}}},\"/articles\":{\"get\":{\"description\":\"Get articles\",\"parameters\":[{\"name\":\"offset\",\"in\":\"query\",\"description\":\"Offset\",\"default\":0,\"type\":\"integer\"},{\"name\":\"limit\",\"in\":\"query\",\"description\":\"Limit\",\"default\":10,\"type\":\"integer\"}],\"responses\":{\"200\":{\"description\":\"Success\",\"schema\":{\"items\":{\"$ref\":\"#/definitions/Group\"}}},\"default\":{\"description\":\"Error\",\"schema\":{\"$ref\":\"#/definitions/ErrorResponse\"}}}}},\"/swagger\":{}},\"definitions\":{\"Group\":{\"items\":{\"$ref\":\"#/definitions/Article\"}},\"Article\":{\"required\":[\"id\",\"title\",\"category_id\"],\"properties\":{\"id\":{\"type\":\"string\"},\"title\":{\"type\":\"string\"},\"category_id\":{\"type\":\"string\"},\"sponsored\":{\"type\":\"boolean\"},\"rules\":{\"$ref\":\"#/definitions/Rules\"},\"nested\":{\"properties\":{\"one\":{\"type\":\"string\"},\"two\":{\"type\":\"string\"}}}}},\"Rules\":{\"type\":\"object\"},\"ErrorResponse\":{\"required\":[\"message\"],\"properties\":{\"message\":{\"type\":\"string\"}}}}}"


test : Result String (List String)
test =
    Result.map (List.map (Debug.log "result") << List.reverse << String.split "\n") <| generate json
