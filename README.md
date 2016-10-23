# Swagger Elm

Swagger elm generates types and decoders from [Swagger document][swagger].

_Note: This project is currently under active and initial development. Thus
everything is not finished and there's no guarantee that anything will work as
promised. However I'm happy for any testing and feedback I can get._

## Installation

1. `npm install ahultgren/swagger-elm -g`
2. Optionally install [elm format][elm-format]

## Usage (unix)

Using elm-format:

```sh
cat swagger.json | swagger-to-elm | elm-format --stdin
```

Unformatted (no guarantee it will work):

```sh
cat swagger.json | swagger-to-elm
```

## Contribution

Help and feedback would be gladly appreciated, but please just drop me a line
(an issue) before you start, to prevent wasted effort.

## Features

- [x] Basic decoding of all types (Integer, Float, String, Boolean, Object, Array, Null)
- [x] `$ref`s
- [x] Enum for strings
  - [ ] Other types (if meaningful)
- [x] Default values for Integer, Float, String, Boolean
  - [ ] Complex types (if meaningful)
- [x] Sanitization of identifiers
- [x] Unit tests
- [ ] Dicts (additionalProperties)
- [ ] Polymorphism (allOf and discriminator)
- [ ] Recursive types and decoders
- [ ] Formats? (datetime, uuid, etc)
- [ ] Encoders
- [ ] Deduplication of types and fields

## License

ISC

[swagger]: http://swagger.io
[elm-format]: https://github.com/avh4/elm-format
