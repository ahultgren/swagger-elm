module Generate.Utils exposing (..)

import Codegen.Utils exposing (capitalize, sanitize)


decoderName : String -> String
decoderName name =
    "decode" ++ typeName name


nestedDecoderName : String -> String -> String
nestedDecoderName parentName name =
    "decode" ++ typeName parentName ++ typeName name


encoderName : String -> String
encoderName name =
    "encode" ++ typeName name


nestedEncoderName : String -> String -> String
nestedEncoderName parentName name =
    "encode" ++ typeName parentName ++ typeName name


typeName : String -> String
typeName =
    capitalize << sanitize


nestedTypeName : String -> String -> String
nestedTypeName parentName name =
    typeName parentName ++ typeName name
