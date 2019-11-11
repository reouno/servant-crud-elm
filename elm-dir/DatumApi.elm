module DatumApi exposing(..)

import Json.Decode
import Json.Encode exposing (Value)
-- The following module comes from bartavelle/json-helpers
import Json.Helpers exposing (..)
import Dict exposing (Dict)
import Set
import Http
import String
import Url.Builder

maybeBoolToIntStr : Maybe Bool -> String
maybeBoolToIntStr mx =
  case mx of
    Nothing -> ""
    Just True -> "1"
    Just False -> "0"

type alias Datum  =
   { datumName: String
   , datumType: DatumType
   , datumContent: String
   , datumCreatedAt: Posix
   }

jsonDecDatum : Json.Decode.Decoder ( Datum )
jsonDecDatum =
   Json.Decode.succeed (\pdatumName pdatumType pdatumContent pdatumCreatedAt -> {datumName = pdatumName, datumType = pdatumType, datumContent = pdatumContent, datumCreatedAt = pdatumCreatedAt})
   |> required "datumName" (Json.Decode.string)
   |> required "datumType" (jsonDecDatumType)
   |> required "datumContent" (Json.Decode.string)
   |> required "datumCreatedAt" (jsonDecPosix)

jsonEncDatum : Datum -> Value
jsonEncDatum  val =
   Json.Encode.object
   [ ("datumName", Json.Encode.string val.datumName)
   , ("datumType", jsonEncDatumType val.datumType)
   , ("datumContent", Json.Encode.string val.datumContent)
   , ("datumCreatedAt", jsonEncPosix val.datumCreatedAt)
   ]

type DatumType  =
    TText 
    | TImage 
    | TAudio 
    | TVideo 

jsonDecDatumType : Json.Decode.Decoder ( DatumType )
jsonDecDatumType = 
    let jsonDecDictDatumType = Dict.fromList [("TText", TText), ("TImage", TImage), ("TAudio", TAudio), ("TVideo", TVideo)]
    in  decodeSumUnaries "DatumType" jsonDecDictDatumType

jsonEncDatumType : DatumType -> Value
jsonEncDatumType  val =
    case val of
        TText -> Json.Encode.string "TText"
        TImage -> Json.Encode.string "TImage"
        TAudio -> Json.Encode.string "TAudio"
        TVideo -> Json.Encode.string "TVideo"


get : (Result Http.Error  ((List (Int, Datum)))  -> msg) -> Cmd msg
get toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "GET"
            , headers =
                []
            , url =
                Url.Builder.crossOrigin ""
                    []
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson toMsg (Json.Decode.list (jsonDec(Int, Datum)))
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

post : Datum -> (Result Http.Error  (NoContent)  -> msg) -> Cmd msg
post body toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "POST"
            , headers =
                []
            , url =
                Url.Builder.crossOrigin ""
                    []
                    params
            , body =
                Http.jsonBody (jsonEncDatum body)
            , expect =
                Http.expectJson toMsg jsonDecNoContent
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

getById : Int -> (Result Http.Error  (Datum)  -> msg) -> Cmd msg
getById capture_id toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "GET"
            , headers =
                []
            , url =
                Url.Builder.crossOrigin ""
                    [ capture_id |> String.fromInt
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson toMsg jsonDecDatum
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

putById : Int -> Datum -> (Result Http.Error  (NoContent)  -> msg) -> Cmd msg
putById capture_id body toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "PUT"
            , headers =
                []
            , url =
                Url.Builder.crossOrigin ""
                    [ capture_id |> String.fromInt
                    ]
                    params
            , body =
                Http.jsonBody (jsonEncDatum body)
            , expect =
                Http.expectJson toMsg jsonDecNoContent
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

deleteById : Int -> (Result Http.Error  (NoContent)  -> msg) -> Cmd msg
deleteById capture_id toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "DELETE"
            , headers =
                []
            , url =
                Url.Builder.crossOrigin ""
                    [ capture_id |> String.fromInt
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson toMsg jsonDecNoContent
            , timeout =
                Nothing
            , tracker =
                Nothing
            }
