port module Ports exposing (..)

import Model exposing (Author, Id, ScorePairs, StoredModel)


port setStorage : StoredModel -> Cmd msg


port scrollToTop : () -> Cmd msg


port exportJson : StoredModel -> Cmd msg


port handleNewExcerpts : StoredModel -> Cmd msg


port requestExcerptEmbeddings : List ( Id, String ) -> Cmd msg


port receiveExcerptEmbeddings : (List Id -> msg) -> Sub msg


port requestBookEmbeddings : List ( Id, List Id ) -> Cmd msg


port receiveBookEmbeddings : (() -> msg) -> Sub msg


port requestAuthorEmbeddings : List ( Id, List Id ) -> Cmd msg


port receiveAuthorEmbeddings : (() -> msg) -> Sub msg


port deleteEmbedding : ( Id, Id ) -> Cmd msg


port requestExcerptNeighbors : ( Id, Bool ) -> Cmd msg


port receiveExcerptNeighbors : (( Id, ScorePairs ) -> msg) -> Sub msg


port requestBookNeighbors : Id -> Cmd msg


port receiveBookNeighbors : (( Id, ScorePairs ) -> msg) -> Sub msg


port requestAuthorNeighbors : Author -> Cmd msg


port receiveAuthorNeighbors : (( Author, ScorePairs ) -> msg) -> Sub msg


port requestSemanticRank : ( Id, List Id ) -> Cmd msg


port receiveSemanticRank : (( Id, ScorePairs ) -> msg) -> Sub msg


port requestUnicodeNormalized : String -> Cmd msg


port receiveUnicodeNormalized : (String -> msg) -> Sub msg


port requestSemanticSearch : ( String, Float ) -> Cmd msg


port receiveSemanticSearch : (( String, ScorePairs ) -> msg) -> Sub msg


port fetchDemoEmbeddings : List Id -> Cmd msg


port syncState : (StoredModel -> msg) -> Sub msg


port initWithClear : StoredModel -> Cmd msg
