module PhotoGroove exposing (main)

import Html exposing (div, h1, img, text)
import Html.Attributes exposing (..)


initialModel =
    { photos =
        [ { url = "1.jpeg" }
        , { url = "2.jpeg" }
        , { url = "3.jpeg" }
        ]
    , selectedUrl = "1.jpeg"
    }


urlPrefix =
    "http://elm-in-action.com/"


viewThumbnail selectedUrl thumb =
    img [ src (urlPrefix ++ thumb.url), classList [ ( "selected", selectedUrl == thumb.url ) ] ] []


view model =
    div [ class "content" ]
        [ h1 [] [ text "Photo Groove" ]
        , div [ id "thumbnails" ]
            (List.map
                (\photo ->
                    viewThumbnail model.selectedUrl photo
                )
                model.photos
            )
        , img [ src (urlPrefix ++ "large/" ++ model.selectedUrl), class "large" ] []
        ]


main =
    view initialModel
