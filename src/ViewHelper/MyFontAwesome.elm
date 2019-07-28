module ViewHelper.MyFontAwesome exposing (addUser, back, chartBar, edit, lose, next, pencil, save, second, table, tabletennis, third, trash, user, win, winner)

import FontAwesome.Icon as Icon
import FontAwesome.Solid as Solid
import Html exposing (Html, div)
import Html.Attributes exposing (class)


edit : Html msg
edit =
    div [ class "FontAwesomeBox" ] [ Icon.view Solid.edit ]


addUser : Html msg
addUser =
    div [ class "FontAwesomeBox" ] [ Icon.view Solid.userPlus ]


trash : Html msg
trash =
    div [ class "FontAwesomeBox" ] [ Icon.view Solid.trashAlt ]


table : Html msg
table =
    div [ class "FontAwesomeBox" ] [ Icon.view Solid.table ]


chartBar : Html msg
chartBar =
    div [ class "FontAwesomeBox" ] [ Icon.view Solid.chartBar ]


back : Html msg
back =
    div [ class "FontAwesomeBox" ] [ Icon.view Solid.arrowLeft ]


next : Html msg
next =
    div [ class "FontAwesomeBox" ] [ Icon.view Solid.arrowRight ]


pencil : Html msg
pencil =
    div [ class "FontAwesomeBox" ] [ Icon.view Solid.pencilAlt ]


win : Html msg
win =
    div [ class "FontAwesomeBox" ] [ Icon.view Solid.thumbsUp ]


lose : Html msg
lose =
    div [ class "FontAwesomeBox" ] [ Icon.view Solid.thumbsDown ]


tabletennis : Html msg
tabletennis =
    div [ class "FontAwesomeBox" ] [ Icon.view Solid.tableTennis ]


user : Html msg
user =
    div [ class "FontAwesomeBox" ] [ Icon.view Solid.user ]


save : Html msg
save =
    div [ class "FontAwesomeBox" ] [ Icon.view Solid.save ]


winner : Html msg
winner =
    div [ class "FontAwesomeWinner" ] [ Icon.view Solid.trophy ]


second : Html msg
second =
    div [ class "FontAwesomeSecond" ] [ Icon.view Solid.medal ]


third : Html msg
third =
    div [ class "FontAwesomeThird" ] [ Icon.view Solid.award ]
