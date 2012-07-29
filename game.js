var threeByThree = "###\n# #\n###";

var gameElement = document.getElementById("game");

var row = "<"

var Board = function(boardString){
    var cargo = {
        height: 0,
        width: 0,
        tiles: {},
        moves: [],
        gravity: "south"
    }
    var rows = boardString.split("\n");
    cargo.height = rows.length;
    cargo.width = rows[0].length;
    for (var y in rows.length){
        // gameElement.
    }
    return cargo;
}


