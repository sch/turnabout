//
// |''||''|                                  '||                       ||    
//    ||                                      ||                       ||    
//    ||    '||  ||` '||''| `||''|,   '''|.   ||''|, .|''|, '||  ||` ''||''  
//    ||     ||  ||   ||     ||  ||  .|''||   ||  || ||  ||  ||  ||    ||    
//   .||.    `|..'|. .||.   .||  ||. `|..||. .||..|' `|..|'  `|..'|.   `|..' 
//                                                                           

var threeByThree = ["###",
                    "# #",+
                    "###"];


var gameElement = document.getElementById("game");

var row = "<"


var Board = function(boardString){
    var cargo = {
        height: 0,
        width: 0,
        map: [],
        tiles: {},
        moves: [],
        gravity: "south",
        prependRow: function(){
            return this.height += 1;
        },
    }
    var rows = boardString.split("\n");
    cargo.height = rows.length;
    cargo.width = rows[0].length;
    for (var y in rows.length){
        // cargo.
    }
    return cargo;
}

var WOO_GLOBAL_COUNTER = 0;
var board = document.getElementById("theTable");
var DIRECTIONS = ["SOUTH","EAST","NORTH","WEST"];
var DIRECTION_OF_GRAVITY = function(){
    return DIRECTIONS[Math.abs(WOO_GLOBAL_COUNTER) % 4];
}

var rotate = function(degrees){
    
    WOO_GLOBAL_COUNTER += (degrees >= 0 ) ? 1 : -1;
    board.style.webkitTransform = "rotate(" + WOO_GLOBAL_COUNTER * 90 + "deg)";

};

var updateMovables = function(){
    var movables = findMovables();
};

var findMovables = function(){
    return {
        balls: document.getElementsByClassName("ball"),
        blocks: document.getElementsByClassName("block")
    };
};

var look = function(direction){
    if (direction === "south"){}
    else if (direction === "west"){}
    else if (direction === "north"){}
    else if (direction === "east"){}
}

document.onkeydown = keyControls;
function keyControls(e) {
    e = e || window.event;
    if (e.keyCode=='37') rotate(-90);       //left
    else if(e.keyCode=='39') rotate(90);    //right
    updateMovables();
};
