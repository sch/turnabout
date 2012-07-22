# _ = require 'underscore'

canvas = document.getElementById('game')
ctx = canvas.getContext('2d')

ctx.fillStyle = "rgb(200,0,0)"
ctx.fillRect 10, 10, 55, 50

l1 = ["########",
      "#      #",
      "#      #",
      "#      #",
      "#r #R  #",
      "########"]

l2 = ["#########",
      "#r  #  r#",
      "##  #  ##",
      " #     # ",
      " #     #",
      "##     ##",
      "#       #",
      "#########"]
      
l2src = """ 
        #########
        #r  #  r#
        ##  #  ##
         #     # 
         #     # 
        ##     ##
        #       #
        #########
        """
      
test = 
"""
####
#  #
 ## 
"""

class Square
  constructor: (@symbol) ->
    @populate()
  
  populate: ->
    switch @symbol
      when '#' then @type = 'block'
      when 'r' then @type = 'ball'; @color = 'red'
      when 'R' then @type = 'colored block'; @color = 'red'
      else @type = 'empty'


class Board
  constructor: (@grid = []) -> @string = "hi"
  
  generate: (string) ->
    stringArray = string.split('\n')
    for row in stringArray
      @grid.push row.split('')
  console.log @grid


makeBoard = (level) ->
    # accepts a list-formatted level and returns a board object
    
    gridArray = []
    boardArray = level.split('\n')
    height = boardArray.length
    
    console.log "the board split on newlines: #{boardArray}"
    
    #console.log "START"
    for eachRow in boardArray
        # console.log "eachRow: #{eachRow} of length #{eachRow.length}"
        inner = []
        for i in [0...eachRow.length]
            # console.log "current row: #{eachRow}, char is #{eachRow[i]}, i is: #{i}"
            inner[i] = { representation: eachRow[i] }
        gridArray.push(inner)  
    #console.log "END"
    width = eachRow.length
    
    console.log "this is the board: ", gridArray
    
    # height = board.length
    # width = board[1].length
    
    board =
        gravity: 'south'
        height: height
        width: width
        grid: gridArray
        parseBoard: ->
          console.log("ASDFOAJFDOIASDJFOASIJF", each) for each in @grid
    
heyoh = makeBoard(test)
heyoh.parseBoard()

console.log heyoh


rotateBoard = (direction, board) ->
    rotateRight = ->
        gravity = board.gravity
        board.gravity = 'east' if gravity is 'south'
        board.gravity = 'north' if gravity is 'east'
        board.gravity = 'west' if gravity is 'north'
        board.gravity = 'south' if gravity is 'west'
    rotateLeft = ->
        gravity = board.gravity
        board.gravity = 'west' if gravity is 'south'
        board.gravity = 'north' if gravity is 'west'
        board.gravity = 'east' if gravity is 'north'
        board.gravity = 'south' if gravity is 'west'
    
    rotateRight() if direction is 'right'
    rotateLeft()  if direction is 'left'
        
    board

applyGravity = (board) ->
    board
    
renderBoard = (board) ->
    # returns ASCII representation of board given the gravity direction
    theList = board.grid

    rotate = (theList) -> _.zip.apply( _, theList.reverse() )
    rotateX = (theList, gravity) ->
      return theList if gravity is 'south'
      theList = rotate(theList)
      return theList if gravity is 'east'
      theList = rotate(theList)
      return theList if gravity is 'north'
      theList = rotate(theList)
      return theList if gravity is 'east'
    
    console.log "the list: ", rotateX(theList, board.gravity)
    

renderBoard heyoh
