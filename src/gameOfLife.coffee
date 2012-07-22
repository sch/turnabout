class GameOfLife
  gridSize:   50
  canvasSize: 600
  lineColor: '#cdcdcd'
  liveColor: '#666'
  deadColor: '#eee'
  initialLifeProbability: 0.5
  animationRate: 60
  
  constructor: (options = {}) ->
    this[key] = value for key, value of options
    @world    = @createWorld()
    do @circleOfLife
    null
  
  createWorld: ->
    @travelWorld (cell) =>
      cell.live = Math.random() < @initialLifeProbability
      cell
  
  circleOfLife: =>
    @world = @travelWorld (cell) =>
      cell = @world[cell.row][cell.col]
      @draw cell
      @resolveNextGeneration cell
    setTimeout @circleOfLife, @animationRate
  
  resolveNextGeneration: (cell) ->
    count = @countNeighbors cell
    cell = row: cell.row, col: cell.col, live: cell.live
    if cell.live or count is 3
      cell.live = 1 < count < 4
    cell
  
  countNeighbors: (cell) ->
    neighbors = 0
    for row in [-1..1]
      for col in [-1..1] when (row or col) and @isAlive cell.row + row, cell.col + col
        ++neighbors
    neighbors
  
  isAlive: (row, col) -> !!@world[row]?[col]?.live
  
  travelWorld: (callback) ->
  for row in [0...@gridSize]
    for col in [0...@gridSize]
      callback.call this, row: row, col: col
  
  draw: (cell) ->
    @context  ||= @createDrawingContext()
    @cellsize ||= @canvasSize/@gridSize
    coords = [cell.row * @cellsize, cell.col * @cellsize, @cellsize, @cellsize]
    @context.strokeStyle = @lineColor
    @context.strokeRect.apply @context, coords
    @context.fillStyle = if cell.live then @liveColor else @deadColor
    @context.fillRect.apply @context, coords
  
  createDrawingContext: ->
    canvas        = document.createElement 'canvas'
    canvas.width  = @canvasSize
    canvas.height = @canvasSize
    document.body.appendChild canvas
    canvas.getContext '2d'

window.conway = (options = {})-> new GameOfLife options
