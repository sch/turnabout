class TwoDeeArray
  constructor: (params) ->
    @height = params?.height or 1
    @width = params?.width or 1

  addRowTop: ->
    @height++

  toString: ->
    string = ""
    for key, value in @
      string += "\n#{key}: #{value}"
    string

root = exports ? window
root.TwoDeeArray = TwoDeeArray


