chai = require 'chai'
chai.should()
expect = chai.expect

{TwoDeeArray} = require '../TwoDeeArray'

describe 'Blank 2d array', ->
  it 'starts as a 1x1 2D array', ->
    a1 = new TwoDeeArray
    a1.height.should.equal 1
    a1.width.should.equal 1

describe 'Array with parameters', ->
  it '3x5 parameter input makes a 3x5 array', ->
    a2 = new TwoDeeArray {height: 3, width: 5}
    a2.height.should.equal 3
    a2.width.should.equal 5

  it 'string is new line delimited list of properties', ->
    a2 = new TwoDeeArray {height: 3, width: 5}
    a2.toString.should.equal 'hi there'
