_ = require 'underscore'

list2 = [[1,2],
         [3,4]]

list3 = [[1,2,3],
         [4,5,6],
         [7,8,9]]


rotate = (theList) -> _.zip.apply( _, theList.reverse() )

rotateX = (theList, x) ->
  times = x % 4
  return theList if times is 0
  theList = rotate(theList)
  return theList if times is 1
  theList = rotate(theList)
  return theList if times is 2
  theList = rotate(theList)

console.log "before: ", list2
list2 = rotate(list2)
console.log "1 rotation: ", list2
list2 = rotate(list2)
console.log "2 rotations: ", list2
list2 = rotate(list2)
console.log "3 rotations: ", list2
list2 = rotate(list2)
console.log "4 rotations: ", list2

console.log "before: ", list3
list3 = rotate(list3)
console.log "1 rotation: ", list3
list3 = rotate(list3)
console.log "2 rotations: ", list3
list3 = rotate(list3)
console.log "3 rotations: ", list3
list3 = rotate(list3)
console.log "4 rotations: ", list3

list3 = [[1,2,3],
         [4,5,6],
         [7,8,9]]

newList = rotateX(list3, 5)
console.log newList
