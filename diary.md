Been a while. Wanted to jot down some notes on how to handle different forms of input for board rotation

Each "turn" in this game needs to do one thing: change the orientation of gravity to the left or right of what it currently is. This could be accomplished by left and right arrow keys, by swiping at the board, or by changing the device orientation (on a phone or tablet).

The device orientation input is pretty interesting, because you need to account for ways that the user can rotate the board while the turn is animating. You could go from portrait to landscape and then back to portrait, and you would have to get the user back into a state to await the next input. Some options:

- treat the direction they rotated to as canonical, and force them to rotate the device back into that position
- treat whatever direction they rotated to as the new direction either by:
  - tracking the movement while the blocks are falling, treating whatever "down" currently is as angle for the animation, and then snapping within the final 45° to the nearest edge (v smooth)
  - doing a 90° or 180° rotation (iOS style) to whatever edge you land on

Who knows only time will tell etc
