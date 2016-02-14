# calc_angle

This is a [Clojure](http://www.clojure.org) program using a stepper robot arm to write Chinese characters. This program consists of three parts: a server, a javascript (actually in ClojureScript) client, arduino code run on a Arduino Pro Mini.
The javascript client works as a human interface. User can draw the strokes of the character and then send the strokes to the server. The server recieve the strokes and translate the strokes into angles by the magic of Math :-) . The arduino basically recieve the angles and perform the strokes after all angles are transferred.
The robot arm use stepper motors to control the base and three joints. The base gives the arm freedom to rotate in the horizontal plane. The next two joints enable arm to reach different points in the plane formed by the first arm and the second arm. The last joint is added to hold the pen vertically in the process.

## Usage
Clone this project or download as a zip bundle

    $ lein ring server

## License

Copyright © 2015 Pan Hao

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
