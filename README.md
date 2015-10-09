# RaytracaH

[![Circle CI](https://circleci.com/gh/rafalnowak/RaytracaH.svg?style=svg)](https://circleci.com/gh/rafalnowak/RaytracaH) [![Build Status](https://travis-ci.org/rafalnowak/RaytracaH.svg?branch=master)](https://travis-ci.org/rafalnowak/RaytracaH)

RaytracaH is ray tracer written in Haskell using functional approach.
Current version is in very early stage and more features are to come soon.

# Features

- Ray tracing scene with simple shapes (for now only sphere and plane are implemented)
- Different types of lights on the scene, in current version there are two types: directional light and point light
- Movable camera
- Materials on primitives with diffuse color, specular and reflective factors
- Configuration readable from file in *.json format
- Output is written to *.ppm file format

More features are to come in next versions which will be released soon.

# Screenshots

![Screenshot](http://i.imgur.com/QeoX2Ab.png)
![Screenshot](http://i.imgur.com/H9zNJkg.png)

# TODO

- Optimization
- More primitive types
- Lights with colors
- Textures
- Antialiasing (with supersampling for example)
- Randomized rays reflections for more realistic effect (simulating rough surfaces)
- Transparency and light rays refraction
- Optional support for constructive solid geometry

# Licence

The RaytracaH is released under version 2.0 of the [Apache License](http://www.apache.org/licenses/LICENSE-2.0).
