# elm-physics-2d

A simple physics engine in [Elm](https://elm-lang.org) for games and simulations.

## Roadmap

### Features
- [x] Draw polygons
- [x] Draw circles
- [x] Animate movement and rotation
- [x] Set velocity and apply forces
- [x] Friction and maximum velocity
- [x] Collision detection for circles
- [ ] Collision detection for polygons
- [x] Collision response (removal)
- [ ] Collision response (elastic)
- [x] Collision response (custom)

### Issues
1. Object's "velocity" is confused with displacement per timestep—what should it be?
1. What should units for rotational velocity be?
1. Incorrect translation between world units and screen units
1. Destructuring to check for `objectId` matches requires a pair of tuple pattern matches (clunky and brittle)
1. `World.viewSvg` should be moved out of `World` and into an example