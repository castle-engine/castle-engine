{ Define this to say you have full castle-engine repository "trunk"
  checkouted. This means we can access ../demo_models/, ../castle/
  and such directories from our test suite.

  Note that this will also make some tests taking *much* longer time,
  in particular the test TestOpeningAndRendering3D will then open + render
  a lot of 3D files then. }
{ $define CASTLE_ENGINE_TRUNK_AVAILABLE}
