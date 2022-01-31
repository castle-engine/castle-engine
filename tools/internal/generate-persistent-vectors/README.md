Generate Pascal code to correctly instantiate and publish instances of:

  TCastleColorRGBPersistent
  TCastleColorPersistent
  TCastleVector3Persistent
  TCastleVector4Persistent

Thanks to these classes, vectors and colors:

- Can be saved/loaded using our serialization system in CastleComponentSerialize,

- Can be saved/loaded using standard Lazarus/Delphi serialization (lfm, dfm)
  (Not yet working, a published TPersistent is not saved automatically in LFM.
  In the future, possibly we will upgrade them to TComponent,
  or we will save a JSON serialization of TCastleControl.Controls in LFM,
  or we will figure out a way to serialize a bare TPersistent in LFM.)

- Can be edited in Castle Game Engine Editor,

- Can be edited in Lazarus/Delphi Object Inspector (when used in TCastleControl).

Instantiating these classes is something we need to do often across the engine.
And it involves a lot of boilerplace code.
So it was more efficient to write a tool to generate this code.

Example output is inside:

  ../../../src/ui/opengl/auto_generated_persistent_vectors

Input of this program is inside

  persistent_vectors_input.txt
  persistent_vectors_template.inc

Compile and run it like

  castle-engine compile
  ./generate-persistent-vectors # or castle-engine run
