Gus was created with Blender, based on the basic Blender tutorial:
  http://mediawiki.blender.org/index.php/Manual/PartI/Your_First_Animation_in_30_plus_30_Minutes_Part_I
  http://mediawiki.blender.org/index.php/Manual/PartI/Your_First_Animation_in_30_plus_30_Minutes_Part_II

I wanted to show here that it's possible to design animation in Blender using
the very comfortable "armature" + insert LocRocSize approach, and then
export it to VRML in such way that my demo_animation will be able to render
this animation.

It is possible, although I admit that it has some quirks ---
because Blender doesn't export animation in any form into VRML
(standard VRML 1.0 doesn't even let Blender to write such thing),
and also it doesn't apply modifiers (such as used subsurf modifier).
This means that the process of exporting requires you to do some work
each time:

gus_1.wrl:
  - open gus.blend
  - go to frame 1
  - go to modifiers panel (press F9 when Gus object is selected)
  - apply every modifier:
    "Make Real" of armature
    "Apply" armature
    "Apply" subsurf
  - then export to gus_1.wrl
gus_2.wrl:
  - *reopen* gus.blend first (or just undo 3 applies done above by Ctrl+Z)
  - go to frame 100
  - go to modifiers panel, apply every modifier like above
  - then export to gus_2.wrl