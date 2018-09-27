This directory contains the icons for Castle Game Engine components,
to be displayed on Lazarus component palette.

Each component class TXxx should have a corresponding image txxx.png
(png format, txxx lowercase). Run "make" to generate castleicons.lrs
from all the png files.

Source icons are done in GIMP.

- For some of them (tcastle2dcontrol),
  just convert the txxx.xcf file to txxx.png, by GIMP or by "make".

- For others (the ones contained in CastleIcons.xcf), for now
  you need to export them by hand from GIMP.
  For each icon, make visible *only* the common layer (the one with tower,
  one before last), and the layer specific to given icon,
  and then export to PNG.

  TODO: maybe nice layer names, and maybe even a script to automate the export
  from XCF to all PNG files
  (see https://sourceforge.net/p/castle-engine/tickets/33/) could be useful
  some day?
