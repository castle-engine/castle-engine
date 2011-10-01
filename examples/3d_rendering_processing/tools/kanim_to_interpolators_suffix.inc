{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "kanim_to_interpolators_suffix.wrl" }
'# Make the animation' + LineEnding + 
'' + LineEnding + 
'DEF Timer TimeSensor { loop TRUE cycleInterval 5 }' + LineEnding + 
'ROUTE Timer.fraction_changed TO Interp.set_fraction' + LineEnding + 
'ROUTE Interp.value_changed TO $(CoordinateNodeName).point' + LineEnding + 
''
