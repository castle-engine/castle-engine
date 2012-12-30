{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "kanim_to_interpolators_suffix.x3dv" }
'# Make the animation' + LineEnding + 
'' + LineEnding + 
'DEF AnimTimeSensor TimeSensor { loop TRUE cycleInterval $(CycleInterval) }' + LineEnding + 
'ROUTE AnimTimeSensor.fraction_changed TO AnimInterpolator.set_fraction' + LineEnding + 
'ROUTE AnimInterpolator.value_changed TO $(CoordinateNodeName).point' + LineEnding + 
''
