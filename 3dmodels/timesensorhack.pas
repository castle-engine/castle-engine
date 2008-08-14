unit TimeSensorHack;

interface

uses VRMLNodes;

var
  { TODO: THIS IS A VERY TEMPORARY AND UGLY HACK, done on 2008-08-14,
    and supposed to be removed when Michalis gets back from vacation
    --- around 2008-08-25. TVRMLFlatScene sets this. In the very very
    next commit, whole WorldTime and SetWorldTime mechanism will be
    moved from VRMLOpenGLRenderer to VRMLFlatScene, and then this
    all will be cleaned.

    For now, this is a quick way to get TimeSensor nodes
    (at least their X3DTimeDependentNode subset) working in common cases.
    This allows TVRMLFlatScene internals to communicate with
    VRMLOpenGLRenderer. }
  TimeSensorNodes: TVRMLNodesList;

implementation

end.
