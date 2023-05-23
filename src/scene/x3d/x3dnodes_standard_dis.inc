{%MainUnit x3dnodes.pas}
{
  Copyright 2008-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}
  TDISEntityTypeMappingNode = class;

  TDISNetworkMode = (nmStandAlone, nmNetworkReader, nmNetworkWriter);

  { }
  TDISEntityManagerNode = class(TAbstractChildNode)
  {$I auto_generated_node_helpers/x3dnodes_disentitymanager.inc}
  end;

  TDISEntityTypeMappingNode = class(TAbstractInfoNode)
  public
    constructor Create(const AX3DName: String = ''; const ABaseUrl: String = ''); override;
  {$I auto_generated_node_helpers/x3dnodes_disentitytypemapping.inc}
  end;

  TEspduTransformNode = class(TAbstractGroupingNode)
  {$I auto_generated_node_helpers/x3dnodes_espdutransform.inc}
  end;

  TReceiverPduNode = class(TAbstractSensorNode)
  {$I auto_generated_node_helpers/x3dnodes_receiverpdu.inc}
  end;

  TSignalPduNode = class(TAbstractSensorNode)
  {$I auto_generated_node_helpers/x3dnodes_signalpdu.inc}
  end;

  TTransmitterPduNode = class(TAbstractSensorNode)
  {$I auto_generated_node_helpers/x3dnodes_transmitterpdu.inc}
  end;

{$endif read_interface}

{$ifdef read_implementation}

const
  DISNetworkModeNames: array [TDISNetworkMode] of String = (
    'standAlone',
    'networkReader',
    'networkWriter'
  );

constructor TDISEntityTypeMappingNode.Create(const AX3DName, ABaseUrl: String);
begin
  inherited;
  AddFunctionality(TUrlFunctionality.Create(Self));
end;

procedure RegisterDISNodes;
begin
  NodesManager.RegisterNodeClasses([
    TDISEntityManagerNode,
    TDISEntityTypeMappingNode,
    TEspduTransformNode,
    TReceiverPduNode,
    TSignalPduNode,
    TTransmitterPduNode
  ]);
end;

{$endif read_implementation}
