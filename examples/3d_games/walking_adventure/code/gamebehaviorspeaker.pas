{
  Copyright 2025-2025 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file LICENSE,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Something that can talk in the game (TBehaviorSpeaker). }
unit GameBehaviorSpeaker;

interface

uses CastleTransform;

type
  { Something that can talk in the game.
    Add this behavior to a TCastleTransform that represents a character
    that can speak. See https://castle-engine.io/behaviors about behaviors.
    This performs 2 functions:

    - Informs tha ViewPlay that this thing *can* speak, so we should
      show ViewTalk when clicking on it.

    - Determines the ViewTalk configuration: what the speaker says.
  }
  TBehaviorSpeaker = class(TCastleBehavior)
  public
    SpeakerName: String;
    Message: String;
    Url: String;
  end;

implementation

end.
