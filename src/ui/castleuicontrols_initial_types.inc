{%MainUnit castleuicontrols.pas}
{
  Copyright 2010-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}

//type

  TBorder = CastleVectors.TBorder;

  TCastleUserInterface = class;
  TInternalChildrenControls = class;
  TCastleContainer = class;
  TCastleTheme = class;
  TCastleUserInterfaceList = class;
  TCastleView = class;
  TCastleViewList = class;

  TContainerEvent = procedure (Container: TCastleContainer);
  TContainerObjectEvent = procedure (Container: TCastleContainer) of object;
  TInputPressReleaseEvent = procedure (Container: TCastleContainer; const Event: TInputPressRelease);
  TInputMotionEvent = procedure (Container: TCastleContainer; const Event: TInputMotion);

  TUiNotifyEvent = procedure (const Sender: TCastleUserInterface) of object;
  TUiUpdateEvent = procedure (const Sender: TCastleUserInterface;
    const SecondsPassed: Single; var HandleInput: Boolean) of object;
  TUiPressReleaseEvent = procedure (const Sender: TCastleUserInterface;
    const Event: TInputPressRelease; var Handled: Boolean) of object;
  TUiMotionEvent = procedure (const Sender: TCastleUserInterface;
    const Event: TInputMotion; var Handled: Boolean) of object;

  { Possible values for TCastleContainer.UIScaling. }
  TUIScaling = (
    { Do not scale UI. }
    usNone,

    { Scale to fake that the container sizes enclose
      @link(TCastleContainer.UIReferenceWidth) and
      @link(TCastleContainer.UIReferenceHeight).
      So one size will be equal to reference size, and the other will be equal
      or larger to reference.

      Controls that look at @link(TCastleUserInterface.UIScale) will be affected by this.
      Together with anchors (see @link(TCastleUserInterface.Anchor)),
      this allows to easily design a scalable UI. }
    usEncloseReferenceSize,

    { Similar to usEncloseReferenceSize, but automatically flips the reference
      width and height to adjust to what is currently bigger (width or height),
      to adjust to mobile orientation changes (portrait vs landscape).

      For example, when UIReferenceWidth = 1600 and UIReferenceHeight = 900,
      then it will adjust to 1600x900 when device is in landscape mode (width > height),
      and to 900x1600 when device is in portrait mode (width < height).

      In effect, in contrast to usEncloseReferenceSize:

      @unorderedList(
        @item(Advantage: On mobile devices, if you allow user to switch
          between portrait and landscape,
          the UI will have the same physical size in both portrait and landscape
          modes. So it looks better on mobile.
        )

        @item(Disadvantage: Designing for this is a bit harder than
          for usEncloseReferenceSize, as you need to test both
          portrait (width smaller than height) and landscape (width bigger than height),
          as they result in different calculations.
        )
      )

      This is different from usEncloseReferenceSize only
      when both @link(TCastleContainer.UIReferenceWidth) and
      @link(TCastleContainer.UIReferenceHeight) are set to non-zero values.
      When one of them is zero, then both usEncloseReferenceSize and
      usEncloseReferenceSizeAutoOrientation behave the same,
      they just adjust scaling based on this one non-zero reference size.
    }
    usEncloseReferenceSizeAutoOrientation,

    { Scale to fake that the container sizes fit inside
      @link(TCastleContainer.UIReferenceWidth) and
      @link(TCastleContainer.UIReferenceHeight).
      So one size will be equal to reference size, and the other will be equal
      or smaller to reference.

      Controls that look at @link(TCastleUserInterface.UIScale) will be affected by this.
      Together with anchors (see @link(TCastleUserInterface.Anchor)),
      this allows to easily design a scalable UI. }
    usFitReferenceSize,

    { Scale to fake that the container sizes are smaller/larger
      by an explicit factor @link(TCastleContainer.UIExplicitScale).
      Controls that look at @link(TCastleUserInterface.UIScale)
      will be affected by this.

      Like usEncloseReferenceSize or usFitReferenceSize,
      this allows to design a scalable UI.
      In this case, the scale factor has to be calculated by your code
      (by default @link(TCastleContainer.UIExplicitScale) is 1.0 and the engine will
      not modify it automatically in any way),
      which allows customizing the scale to your requirements. }
    usExplicitScale,

    { Scale to pretend that the container size is smaller/larger
      by a factor derived from @link(TCastleContainer.Dpi).

      This allows to adjust controls to:

      @unorderedList(
        @item(The physical size.
          @link(TCastleContainer.Dpi) on iOS and Android really
          reflects physical size.
        )
        @item(Or at least to follow user preferred UI scaling.
          @link(TCastleContainer.Dpi) on desktops is often more like "configurable
          way to scale UI" than derived from actual monitor physical size.
        )
      )

      This is the same scaling done by normal desktop applications,
      e.g. using Lazarus LCL. Using this scaling is best if your user interface
      using CGE should match (the size of) the user interface done by
      Lazarus LCL or native applications.
    }
    usDpiScale
  );

  { Things that can cause @link(TCastleUserInterface.VisibleChange) notification. }
  TCastleUserInterfaceChange = (
    { The look of this control changed.
      This concerns all the things that affect what @link(TCastleUserInterface.Render) does.

      Note that changing chRectangle implies that the look changed too.
      So when chRectangle is in Changes, you should always behave
      like chRender is also in Changes, regardless if it's there or not. }
    chRender,

    { The rectangle (size or position) of the control changed.
      This concerns all the things that affect size
      or our position inside parent (anchors).

      Note that this is not (necessarily) called when the screen position changed
      just because the parent screen position changed.
      We only notify when the size or position changed with respect to the parent.

      Note that changing chRectangle implies that the look changed too.
      So when chRectangle is in Changes, you should always behave
      like chRender is also in Changes, regardless if it's there or not. }
    chRectangle,

    chCursor,

    chExists,

    { A (direct) child control was added or removed. }
    chChildren,

    { A (direct) child control @link(TCastleUserInterface.Exists) value changed. }
    chChildrenExists
  );
  TCastleUserInterfaceChanges = set of TCastleUserInterfaceChange;
  TCastleUserInterfaceChangeEvent = procedure(const Sender: TCastleUserInterface;
    const Changes: TCastleUserInterfaceChanges; const ChangeInitiatedByChildren: boolean)
    of object;

  { Position for relative layout of one control in respect to another.
    @deprecated Deprecated, rather use cleaner
    THorizontalPosition and TVerticalPosition.
  }
  TPositionRelative = (
    prLow,
    prMiddle,
    prHigh
  ) deprecated;

{$endif read_interface}

{$ifdef read_implementation}


{$endif read_implementation}
