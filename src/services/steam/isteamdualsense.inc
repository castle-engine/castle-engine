{ H2Pas converted isteamdualsense.h for completness
}

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}

{ SIE CONFIDENTIAL
 * $PSLibId$
 * Copyright (C) 2019 Sony Interactive Entertainment Inc.
 * All Rights Reserved.
  }

  const
    SCE_PAD_TRIGGER_EFFECT_TRIGGER_MASK_L2 = $01;
    SCE_PAD_TRIGGER_EFFECT_TRIGGER_MASK_R2 = $02;
    SCE_PAD_TRIGGER_EFFECT_PARAM_INDEX_FOR_L2 = 0;
    SCE_PAD_TRIGGER_EFFECT_PARAM_INDEX_FOR_R2 = 1;
    SCE_PAD_TRIGGER_EFFECT_TRIGGER_NUM = 2;
{ Definition of control point num  }
    SCE_PAD_TRIGGER_EFFECT_CONTROL_POINT_NUM = 10;

  type
  uint8_t = UInt8;

    ScePadTriggerEffectMode = (SCE_PAD_TRIGGER_EFFECT_MODE_OFF,SCE_PAD_TRIGGER_EFFECT_MODE_FEEDBACK,
      SCE_PAD_TRIGGER_EFFECT_MODE_WEAPON,SCE_PAD_TRIGGER_EFFECT_MODE_VIBRATION,
      SCE_PAD_TRIGGER_EFFECT_MODE_MULTIPLE_POSITION_FEEDBACK,
      SCE_PAD_TRIGGER_EFFECT_MODE_SLOPE_FEEDBACK,
      SCE_PAD_TRIGGER_EFFECT_MODE_MULTIPLE_POSITION_VIBRATION
      );
{*
 *E  
 *  @brief parameter for setting the trigger effect to off mode.
 *         Off Mode: Stop trigger effect.
 * }

    ScePadTriggerEffectOffParam = record
        padding : array[0..47] of uint8_t;
      end;
{*
 *E  
 *  @brief parameter for setting the trigger effect to Feedback mode.
 *         Feedback Mode: The motor arm pushes back trigger.
 *                        Trigger obtains stiffness at specified position.
 * }
{E position where the strength of target trigger start changing(0~9).  }
{E strength that the motor arm pushes back target trigger(0~8 (0: Same as Off mode)).  }

    ScePadTriggerEffectFeedbackParam = record
        position : uint8_t;
        strength : uint8_t;
        padding : array[0..45] of uint8_t;
      end;
{*
 *E  
 *  @brief parameter for setting the trigger effect to Weapon mode.
 *         Weapon Mode: Emulate weapon like gun trigger.
 * }
{E position where the stiffness of trigger start changing(2~7).  }
{E position where the stiffness of trigger finish changing(startPosition+1~8).  }
{E strength of gun trigger(0~8 (0: Same as Off mode)).  }

    ScePadTriggerEffectWeaponParam = record
        startPosition : uint8_t;
        endPosition : uint8_t;
        strength : uint8_t;
        padding : array[0..44] of uint8_t;
      end;
{*
 *E  
 *  @brief parameter for setting the trigger effect to Vibration mode.
 *         Vibration Mode: Vibrates motor arm around specified position.
 * }
{E position where the motor arm start vibrating(0~9).  }
{E vibration amplitude(0~8 (0: Same as Off mode)).  }
{E vibration frequency(0~255[Hz] (0: Same as Off mode)).  }

    ScePadTriggerEffectVibrationParam = record
        position : uint8_t;
        amplitude : uint8_t;
        frequency : uint8_t;
        padding : array[0..44] of uint8_t;
      end;
{*
 *E  
 *  @brief parameter for setting the trigger effect to ScePadTriggerEffectMultiplePositionFeedbackParam mode.
 *         Multi Position Feedback Mode: The motor arm pushes back trigger.
 *                                       Trigger obtains specified stiffness at each control point.
 * }
{E strength that the motor arm pushes back target trigger at position(0~8 (0: Same as Off mode)).
																 *  strength[0] means strength of motor arm at position0.
																 *  strength[1] means strength of motor arm at position1.
																 *  ...
																 *  }

    ScePadTriggerEffectMultiplePositionFeedbackParam = record
        strength : array[0..(SCE_PAD_TRIGGER_EFFECT_CONTROL_POINT_NUM)-1] of uint8_t;
        padding : array[0..37] of uint8_t;
      end;
{*
 *E  
 *  @brief parameter for setting the trigger effect to Feedback3 mode.
 *         Slope Feedback Mode: The motor arm pushes back trigger between two spedified control points.
 *                              Stiffness of the trigger is changing depending on the set place.
 * }
{E position where the strength of target trigger start changing(0~endPosition).  }
{E position where the strength of target trigger finish changing(startPosition+1~9).  }
{E strength when trigger's position is startPosition(1~8)  }
{E strength when trigger's position is endPosition(1~8)  }

    ScePadTriggerEffectSlopeFeedbackParam = record
        startPosition : uint8_t;
        endPosition : uint8_t;
        startStrength : uint8_t;
        endStrength : uint8_t;
        padding : array[0..43] of uint8_t;
      end;
{*
 *E  
 *  @brief parameter for setting the trigger effect to Vibration2 mode.
 *         Multi Position Vibration Mode: Vibrates motor arm around specified control point.
 *                                        Trigger vibrates specified amplitude at each control point.
 * }
{E vibration frequency(0~255 (0: Same as Off mode))  }
{E vibration amplitude at position(0~8 (0: Same as Off mode)).
																	 *  amplitude[0] means amplitude of vibration at position0.
																	 *  amplitude[1] means amplitude of vibration at position1.
																	 *  ...
																	 *  }

    ScePadTriggerEffectMultiplePositionVibrationParam = record
        frequency : uint8_t;
        amplitude : array[0..(SCE_PAD_TRIGGER_EFFECT_CONTROL_POINT_NUM)-1] of uint8_t;
        padding : array[0..36] of uint8_t;
      end;
{*
 *E  
 *  @brief parameter for setting the trigger effect mode.
 * }

    ScePadTriggerEffectCommandData = record
        case longint of
          0 : ( offParam : ScePadTriggerEffectOffParam );
          1 : ( feedbackParam : ScePadTriggerEffectFeedbackParam );
          2 : ( weaponParam : ScePadTriggerEffectWeaponParam );
          3 : ( vibrationParam : ScePadTriggerEffectVibrationParam );
          4 : ( multiplePositionFeedbackParam : ScePadTriggerEffectMultiplePositionFeedbackParam );
          5 : ( slopeFeedbackParam : ScePadTriggerEffectSlopeFeedbackParam );
          6 : ( multiplePositionVibrationParam : ScePadTriggerEffectMultiplePositionVibrationParam );
        end;
{*
 *E  
 *  @brief parameter for setting the trigger effect.
 * }

    ScePadTriggerEffectCommand = record
        mode : ScePadTriggerEffectMode;
        padding : array[0..3] of uint8_t;
        commandData : ScePadTriggerEffectCommandData;
      end;
{*
 *E
 *  @brief parameter for the scePadSetTriggerEffect function.
 * }
{E Set trigger mask to activate trigger effect commands.
								 *  SCE_PAD_TRIGGER_EFFECT_TRIGGER_MASK_L2 : 0x01
								 *  SCE_PAD_TRIGGER_EFFECT_TRIGGER_MASK_R2 : 0x02
								 *  }
{E command[SCE_PAD_TRIGGER_EFFECT_PARAM_INDEX_FOR_L2] is for L2 trigger setting
																			 *  and param[SCE_PAD_TRIGGER_EFFECT_PARAM_INDEX_FOR_R2] is for R2 trgger setting.
																			 *  }

    ScePadTriggerEffectParam = record
        triggerMask : uint8_t;
        padding : array[0..6] of uint8_t;
        command : array[0..(SCE_PAD_TRIGGER_EFFECT_TRIGGER_NUM)-1] of ScePadTriggerEffectCommand;
      end;
	PScePadTriggerEffectParam = ^ScePadTriggerEffectParam;
{ _SCE_PAD_TRIGGER_EFFECT_H  }

  PEInputActionOrigin = ^EInputActionOrigin;
  EInputActionOrigin = (
	k_EInputActionOrigin_None,k_EInputActionOrigin_SteamController_A,
      k_EInputActionOrigin_SteamController_B,
      k_EInputActionOrigin_SteamController_X,
      k_EInputActionOrigin_SteamController_Y,
      k_EInputActionOrigin_SteamController_LeftBumper,
      k_EInputActionOrigin_SteamController_RightBumper,
      k_EInputActionOrigin_SteamController_LeftGrip,
      k_EInputActionOrigin_SteamController_RightGrip,
      k_EInputActionOrigin_SteamController_Start,
      k_EInputActionOrigin_SteamController_Back,
      k_EInputActionOrigin_SteamController_LeftPad_Touch,
      k_EInputActionOrigin_SteamController_LeftPad_Swipe,
      k_EInputActionOrigin_SteamController_LeftPad_Click,
      k_EInputActionOrigin_SteamController_LeftPad_DPadNorth,
      k_EInputActionOrigin_SteamController_LeftPad_DPadSouth,
      k_EInputActionOrigin_SteamController_LeftPad_DPadWest,
      k_EInputActionOrigin_SteamController_LeftPad_DPadEast,
      k_EInputActionOrigin_SteamController_RightPad_Touch,
      k_EInputActionOrigin_SteamController_RightPad_Swipe,
      k_EInputActionOrigin_SteamController_RightPad_Click,
      k_EInputActionOrigin_SteamController_RightPad_DPadNorth,
      k_EInputActionOrigin_SteamController_RightPad_DPadSouth,
      k_EInputActionOrigin_SteamController_RightPad_DPadWest,
      k_EInputActionOrigin_SteamController_RightPad_DPadEast,
      k_EInputActionOrigin_SteamController_LeftTrigger_Pull,
      k_EInputActionOrigin_SteamController_LeftTrigger_Click,
      k_EInputActionOrigin_SteamController_RightTrigger_Pull,
      k_EInputActionOrigin_SteamController_RightTrigger_Click,
      k_EInputActionOrigin_SteamController_LeftStick_Move,
      k_EInputActionOrigin_SteamController_LeftStick_Click,
      k_EInputActionOrigin_SteamController_LeftStick_DPadNorth,
      k_EInputActionOrigin_SteamController_LeftStick_DPadSouth,
      k_EInputActionOrigin_SteamController_LeftStick_DPadWest,
      k_EInputActionOrigin_SteamController_LeftStick_DPadEast,
      k_EInputActionOrigin_SteamController_Gyro_Move,
      k_EInputActionOrigin_SteamController_Gyro_Pitch,
      k_EInputActionOrigin_SteamController_Gyro_Yaw,
      k_EInputActionOrigin_SteamController_Gyro_Roll,
      k_EInputActionOrigin_SteamController_Reserved0,
      k_EInputActionOrigin_SteamController_Reserved1,
      k_EInputActionOrigin_SteamController_Reserved2,
      k_EInputActionOrigin_SteamController_Reserved3,
      k_EInputActionOrigin_SteamController_Reserved4,
      k_EInputActionOrigin_SteamController_Reserved5,
      k_EInputActionOrigin_SteamController_Reserved6,
      k_EInputActionOrigin_SteamController_Reserved7,
      k_EInputActionOrigin_SteamController_Reserved8,
      k_EInputActionOrigin_SteamController_Reserved9,
      k_EInputActionOrigin_SteamController_Reserved10,
      k_EInputActionOrigin_PS4_X,k_EInputActionOrigin_PS4_Circle,
      k_EInputActionOrigin_PS4_Triangle,k_EInputActionOrigin_PS4_Square,
      k_EInputActionOrigin_PS4_LeftBumper,
      k_EInputActionOrigin_PS4_RightBumper,
      k_EInputActionOrigin_PS4_Options,k_EInputActionOrigin_PS4_Share,
      k_EInputActionOrigin_PS4_LeftPad_Touch,
      k_EInputActionOrigin_PS4_LeftPad_Swipe,
      k_EInputActionOrigin_PS4_LeftPad_Click,
      k_EInputActionOrigin_PS4_LeftPad_DPadNorth,
      k_EInputActionOrigin_PS4_LeftPad_DPadSouth,
      k_EInputActionOrigin_PS4_LeftPad_DPadWest,
      k_EInputActionOrigin_PS4_LeftPad_DPadEast,
      k_EInputActionOrigin_PS4_RightPad_Touch,
      k_EInputActionOrigin_PS4_RightPad_Swipe,
      k_EInputActionOrigin_PS4_RightPad_Click,
      k_EInputActionOrigin_PS4_RightPad_DPadNorth,
      k_EInputActionOrigin_PS4_RightPad_DPadSouth,
      k_EInputActionOrigin_PS4_RightPad_DPadWest,
      k_EInputActionOrigin_PS4_RightPad_DPadEast,
      k_EInputActionOrigin_PS4_CenterPad_Touch,
      k_EInputActionOrigin_PS4_CenterPad_Swipe,
      k_EInputActionOrigin_PS4_CenterPad_Click,
      k_EInputActionOrigin_PS4_CenterPad_DPadNorth,
      k_EInputActionOrigin_PS4_CenterPad_DPadSouth,
      k_EInputActionOrigin_PS4_CenterPad_DPadWest,
      k_EInputActionOrigin_PS4_CenterPad_DPadEast,
      k_EInputActionOrigin_PS4_LeftTrigger_Pull,
      k_EInputActionOrigin_PS4_LeftTrigger_Click,
      k_EInputActionOrigin_PS4_RightTrigger_Pull,
      k_EInputActionOrigin_PS4_RightTrigger_Click,
      k_EInputActionOrigin_PS4_LeftStick_Move,
      k_EInputActionOrigin_PS4_LeftStick_Click,
      k_EInputActionOrigin_PS4_LeftStick_DPadNorth,
      k_EInputActionOrigin_PS4_LeftStick_DPadSouth,
      k_EInputActionOrigin_PS4_LeftStick_DPadWest,
      k_EInputActionOrigin_PS4_LeftStick_DPadEast,
      k_EInputActionOrigin_PS4_RightStick_Move,
      k_EInputActionOrigin_PS4_RightStick_Click,
      k_EInputActionOrigin_PS4_RightStick_DPadNorth,
      k_EInputActionOrigin_PS4_RightStick_DPadSouth,
      k_EInputActionOrigin_PS4_RightStick_DPadWest,
      k_EInputActionOrigin_PS4_RightStick_DPadEast,
      k_EInputActionOrigin_PS4_DPad_North,
      k_EInputActionOrigin_PS4_DPad_South,
      k_EInputActionOrigin_PS4_DPad_West,k_EInputActionOrigin_PS4_DPad_East,
      k_EInputActionOrigin_PS4_Gyro_Move,k_EInputActionOrigin_PS4_Gyro_Pitch,
      k_EInputActionOrigin_PS4_Gyro_Yaw,k_EInputActionOrigin_PS4_Gyro_Roll,
      k_EInputActionOrigin_PS4_DPad_Move,k_EInputActionOrigin_PS4_Reserved1,
      k_EInputActionOrigin_PS4_Reserved2,k_EInputActionOrigin_PS4_Reserved3,
      k_EInputActionOrigin_PS4_Reserved4,k_EInputActionOrigin_PS4_Reserved5,
      k_EInputActionOrigin_PS4_Reserved6,k_EInputActionOrigin_PS4_Reserved7,
      k_EInputActionOrigin_PS4_Reserved8,k_EInputActionOrigin_PS4_Reserved9,
      k_EInputActionOrigin_PS4_Reserved10,
      k_EInputActionOrigin_XBoxOne_A,k_EInputActionOrigin_XBoxOne_B,
      k_EInputActionOrigin_XBoxOne_X,k_EInputActionOrigin_XBoxOne_Y,
      k_EInputActionOrigin_XBoxOne_LeftBumper,
      k_EInputActionOrigin_XBoxOne_RightBumper,
      k_EInputActionOrigin_XBoxOne_Menu,k_EInputActionOrigin_XBoxOne_View,
      k_EInputActionOrigin_XBoxOne_LeftTrigger_Pull,
      k_EInputActionOrigin_XBoxOne_LeftTrigger_Click,
      k_EInputActionOrigin_XBoxOne_RightTrigger_Pull,
      k_EInputActionOrigin_XBoxOne_RightTrigger_Click,
      k_EInputActionOrigin_XBoxOne_LeftStick_Move,
      k_EInputActionOrigin_XBoxOne_LeftStick_Click,
      k_EInputActionOrigin_XBoxOne_LeftStick_DPadNorth,
      k_EInputActionOrigin_XBoxOne_LeftStick_DPadSouth,
      k_EInputActionOrigin_XBoxOne_LeftStick_DPadWest,
      k_EInputActionOrigin_XBoxOne_LeftStick_DPadEast,
      k_EInputActionOrigin_XBoxOne_RightStick_Move,
      k_EInputActionOrigin_XBoxOne_RightStick_Click,
      k_EInputActionOrigin_XBoxOne_RightStick_DPadNorth,
      k_EInputActionOrigin_XBoxOne_RightStick_DPadSouth,
      k_EInputActionOrigin_XBoxOne_RightStick_DPadWest,
      k_EInputActionOrigin_XBoxOne_RightStick_DPadEast,
      k_EInputActionOrigin_XBoxOne_DPad_North,
      k_EInputActionOrigin_XBoxOne_DPad_South,
      k_EInputActionOrigin_XBoxOne_DPad_West,
      k_EInputActionOrigin_XBoxOne_DPad_East,
      k_EInputActionOrigin_XBoxOne_DPad_Move,
      k_EInputActionOrigin_XBoxOne_LeftGrip_Lower,
      k_EInputActionOrigin_XBoxOne_LeftGrip_Upper,
      k_EInputActionOrigin_XBoxOne_RightGrip_Lower,
      k_EInputActionOrigin_XBoxOne_RightGrip_Upper,
      k_EInputActionOrigin_XBoxOne_Share,k_EInputActionOrigin_XBoxOne_Reserved6,
      k_EInputActionOrigin_XBoxOne_Reserved7,
      k_EInputActionOrigin_XBoxOne_Reserved8,
      k_EInputActionOrigin_XBoxOne_Reserved9,
      k_EInputActionOrigin_XBoxOne_Reserved10,
      k_EInputActionOrigin_XBox360_A,k_EInputActionOrigin_XBox360_B,
      k_EInputActionOrigin_XBox360_X,k_EInputActionOrigin_XBox360_Y,
      k_EInputActionOrigin_XBox360_LeftBumper,
      k_EInputActionOrigin_XBox360_RightBumper,
      k_EInputActionOrigin_XBox360_Start,k_EInputActionOrigin_XBox360_Back,
      k_EInputActionOrigin_XBox360_LeftTrigger_Pull,
      k_EInputActionOrigin_XBox360_LeftTrigger_Click,
      k_EInputActionOrigin_XBox360_RightTrigger_Pull,
      k_EInputActionOrigin_XBox360_RightTrigger_Click,
      k_EInputActionOrigin_XBox360_LeftStick_Move,
      k_EInputActionOrigin_XBox360_LeftStick_Click,
      k_EInputActionOrigin_XBox360_LeftStick_DPadNorth,
      k_EInputActionOrigin_XBox360_LeftStick_DPadSouth,
      k_EInputActionOrigin_XBox360_LeftStick_DPadWest,
      k_EInputActionOrigin_XBox360_LeftStick_DPadEast,
      k_EInputActionOrigin_XBox360_RightStick_Move,
      k_EInputActionOrigin_XBox360_RightStick_Click,
      k_EInputActionOrigin_XBox360_RightStick_DPadNorth,
      k_EInputActionOrigin_XBox360_RightStick_DPadSouth,
      k_EInputActionOrigin_XBox360_RightStick_DPadWest,
      k_EInputActionOrigin_XBox360_RightStick_DPadEast,
      k_EInputActionOrigin_XBox360_DPad_North,
      k_EInputActionOrigin_XBox360_DPad_South,
      k_EInputActionOrigin_XBox360_DPad_West,
      k_EInputActionOrigin_XBox360_DPad_East,
      k_EInputActionOrigin_XBox360_DPad_Move,
      k_EInputActionOrigin_XBox360_Reserved1,
      k_EInputActionOrigin_XBox360_Reserved2,
      k_EInputActionOrigin_XBox360_Reserved3,
      k_EInputActionOrigin_XBox360_Reserved4,
      k_EInputActionOrigin_XBox360_Reserved5,
      k_EInputActionOrigin_XBox360_Reserved6,
      k_EInputActionOrigin_XBox360_Reserved7,
      k_EInputActionOrigin_XBox360_Reserved8,
      k_EInputActionOrigin_XBox360_Reserved9,
      k_EInputActionOrigin_XBox360_Reserved10,
      k_EInputActionOrigin_Switch_A,k_EInputActionOrigin_Switch_B,
      k_EInputActionOrigin_Switch_X,k_EInputActionOrigin_Switch_Y,
      k_EInputActionOrigin_Switch_LeftBumper,
      k_EInputActionOrigin_Switch_RightBumper,
      k_EInputActionOrigin_Switch_Plus,k_EInputActionOrigin_Switch_Minus,
      k_EInputActionOrigin_Switch_Capture,
      k_EInputActionOrigin_Switch_LeftTrigger_Pull,
      k_EInputActionOrigin_Switch_LeftTrigger_Click,
      k_EInputActionOrigin_Switch_RightTrigger_Pull,
      k_EInputActionOrigin_Switch_RightTrigger_Click,
      k_EInputActionOrigin_Switch_LeftStick_Move,
      k_EInputActionOrigin_Switch_LeftStick_Click,
      k_EInputActionOrigin_Switch_LeftStick_DPadNorth,
      k_EInputActionOrigin_Switch_LeftStick_DPadSouth,
      k_EInputActionOrigin_Switch_LeftStick_DPadWest,
      k_EInputActionOrigin_Switch_LeftStick_DPadEast,
      k_EInputActionOrigin_Switch_RightStick_Move,
      k_EInputActionOrigin_Switch_RightStick_Click,
      k_EInputActionOrigin_Switch_RightStick_DPadNorth,
      k_EInputActionOrigin_Switch_RightStick_DPadSouth,
      k_EInputActionOrigin_Switch_RightStick_DPadWest,
      k_EInputActionOrigin_Switch_RightStick_DPadEast,
      k_EInputActionOrigin_Switch_DPad_North,
      k_EInputActionOrigin_Switch_DPad_South,
      k_EInputActionOrigin_Switch_DPad_West,
      k_EInputActionOrigin_Switch_DPad_East,
      k_EInputActionOrigin_Switch_ProGyro_Move,
      k_EInputActionOrigin_Switch_ProGyro_Pitch,
      k_EInputActionOrigin_Switch_ProGyro_Yaw,
      k_EInputActionOrigin_Switch_ProGyro_Roll,
      k_EInputActionOrigin_Switch_DPad_Move,
      k_EInputActionOrigin_Switch_Reserved1,
      k_EInputActionOrigin_Switch_Reserved2,
      k_EInputActionOrigin_Switch_Reserved3,
      k_EInputActionOrigin_Switch_Reserved4,
      k_EInputActionOrigin_Switch_Reserved5,
      k_EInputActionOrigin_Switch_Reserved6,
      k_EInputActionOrigin_Switch_Reserved7,
      k_EInputActionOrigin_Switch_Reserved8,
      k_EInputActionOrigin_Switch_Reserved9,
      k_EInputActionOrigin_Switch_Reserved10,
      k_EInputActionOrigin_Switch_RightGyro_Move,
      k_EInputActionOrigin_Switch_RightGyro_Pitch,
      k_EInputActionOrigin_Switch_RightGyro_Yaw,
      k_EInputActionOrigin_Switch_RightGyro_Roll,
      k_EInputActionOrigin_Switch_LeftGyro_Move,
      k_EInputActionOrigin_Switch_LeftGyro_Pitch,
      k_EInputActionOrigin_Switch_LeftGyro_Yaw,
      k_EInputActionOrigin_Switch_LeftGyro_Roll,
      k_EInputActionOrigin_Switch_LeftGrip_Lower,
      k_EInputActionOrigin_Switch_LeftGrip_Upper,
      k_EInputActionOrigin_Switch_RightGrip_Lower,
      k_EInputActionOrigin_Switch_RightGrip_Upper,
      k_EInputActionOrigin_Switch_JoyConButton_N,
      k_EInputActionOrigin_Switch_JoyConButton_E,
      k_EInputActionOrigin_Switch_JoyConButton_S,
      k_EInputActionOrigin_Switch_JoyConButton_W,
      k_EInputActionOrigin_Switch_Reserved15,
      k_EInputActionOrigin_Switch_Reserved16,
      k_EInputActionOrigin_Switch_Reserved17,
      k_EInputActionOrigin_Switch_Reserved18,
      k_EInputActionOrigin_Switch_Reserved19,
      k_EInputActionOrigin_Switch_Reserved20,
      k_EInputActionOrigin_PS5_X,k_EInputActionOrigin_PS5_Circle,
      k_EInputActionOrigin_PS5_Triangle,k_EInputActionOrigin_PS5_Square,
      k_EInputActionOrigin_PS5_LeftBumper,
      k_EInputActionOrigin_PS5_RightBumper,
      k_EInputActionOrigin_PS5_Option,k_EInputActionOrigin_PS5_Create,
      k_EInputActionOrigin_PS5_Mute,k_EInputActionOrigin_PS5_LeftPad_Touch,
      k_EInputActionOrigin_PS5_LeftPad_Swipe,
      k_EInputActionOrigin_PS5_LeftPad_Click,
      k_EInputActionOrigin_PS5_LeftPad_DPadNorth,
      k_EInputActionOrigin_PS5_LeftPad_DPadSouth,
      k_EInputActionOrigin_PS5_LeftPad_DPadWest,
      k_EInputActionOrigin_PS5_LeftPad_DPadEast,
      k_EInputActionOrigin_PS5_RightPad_Touch,
      k_EInputActionOrigin_PS5_RightPad_Swipe,
      k_EInputActionOrigin_PS5_RightPad_Click,
      k_EInputActionOrigin_PS5_RightPad_DPadNorth,
      k_EInputActionOrigin_PS5_RightPad_DPadSouth,
      k_EInputActionOrigin_PS5_RightPad_DPadWest,
      k_EInputActionOrigin_PS5_RightPad_DPadEast,
      k_EInputActionOrigin_PS5_CenterPad_Touch,
      k_EInputActionOrigin_PS5_CenterPad_Swipe,
      k_EInputActionOrigin_PS5_CenterPad_Click,
      k_EInputActionOrigin_PS5_CenterPad_DPadNorth,
      k_EInputActionOrigin_PS5_CenterPad_DPadSouth,
      k_EInputActionOrigin_PS5_CenterPad_DPadWest,
      k_EInputActionOrigin_PS5_CenterPad_DPadEast,
      k_EInputActionOrigin_PS5_LeftTrigger_Pull,
      k_EInputActionOrigin_PS5_LeftTrigger_Click,
      k_EInputActionOrigin_PS5_RightTrigger_Pull,
      k_EInputActionOrigin_PS5_RightTrigger_Click,
      k_EInputActionOrigin_PS5_LeftStick_Move,
      k_EInputActionOrigin_PS5_LeftStick_Click,
      k_EInputActionOrigin_PS5_LeftStick_DPadNorth,
      k_EInputActionOrigin_PS5_LeftStick_DPadSouth,
      k_EInputActionOrigin_PS5_LeftStick_DPadWest,
      k_EInputActionOrigin_PS5_LeftStick_DPadEast,
      k_EInputActionOrigin_PS5_RightStick_Move,
      k_EInputActionOrigin_PS5_RightStick_Click,
      k_EInputActionOrigin_PS5_RightStick_DPadNorth,
      k_EInputActionOrigin_PS5_RightStick_DPadSouth,
      k_EInputActionOrigin_PS5_RightStick_DPadWest,
      k_EInputActionOrigin_PS5_RightStick_DPadEast,
      k_EInputActionOrigin_PS5_DPad_North,
      k_EInputActionOrigin_PS5_DPad_South,
      k_EInputActionOrigin_PS5_DPad_West,k_EInputActionOrigin_PS5_DPad_East,
      k_EInputActionOrigin_PS5_Gyro_Move,k_EInputActionOrigin_PS5_Gyro_Pitch,
      k_EInputActionOrigin_PS5_Gyro_Yaw,k_EInputActionOrigin_PS5_Gyro_Roll,
      k_EInputActionOrigin_PS5_DPad_Move,k_EInputActionOrigin_PS5_LeftGrip,
      k_EInputActionOrigin_PS5_RightGrip,k_EInputActionOrigin_PS5_LeftFn,
      k_EInputActionOrigin_PS5_RightFn,k_EInputActionOrigin_PS5_Reserved5,
      k_EInputActionOrigin_PS5_Reserved6,k_EInputActionOrigin_PS5_Reserved7,
      k_EInputActionOrigin_PS5_Reserved8,k_EInputActionOrigin_PS5_Reserved9,
      k_EInputActionOrigin_PS5_Reserved10,
      k_EInputActionOrigin_PS5_Reserved11,
      k_EInputActionOrigin_PS5_Reserved12,
      k_EInputActionOrigin_PS5_Reserved13,
      k_EInputActionOrigin_PS5_Reserved14,
      k_EInputActionOrigin_PS5_Reserved15,
      k_EInputActionOrigin_PS5_Reserved16,
      k_EInputActionOrigin_PS5_Reserved17,
      k_EInputActionOrigin_PS5_Reserved18,
      k_EInputActionOrigin_PS5_Reserved19,
      k_EInputActionOrigin_PS5_Reserved20,
      k_EInputActionOrigin_SteamDeck_A,k_EInputActionOrigin_SteamDeck_B,
      k_EInputActionOrigin_SteamDeck_X,k_EInputActionOrigin_SteamDeck_Y,
      k_EInputActionOrigin_SteamDeck_L1,k_EInputActionOrigin_SteamDeck_R1,
      k_EInputActionOrigin_SteamDeck_Menu,
      k_EInputActionOrigin_SteamDeck_View,
      k_EInputActionOrigin_SteamDeck_LeftPad_Touch,
      k_EInputActionOrigin_SteamDeck_LeftPad_Swipe,
      k_EInputActionOrigin_SteamDeck_LeftPad_Click,
      k_EInputActionOrigin_SteamDeck_LeftPad_DPadNorth,
      k_EInputActionOrigin_SteamDeck_LeftPad_DPadSouth,
      k_EInputActionOrigin_SteamDeck_LeftPad_DPadWest,
      k_EInputActionOrigin_SteamDeck_LeftPad_DPadEast,
      k_EInputActionOrigin_SteamDeck_RightPad_Touch,
      k_EInputActionOrigin_SteamDeck_RightPad_Swipe,
      k_EInputActionOrigin_SteamDeck_RightPad_Click,
      k_EInputActionOrigin_SteamDeck_RightPad_DPadNorth,
      k_EInputActionOrigin_SteamDeck_RightPad_DPadSouth,
      k_EInputActionOrigin_SteamDeck_RightPad_DPadWest,
      k_EInputActionOrigin_SteamDeck_RightPad_DPadEast,
      k_EInputActionOrigin_SteamDeck_L2_SoftPull,
      k_EInputActionOrigin_SteamDeck_L2,k_EInputActionOrigin_SteamDeck_R2_SoftPull,
      k_EInputActionOrigin_SteamDeck_R2,k_EInputActionOrigin_SteamDeck_LeftStick_Move,
      k_EInputActionOrigin_SteamDeck_L3,k_EInputActionOrigin_SteamDeck_LeftStick_DPadNorth,
      k_EInputActionOrigin_SteamDeck_LeftStick_DPadSouth,
      k_EInputActionOrigin_SteamDeck_LeftStick_DPadWest,
      k_EInputActionOrigin_SteamDeck_LeftStick_DPadEast,
      k_EInputActionOrigin_SteamDeck_LeftStick_Touch,
      k_EInputActionOrigin_SteamDeck_RightStick_Move,
      k_EInputActionOrigin_SteamDeck_R3,k_EInputActionOrigin_SteamDeck_RightStick_DPadNorth,
      k_EInputActionOrigin_SteamDeck_RightStick_DPadSouth,
      k_EInputActionOrigin_SteamDeck_RightStick_DPadWest,
      k_EInputActionOrigin_SteamDeck_RightStick_DPadEast,
      k_EInputActionOrigin_SteamDeck_RightStick_Touch,
      k_EInputActionOrigin_SteamDeck_L4,k_EInputActionOrigin_SteamDeck_R4,
      k_EInputActionOrigin_SteamDeck_L5,k_EInputActionOrigin_SteamDeck_R5,
      k_EInputActionOrigin_SteamDeck_DPad_Move,
      k_EInputActionOrigin_SteamDeck_DPad_North,
      k_EInputActionOrigin_SteamDeck_DPad_South,
      k_EInputActionOrigin_SteamDeck_DPad_West,
      k_EInputActionOrigin_SteamDeck_DPad_East,
      k_EInputActionOrigin_SteamDeck_Gyro_Move,
      k_EInputActionOrigin_SteamDeck_Gyro_Pitch,
      k_EInputActionOrigin_SteamDeck_Gyro_Yaw,
      k_EInputActionOrigin_SteamDeck_Gyro_Roll,
      k_EInputActionOrigin_SteamDeck_Reserved1,
      k_EInputActionOrigin_SteamDeck_Reserved2,
      k_EInputActionOrigin_SteamDeck_Reserved3,
      k_EInputActionOrigin_SteamDeck_Reserved4,
      k_EInputActionOrigin_SteamDeck_Reserved5,
      k_EInputActionOrigin_SteamDeck_Reserved6,
      k_EInputActionOrigin_SteamDeck_Reserved7,
      k_EInputActionOrigin_SteamDeck_Reserved8,
      k_EInputActionOrigin_SteamDeck_Reserved9,
      k_EInputActionOrigin_SteamDeck_Reserved10,
      k_EInputActionOrigin_SteamDeck_Reserved11,
      k_EInputActionOrigin_SteamDeck_Reserved12,
      k_EInputActionOrigin_SteamDeck_Reserved13,
      k_EInputActionOrigin_SteamDeck_Reserved14,
      k_EInputActionOrigin_SteamDeck_Reserved15,
      k_EInputActionOrigin_SteamDeck_Reserved16,
      k_EInputActionOrigin_SteamDeck_Reserved17,
      k_EInputActionOrigin_SteamDeck_Reserved18,
      k_EInputActionOrigin_SteamDeck_Reserved19,
      k_EInputActionOrigin_SteamDeck_Reserved20,
      k_EInputActionOrigin_Horipad_M1,k_EInputActionOrigin_Horipad_M2,
      k_EInputActionOrigin_Horipad_L4,k_EInputActionOrigin_Horipad_R4,
      k_EInputActionOrigin_Count,
      k_EInputActionOrigin_MaximumPossibleValue = 32767
      );

  PESteamInputType = ^ESteamInputType;
  ESteamInputType = (k_ESteamInputType_Unknown,k_ESteamInputType_SteamController,
    k_ESteamInputType_XBox360Controller,
    k_ESteamInputType_XBoxOneController,
    k_ESteamInputType_GenericGamepad,k_ESteamInputType_PS4Controller,
    k_ESteamInputType_AppleMFiController,
    k_ESteamInputType_AndroidController,
    k_ESteamInputType_SwitchJoyConPair,k_ESteamInputType_SwitchJoyConSingle,
    k_ESteamInputType_SwitchProController,
    k_ESteamInputType_MobileTouch,k_ESteamInputType_PS3Controller,
    k_ESteamInputType_PS5Controller,k_ESteamInputType_SteamDeckController,
    k_ESteamInputType_Count,k_ESteamInputType_MaximumPossibleValue = 255
    );

  PEXboxOrigin = ^EXboxOrigin;
  EXboxOrigin = (k_EXboxOrigin_A,k_EXboxOrigin_B,k_EXboxOrigin_X,
    k_EXboxOrigin_Y,k_EXboxOrigin_LeftBumper,
    k_EXboxOrigin_RightBumper,k_EXboxOrigin_Menu,
    k_EXboxOrigin_View,k_EXboxOrigin_LeftTrigger_Pull,
    k_EXboxOrigin_LeftTrigger_Click,k_EXboxOrigin_RightTrigger_Pull,
    k_EXboxOrigin_RightTrigger_Click,k_EXboxOrigin_LeftStick_Move,
    k_EXboxOrigin_LeftStick_Click,k_EXboxOrigin_LeftStick_DPadNorth,
    k_EXboxOrigin_LeftStick_DPadSouth,k_EXboxOrigin_LeftStick_DPadWest,
    k_EXboxOrigin_LeftStick_DPadEast,k_EXboxOrigin_RightStick_Move,
    k_EXboxOrigin_RightStick_Click,k_EXboxOrigin_RightStick_DPadNorth,
    k_EXboxOrigin_RightStick_DPadSouth,k_EXboxOrigin_RightStick_DPadWest,
    k_EXboxOrigin_RightStick_DPadEast,k_EXboxOrigin_DPad_North,
    k_EXboxOrigin_DPad_South,k_EXboxOrigin_DPad_West,
    k_EXboxOrigin_DPad_East,k_EXboxOrigin_Count
    );

  PEInputSourceMode = ^EInputSourceMode;
  EInputSourceMode = (k_EInputSourceMode_None,k_EInputSourceMode_Dpad,
    k_EInputSourceMode_Buttons,k_EInputSourceMode_FourButtons,
    k_EInputSourceMode_AbsoluteMouse,k_EInputSourceMode_RelativeMouse,
    k_EInputSourceMode_JoystickMove,k_EInputSourceMode_JoystickMouse,
    k_EInputSourceMode_JoystickCamera,k_EInputSourceMode_ScrollWheel,
    k_EInputSourceMode_Trigger,k_EInputSourceMode_TouchMenu,
    k_EInputSourceMode_MouseJoystick,k_EInputSourceMode_MouseRegion,
    k_EInputSourceMode_RadialMenu,k_EInputSourceMode_SingleButton,
    k_EInputSourceMode_Switches
  );

  PESteamInputGlyphSize = ^ESteamInputGlyphSize;
  ESteamInputGlyphSize = (k_ESteamInputGlyphSize_Small,k_ESteamInputGlyphSize_Medium,
    k_ESteamInputGlyphSize_Large,k_ESteamInputGlyphSize_Count
    );


