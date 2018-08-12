Place here your game/application asset files, that you will load during the game.

The "data" subdirectory is special for these Castle Game Engine functions:

1. It is automatically correctly packaged by the CGE build tool and editor.
   E.g. it will be correctly added to the Android apk file.

2. It is automatically recognized by the ApplicationData function.

Strictly speaking, you do not have to place your assets inside the "data"
subdirectory. In CGE, you can always load any game asset from any filename
or URL, so you can open any file on disk etc.
However, using "data" is adviced for typical cross-platform games,
this way the build tool will automatically package your game correctly.

Example things to put here:

- 3D models, loaded e.g. by TCastleScene.Load(ApplicationData('my_model.x3d'))
  Manual: https://castle-engine.io/manual_load_3d.php

- 2D images, loaded e.g. by TGLImage.Create(ApplicationData('my_image.png'))
  Manual: https://castle-engine.io/manual_2d_ui_custom_drawn.php

- Sounds, loaded e.g. by SoundEngine.LoadBuffer(ApplicationData('my_sound.wav'))
  Manual: https://castle-engine.io/manual_sound.php

- ... and really anything else you plan to load during the game.
