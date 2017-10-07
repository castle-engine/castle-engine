<?xml version="1.0" encoding="UTF-8"?>
<app_delegate_patch>

<import>
#import "game_center/GameCenterService.h"
</import>

<create>
GameCenterService* gameCenterService;
gameCenterService = [[GameCenterService alloc] init];
gameCenterService.mainController = viewController;
gameCenterService.window = self.window;
[services addObject: gameCenterService];
</create>

</app_delegate_patch>
