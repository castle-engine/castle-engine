<?php

/* Common code for the other PHP files in this directory.
   This loads cge-www utilities for generating HTML parts for PasDoc.
   The way this works is closely synchronized with current version
   of https://github.com/castle-engine/cge-www .
*/

/* Load resources from local files,
   but make links to remote https://castle-engine.io/ */
define('CASTLE_ENVIRONMENT', 'offline');

global $castle_php_relative_path;
$castle_php_relative_path = '../../../../cge-www/htdocs/';

require_once $castle_php_relative_path . 'castle_engine_functions.php';

?>