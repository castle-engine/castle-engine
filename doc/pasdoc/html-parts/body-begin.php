<?php

global $castle_php_relative_path;
$castle_php_relative_path = '../../../../cge-www/htdocs/';

require_once $castle_php_relative_path . 'castle_engine_functions.php';

/* For the navigation purposes (which main menu tab to show as "chosen"),
   we're the page 'reference' under 'documentation'. */
global $page_basename;
$page_basename = 'reference';
echo_castle_header_suffix(array('documentation', $page_basename), false);
?>

<div class="pasdoc-container">
