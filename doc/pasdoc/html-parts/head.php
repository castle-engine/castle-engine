<?php

global $castle_php_relative_path;
$castle_php_relative_path = '../../../../cge-www/htdocs/';

require_once $castle_php_relative_path . 'castle_engine_functions.php';

/* Below is copied from kambi-php-lib/kambi_common.php */
?>

<!-- Bootstrap -->
<link href="<?php echo CURRENT_URL; ?>kambi-php-lib/bootstrap/css/bootstrap.min.css" rel="stylesheet">
<!-- Bootstrap theme -->
<link href="<?php echo CURRENT_URL; ?>kambi-php-lib/bootstrap/css/bootstrap-theme.min.css" rel="stylesheet">

<!-- Colorbox -->
<link href="<?php echo CURRENT_URL; ?>kambi-php-lib/colorbox/example3/colorbox.css" type="text/css" rel="stylesheet">

<!-- HTML5 shim and Respond.js for IE8 support of HTML5 elements and media queries -->
<!-- WARNING: Respond.js doesn't work if you view the page via file:// -->
<!--[if lt IE 9]>
  <script src="https://oss.maxcdn.com/html5shiv/3.7.3/html5shiv.min.js"></script>
  <script src="https://oss.maxcdn.com/respond/1.4.2/respond.min.js"></script>
<![endif]-->

<?php
echo_header_bonus();
?>