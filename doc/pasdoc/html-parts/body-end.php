</div> <!-- .pasdoc-container -->

<?php

global $castle_php_relative_path;
$castle_php_relative_path = '../../../../cge-www/htdocs/';

require_once $castle_php_relative_path . 'castle_engine_functions.php';

echo '</div>'; // behave like castle_footer when $castle_sidebar is empty

echo_footer();
echo_piwik_tracking();
echo_google_analytics_tracking();

/* Below is copied from kambi-php-lib/kambi_common.php */
?>
<!-- jQuery (necessary for Bootstrap's JavaScript plugins).
     Used also by colorbox. -->
<script src="<?php echo CURRENT_URL; ?>kambi-php-lib/js/jquery.min.js" type="text/javascript"></script>
<!-- Include colorbox after jQuery is known -->
<script src="<?php echo CURRENT_URL; ?>kambi-php-lib/colorbox/jquery.colorbox-min.js" type="text/javascript"></script>
<script type="text/javascript">
  jQuery('a.screenshot').colorbox({opacity: 0.9, rel:'screenshot', maxWidth:'90%', maxHeight:'90%'});
</script>
<!-- Include all compiled plugins (below), or include individual files as needed -->
<script src="<?php echo CURRENT_URL; ?>kambi-php-lib/bootstrap/js/bootstrap.min.js"></script>
