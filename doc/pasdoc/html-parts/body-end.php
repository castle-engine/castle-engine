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
     Used also by colorbox.

     For API reference: load jQuery only if not loaded yet, otherwise
     Tipue search results are broken (since they load a duplicate jQuery earlier).
     The code to load jQuery conditionally is from
     http://stackoverflow.com/questions/10371211/include-jquery-if-not-included-already
-->
<script type="text/javascript">
    if(typeof jQuery == 'undefined'){
        document.write('<script type="text/javascript" src="<?php echo CURRENT_URL; ?>kambi-php-lib/js/jquery.min.js"></'+'script>');

        /* This looks nicer, but to make it work, also the following colorbox and bootstrap JS
           have to be loaded the same way. Otherwise, colorbox and bootstrap JS are loaded
           too early, and they don't work.
        var oScriptElem = document.createElement("script");
        oScriptElem.type = "text/javascript";
        oScriptElem.src = "<?php echo CURRENT_URL; ?>kambi-php-lib/js/jquery.min.js";
        document.head.insertBefore(oScriptElem, document.head.getElementsByTagName("script")[0])
        */
    }
</script>
<!-- Include colorbox after jQuery is known -->
<script src="<?php echo CURRENT_URL; ?>kambi-php-lib/colorbox/jquery.colorbox-min.js" type="text/javascript"></script>
<script type="text/javascript">
  jQuery('a.screenshot').colorbox({opacity: 0.9, rel:'screenshot', maxWidth:'90%', maxHeight:'90%'});
</script>
<!-- Include all compiled plugins (below), or include individual files as needed -->
<script src="<?php echo CURRENT_URL; ?>kambi-php-lib/bootstrap/js/bootstrap.min.js"></script>
