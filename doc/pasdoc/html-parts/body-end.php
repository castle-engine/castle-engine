<?php

require_once 'cge-www-configure.php';

/* Read current CGE version from src/base/castleversion.inc */
function read_cge_version()
{
    $handle = fopen('../../../src/base/castleversion.inc', 'rt');
    if ($handle == false) {
        throw new Exception('Error: failed to open castleversion.inc');
    }
    $line = fgets($handle);
    if ($line === false) {
        throw new Exception('Error: cannot read line using fgets()');
    }
    fclose($handle);
    return trim($line, "\t\n'");
}

$cgeVersions = array(
    '6.4' => array('url' => CASTLE_FINAL_URL . 'apidoc/html/'),
    '6.5 (unstable)' => array('url' => CASTLE_FINAL_URL . 'apidoc-unstable/html/'),
);

$cgeCurrentVersion = read_cge_version();
if (!array_key_exists($cgeCurrentVersion, $cgeVersions)) {
    throw new Exception('Current version ' . $cgeCurrentVersion . ' has no entry in cgeVersions table');
}
?>

</div> <!-- .pasdoc-container -->

</div> <!-- behave like castle_footer when $castle_sidebar is empty -->

<?php
echo_footer();

/* Below is copied from castle-engine-website-base/kambi_common.php */
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
        document.write('<script type="text/javascript" src="<?php echo page_requisite('castle-engine-website-base/js/jquery.min.js'); ?>"></'+'script>');

        /* This looks nicer, but to make it work, also the following colorbox and bootstrap JS
           have to be loaded the same way. Otherwise, colorbox and bootstrap JS are loaded
           too early, and they don't work.
        var oScriptElem = document.createElement("script");
        oScriptElem.type = "text/javascript";
        oScriptElem.src = "<?php echo page_requisite('castle-engine-website-base/js/jquery.min.js'); ?>";
        document.head.insertBefore(oScriptElem, document.head.getElementsByTagName("script")[0])
        */
    }
</script>
<!-- Include colorbox after jQuery is known -->
<script src="<?php echo page_requisite('castle-engine-website-base/colorbox/jquery.colorbox-min.js'); ?>" type="text/javascript"></script>
<script type="text/javascript">
  jQuery('a.screenshot').colorbox({opacity: 0.9, rel:'screenshot', maxWidth:'90%', maxHeight:'90%'});
</script>
<!-- Include all compiled plugins (below), or include individual files as needed -->
<script src="<?php echo page_requisite('castle-engine-website-base/bootstrap/js/bootstrap.min.js'); ?>"></script>
<!-- JS using jquery -->
<script type="text/javascript" src="<?php echo page_requisite('castle-engine-website-base/castle-engine.js'); ?>"></script>

<?php

/* Add JavaScript code specially for PasDoc docs. */
echo "<script type='text/javascript'>
var cgeCurrentVersion = '$cgeCurrentVersion';
var cgeVersions = [
";
$cgeVersionsStr = '';
foreach ($cgeVersions as $versionId => $versionInfo) {
    $versionUrl = $versionInfo['url'];
    $cgeVersionsStr .= "    {'id': '$versionId', 'url': '$versionUrl'},\n";
}
$cgeVersionsStr = trim($cgeVersionsStr, "\n,"); // remove last comma
echo $cgeVersionsStr . "
];
</script>
";
?>
<script type="text/javascript" src="<?php echo page_requisite('cge-pasdoc.js'); ?>"></script>
