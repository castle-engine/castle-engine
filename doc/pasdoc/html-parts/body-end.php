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

// Right now we only show 7.0-alpha.XXX.snapshot version
$cgeVersions = array(
    '7.0-alpha.3.snapshot' => array('url' => CASTLE_PROD_URL . 'apidoc/html/'),
);

$cgeCurrentVersion = read_cge_version();
if (!array_key_exists($cgeCurrentVersion, $cgeVersions)) {
    throw new Exception('Current version ' . $cgeCurrentVersion . ' has no entry in cgeVersions table');
}

echo '</div> <!-- .pasdoc-container -->';

// end div, just like castle_footer when $castle_sidebar is empty.
// Note: This is not included in echo_shared_body_end,
// though it is required to match echo_shared_body_begin.
echo '</div>';

echo_shared_body_end();

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
