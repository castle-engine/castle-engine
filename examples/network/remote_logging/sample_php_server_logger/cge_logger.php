<?php

// Sends the POST parameter 'message' to syslog (usually /var/log/syslog).

if (!openlog("cge_logger", 0, LOG_LOCAL0)) {
  die('openlog failed');
}

$message = $_POST['message'];
if (!syslog(LOG_INFO, $message)) {
  die('syslog failed');
}

//echo 'Received message, stored in syslog. Contents: <b>' . htmlspecialchars($message) . '</b>';
echo 'Received message, stored in syslog. Message length: <b>' . strlen($message) . '</b>';
