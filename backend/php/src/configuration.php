<?php
global $configuration;
$configuration['soap'] = "http://www.phpobjectgenerator.com/services/soap.php?wsdl";
$configuration['homepage'] = "http://www.phpobjectgenerator.com";
$configuration['revisionNumber'] = "";
$configuration['versionNumber'] = "3.0d";

$configuration['setup_password'] = '';


// to enable automatic data encoding, run setup, go to the manage plugins tab and install the base64 plugin.
// then set db_encoding = 1 below.
// when enabled, db_encoding transparently encodes and decodes data to and from the database without any
// programmatic effort on your part.
$configuration['db_encoding'] = 0;

// edit the information below to match your database settings

$configuration['db']	= 'clipperz'; 	//	database name
$configuration['host']	= 'localhost';	//	database host
$configuration['user']	= 'clipperz';	//	database user
$configuration['pass']	= 'clipperz';	//	database password
$configuration['port'] 	= '3306';		//	database port


//proxy settings - if you are behnd a proxy, change the settings below
$configuration['proxy_host'] = false;
$configuration['proxy_port'] = false;
$configuration['proxy_username'] = false;
$configuration['proxy_password'] = false;


//plugin settings
$configuration['plugins_path'] = dirname(__FILE__).'/plugins';

?>