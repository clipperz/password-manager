<?php
if (sizeof($_POST) > 0 && $GLOBALS['configuration']['setup_password'] != "" && (!isset($_SESSION['authenticated']) || !$_SESSION['authenticated']))
{
	if ($_POST['setup_password'] == $GLOBALS['configuration']['setup_password'])
	{
		$_SESSION['authenticated'] = true;
	}
	$_POST = null;
}
if ((!isset($_SESSION['authenticated']) || !$_SESSION['authenticated']) && $GLOBALS['configuration']['setup_password'] != "")
{
?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">
<head>
<title>Php Object Generator Setup <?=$GLOBALS['configuration']['versionNumber'].$GLOBALS['configuration']['revisionNumber']?></title>
<link rel="stylesheet" href="./setup.css" type="text/css" />
<link rel="stylesheet" type="text/css" href="./setup_library/xPandMenu.css"/>
<div align="center">
<form action="./index.php" method="POST"><br/>
<img src="setup_images/mini_pog.jpg"/><br/><br/>
<input name="setup_password" type="password" class="i"/>
<br/><br/><input type="image" src="setup_images/generate.jpg" name="submit"/>
</form>
</div>
</html>
<?php
exit;
}
?>