<?php
include "./setup_library/xPandMenu.php";
include "./setup_library/setup_misc.php";
if(file_exists("../configuration.php"))
{
	include_once("../configuration.php");
}

if(file_exists("../objects/class.database.php"))
{
	include_once("../objects/class.database.php");
}
include_once('../objects/class.pog_base.php');

$objectName = isset($_REQUEST['objectname']) ? $_REQUEST['objectname'] : '';
$anchor = isset($_REQUEST['anchor']) ? $_REQUEST['anchor'] : '';
$offset = isset($_REQUEST['offset']) ? $_REQUEST['offset'] : '';
$limit = isset($_REQUEST['limit']) ? $_REQUEST['limit'] : '';


//include all classes (possible relations)
$dir = opendir('../objects/');
$objects = array();
while(($file = readdir($dir)) !== false)
{
	if(strlen($file) > 4 && substr(strtolower($file), strlen($file) - 4) === '.php' && !is_dir($file) && $file != "class.database.php" && $file != "configuration.php" && $file != "setup.php" && $file != "class.pog_base.php")
	{
		$objects[] = $file;
	}
}
closedir($dir);
foreach ($objects as $object)
{
	include_once("../objects/{$object}");
}

eval ('$instance = new '.$objectName.'();');
$attributeList = array_keys(get_object_vars($instance));
$noOfExternalAttributes = sizeof($attributeList) - 3;

// get object id to perform action. required for Delete() and Update()
$objectId = isset($_REQUEST['objectid']) ? $_REQUEST['objectid'] : '';

// get the ids of all open nodes before action is performed
$openNodes = isset($_REQUEST['opennodes']) ? explode('-', $_REQUEST['opennodes']) : '';

// get action to perform
$action = $_GET['action'];

$currentNode = -1;
if (isset($_GET['currentnode']))
{
	// get the node id on which the action is performed. required for Delete() and Update()
	$currentNode = $_GET['currentnode'];
	$currentNodeParts = explode('Xnode', $currentNode);
	if (isset($currentNodeParts[1]))
	{
		$currentNode = $currentNodeParts[1];
	}
}
$root = new XMenu();

if ($openNodes != '')
{
	foreach ($openNodes as $openNode)
	{
		$openNodeParts = explode('Xtree', $openNode);
		$noParts = sizeof($openNodeParts);

		// all open nodes when action is initiated
		if ($noParts > 0 && is_numeric($openNodeParts[$noParts - 1]))
		{
			// initialize all open nodes
			$root->visibleNodes[] = $openNodeParts[$noParts - 1];
		}
	}
}
// perform requested action
switch($action)
{
    case 'Add':
    	eval ('$instance = new '.$objectName.'();');
    	$attributeList = array_keys(get_object_vars($instance));
    	foreach($attributeList as $attribute)
		{
			if ($attribute != "pog_attribute_type" && $attribute!= "pog_query")
			{
				if (isset($instance->pog_attribute_type[$attribute]))
				{
					if (isset($_GET[$attribute]))
					{
						$instance->{$attribute} = $_GET[$attribute];
					}
				}
			}
		}
		if ($instance->Save())
		{
			for ($i = 0; $i < sizeof($root->visibleNodes); $i++)
			{
				if ($root->visibleNodes[$i] > ($noOfExternalAttributes + 2))
				{
					$root->visibleNodes[$i] += ($noOfExternalAttributes + 1);
				}
			}
		}
    	RefreshTree($anchor, $root);
    break;
    case 'Refresh':
		RefreshTree($objectName, $root, $offset, $limit);
    break;
    case 'GetList':
		RefreshTree($anchor, $root, $offset, $limit);
    break;
    case 'DeleteDeep':
    case 'Delete':
    	eval ('$instance = new '.$objectName.'();');
    	$instance->Get($objectId);
    	$instance->Delete(($action == 'DeleteDeep'));
    	for ($i = 0; $i < sizeof($root->visibleNodes); $i++)
		{
			if ($root->visibleNodes[$i] > ($noOfExternalAttributes + 2))
			{
				if (intval($root->visibleNodes[$i]) == intval($openNodeParts[$noParts - 1]))
				{
					$root->visibleNodes[$i] = null;
				}
				else if ($root->visibleNodes[$i] > $currentNode)
				{
					$root->visibleNodes[$i] -= ($noOfExternalAttributes + 1);
				}
			}
		}
    	RefreshTree($anchor, $root);
    break;
    case 'Update':
    	eval ('$instance = new '.$objectName.'();');
    	$instance->Get($objectId);
    	$attributeList = array_keys(get_object_vars($instance));
    	foreach($attributeList as $attribute)
		{
			if ($attribute != "pog_attribute_type" && $attribute!= "pog_query")
			{
				if (isset($instance->pog_attribute_type[$attribute]))
				{
					if (isset($_GET[$attribute]))
					{
						$instance->{$attribute} = $_GET[$attribute];
					}
				}
			}
		}
    	$instance->Save();
    	RefreshTree($anchor, $root);
    break;
 }

 /**
  * Refreshes the tree after an operation while preserving node statuses
  *
  * @param unknown_type $objectName
  * @param unknown_type $root
  */
 function RefreshTree($objectName, $root, $offset = '', $limit = '')
 {
 		if ($limit == '')
 		{
 			$offset = 0;
 			$limit = 50;
 		}
		$sqlLimit = "$offset, $limit";

 		$js = "new Array(";
 		eval ('$instance = new '.$objectName.'();');
 		$recCount = GetNumberOfRecords(strtolower($objectName));
		$attributeList = array_keys(get_object_vars($instance));
		$instanceList = $instance->GetList(array(array(strtolower($objectName)."Id",">",0)), strtolower($objectName)."Id", false, $sqlLimit);
		$x = 0;
		$masterNode = &$root->addItem(new XNode("<span style='color:#998D05'>".$objectName."</span>&nbsp;<span style='font-weight:normal'>{Dimensions:[".sizeof($instanceList)."]}</span>", false, "setup_images/folderclose.gif","setup_images/folderopen.gif"));
		$node = &$masterNode->addItem(new XNode("<span style='color:#998D05'>ADD RECORD</span>", false,"setup_images/folderclose.gif","setup_images/folderopen.gif"));
		foreach($attributeList as $attribute)
		{
			if ($attribute != "pog_attribute_type" && $attribute!= "pog_query")
			{
				if ($x != 0 && isset($instance->pog_attribute_type[$attribute]))
				{
					$js .= '"'.$attribute.'",';
					$thisValue = ConvertAttributeToHtml($attribute, $instance->pog_attribute_type[$attribute]['db_attributes'], $instance->{$attribute}, $instance->{$attributeList[0]});
					$subnode = &$node->addItem(new XNode("<br/><span style='color:#998D05'>".$attribute."</span>&nbsp;<span style='font-weight:normal;color:#ADA8B2;'>{".$instance->pog_attribute_type[$attribute]['db_attributes'][1]."}</span><br/>".$thisValue."<br/>", false,'',"setup_images/folderopen.gif"));
				}
			}
			$x++;
		}
		$js = trim($js, ",");
		$js .= ")";
		$subnode = &$node->addItem(new XNode("<br/><a href='#' onclick='javascript:sndReq(\"Add\", getOpenNodes(), \"$objectName\", \"".$instance->{strtolower($objectName).'Id'}."\", this.parentNode.parentNode.parentNode.parentNode.id, $js, \"$objectName\");return false;'><img src='./setup_images/button_add.gif' border='0'/></a>", false,'',"folderopen.gif"));

		if ($instanceList != null)
		{
			foreach($instanceList as $instance)
			{
				ConvertObjectToNode($instance, $masterNode, $js, $objectName);
			}
		}

		$menu_html_code = $root->generateTree();
		$menu_html_code .= "<div class='nav'>";
		$pre = "<div class='nav'>";
		if ($offset != '' && $offset != 0)
		{
			$pre .= "&#8249;&#8249;<a href='#' onclick='javascript:refTree(".($offset-$limit).", $limit, \"$objectName\");return false;'>Newer</a> | ";
			$menu_html_code.= "&#8249;&#8249;<a href='#' onclick='javascript:refTree(".($offset-$limit).", $limit, \"$objectName\");return false;'>Newer</a> | ";
		}
		$pre .= "<b>".($recCount-$offset-$limit < 0 ? 0 : $recCount-$offset-$limit)." - ".($recCount-$offset)." of $recCount </b>";
		$menu_html_code .= "<b>".($recCount-$offset-$limit < 0 ? 0 : $recCount-$offset-$limit)." - ".($recCount-$offset)." of $recCount </b>";

		if ($offset <= $recCount - $limit)
		{
			$pre .= "| <a href='#' onclick='javascript:refTree(".($offset+$limit).", $limit, \"$objectName\");return false;'>Older</a>&#8250;&#8250;";
			$menu_html_code.= "| <a href='#' onclick='javascript:refTree(".($offset+$limit).", $limit, \"$objectName\");return false;'>Older</a>&#8250;&#8250;";
		}
		$menu_html_code .= "</div>";
		$pre  .= "</div>";
		$table = "<div id='container'><br/><br/>".$pre.$menu_html_code."</div>";
		echo $table;
 }
?>
