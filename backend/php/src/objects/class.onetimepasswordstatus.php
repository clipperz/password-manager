<?php
/*
	This SQL query will create the table to store your object.

	CREATE TABLE `onetimepasswordstatus` (
	`onetimepasswordstatusid` int(11) NOT NULL auto_increment,
	`code` VARCHAR(255) NOT NULL,
	`name` VARCHAR(255) NOT NULL,
	`description` TEXT NOT NULL, PRIMARY KEY  (`onetimepasswordstatusid`)) ENGINE=MyISAM;
*/

/**
* <b>onetimepasswordstatus</b> class with integrated CRUD methods.
* @author Php Object Generator
* @version POG 3.0d / PHP5.1 MYSQL
* @see http://www.phpobjectgenerator.com/plog/tutorials/45/pdo-mysql
* @copyright Free for personal & commercial use. (Offered under the BSD license)
* @link http://www.phpobjectgenerator.com/?language=php5.1&wrapper=pdo&pdoDriver=mysql&objectName=onetimepasswordstatus&attributeList=array+%28%0A++0+%3D%3E+%27onetimepassword%27%2C%0A++1+%3D%3E+%27code%27%2C%0A++2+%3D%3E+%27name%27%2C%0A++3+%3D%3E+%27description%27%2C%0A%29&typeList=array%2B%2528%250A%2B%2B0%2B%253D%253E%2B%2527HASMANY%2527%252C%250A%2B%2B1%2B%253D%253E%2B%2527VARCHAR%2528255%2529%2527%252C%250A%2B%2B2%2B%253D%253E%2B%2527VARCHAR%2528255%2529%2527%252C%250A%2B%2B3%2B%253D%253E%2B%2527TEXT%2527%252C%250A%2529
*/
include_once('class.pog_base.php');
class onetimepasswordstatus extends POG_Base
{
	public $onetimepasswordstatusId = '';

	/**
	 * @var private array of onetimepassword objects
	 */
	private $_onetimepasswordList = array();
	
	/**
	 * @var VARCHAR(255)
	 */
	public $code;
	
	/**
	 * @var VARCHAR(255)
	 */
	public $name;
	
	/**
	 * @var TEXT
	 */
	public $description;
	
	public $pog_attribute_type = array(
		"onetimepasswordstatusId" => array('db_attributes' => array("NUMERIC", "INT")),
		"onetimepassword" => array('db_attributes' => array("OBJECT", "HASMANY")),
		"code" => array('db_attributes' => array("TEXT", "VARCHAR", "255")),
		"name" => array('db_attributes' => array("TEXT", "VARCHAR", "255")),
		"description" => array('db_attributes' => array("TEXT", "TEXT")),
		);
	public $pog_query;
	
	
	/**
	* Getter for some private attributes
	* @return mixed $attribute
	*/
	public function __get($attribute)
	{
		if (isset($this->{"_".$attribute}))
		{
			return $this->{"_".$attribute};
		}
		else
		{
			return false;
		}
	}
	
	function onetimepasswordstatus($code='', $name='', $description='')
	{
		$this->_onetimepasswordList = array();
		$this->code = $code;
		$this->name = $name;
		$this->description = $description;
	}
	
	
	/**
	* Gets object from database
	* @param integer $onetimepasswordstatusId 
	* @return object $onetimepasswordstatus
	*/
	function Get($onetimepasswordstatusId)
	{
		$connection = Database::Connect();
		$this->pog_query = "select * from `onetimepasswordstatus` where `onetimepasswordstatusid`='".intval($onetimepasswordstatusId)."' LIMIT 1";
		$cursor = Database::Reader($this->pog_query, $connection);
		while ($row = Database::Read($cursor))
		{
			$this->onetimepasswordstatusId = $row['onetimepasswordstatusid'];
			$this->code = $this->Unescape($row['code']);
			$this->name = $this->Unescape($row['name']);
			$this->description = $this->Unescape($row['description']);
		}
		return $this;
	}
	
	
	/**
	* Returns a sorted array of objects that match given conditions
	* @param multidimensional array {("field", "comparator", "value"), ("field", "comparator", "value"), ...} 
	* @param string $sortBy 
	* @param boolean $ascending 
	* @param int limit 
	* @return array $onetimepasswordstatusList
	*/
	function GetList($fcv_array = array(), $sortBy='', $ascending=true, $limit='')
	{
		$connection = Database::Connect();
		$sqlLimit = ($limit != '' ? "LIMIT $limit" : '');
		$this->pog_query = "select * from `onetimepasswordstatus` ";
		$onetimepasswordstatusList = Array();
		if (sizeof($fcv_array) > 0)
		{
			$this->pog_query .= " where ";
			for ($i=0, $c=sizeof($fcv_array); $i<$c; $i++)
			{
				if (sizeof($fcv_array[$i]) == 1)
				{
					$this->pog_query .= " ".$fcv_array[$i][0]." ";
					continue;
				}
				else
				{
					if ($i > 0 && sizeof($fcv_array[$i-1]) != 1)
					{
						$this->pog_query .= " AND ";
					}
					if (isset($this->pog_attribute_type[$fcv_array[$i][0]]['db_attributes']) && $this->pog_attribute_type[$fcv_array[$i][0]]['db_attributes'][0] != 'NUMERIC' && $this->pog_attribute_type[$fcv_array[$i][0]]['db_attributes'][0] != 'SET')
					{
						if ($GLOBALS['configuration']['db_encoding'] == 1)
						{
							$value = POG_Base::IsColumn($fcv_array[$i][2]) ? "BASE64_DECODE(".$fcv_array[$i][2].")" : "'".$fcv_array[$i][2]."'";
							$this->pog_query .= "BASE64_DECODE(`".$fcv_array[$i][0]."`) ".$fcv_array[$i][1]." ".$value;
						}
						else
						{
							$value =  POG_Base::IsColumn($fcv_array[$i][2]) ? $fcv_array[$i][2] : "'".$this->Escape($fcv_array[$i][2])."'";
							$this->pog_query .= "`".$fcv_array[$i][0]."` ".$fcv_array[$i][1]." ".$value;
						}
					}
					else
					{
						$value = POG_Base::IsColumn($fcv_array[$i][2]) ? $fcv_array[$i][2] : "'".$fcv_array[$i][2]."'";
						$this->pog_query .= "`".$fcv_array[$i][0]."` ".$fcv_array[$i][1]." ".$value;
					}
				}
			}
		}
		if ($sortBy != '')
		{
			if (isset($this->pog_attribute_type[$sortBy]['db_attributes']) && $this->pog_attribute_type[$sortBy]['db_attributes'][0] != 'NUMERIC' && $this->pog_attribute_type[$sortBy]['db_attributes'][0] != 'SET')
			{
				if ($GLOBALS['configuration']['db_encoding'] == 1)
				{
					$sortBy = "BASE64_DECODE($sortBy) ";
				}
				else
				{
					$sortBy = "$sortBy ";
				}
			}
			else
			{
				$sortBy = "$sortBy ";
			}
		}
		else
		{
			$sortBy = "onetimepasswordstatusid";
		}
		$this->pog_query .= " order by ".$sortBy." ".($ascending ? "asc" : "desc")." $sqlLimit";
		$thisObjectName = get_class($this);
		$cursor = Database::Reader($this->pog_query, $connection);
		while ($row = Database::Read($cursor))
		{
			$onetimepasswordstatus = new $thisObjectName();
			$onetimepasswordstatus->onetimepasswordstatusId = $row['onetimepasswordstatusid'];
			$onetimepasswordstatus->code = $this->Unescape($row['code']);
			$onetimepasswordstatus->name = $this->Unescape($row['name']);
			$onetimepasswordstatus->description = $this->Unescape($row['description']);
			$onetimepasswordstatusList[] = $onetimepasswordstatus;
		}
		return $onetimepasswordstatusList;
	}
	
	
	/**
	* Saves the object to the database
	* @return integer $onetimepasswordstatusId
	*/
	function Save($deep = true)
	{
		$connection = Database::Connect();
		$this->pog_query = "select `onetimepasswordstatusid` from `onetimepasswordstatus` where `onetimepasswordstatusid`='".$this->onetimepasswordstatusId."' LIMIT 1";
		$rows = Database::Query($this->pog_query, $connection);
		if ($rows > 0)
		{
			$this->pog_query = "update `onetimepasswordstatus` set 
			`code`='".$this->Escape($this->code)."', 
			`name`='".$this->Escape($this->name)."', 
			`description`='".$this->Escape($this->description)."' where `onetimepasswordstatusid`='".$this->onetimepasswordstatusId."'";
		}
		else
		{
			$this->pog_query = "insert into `onetimepasswordstatus` (`code`, `name`, `description` ) values (
			'".$this->Escape($this->code)."', 
			'".$this->Escape($this->name)."', 
			'".$this->Escape($this->description)."' )";
		}
		$insertId = Database::InsertOrUpdate($this->pog_query, $connection);
		if ($this->onetimepasswordstatusId == "")
		{
			$this->onetimepasswordstatusId = $insertId;
		}
		if ($deep)
		{
			foreach ($this->_onetimepasswordList as $onetimepassword)
			{
				$onetimepassword->onetimepasswordstatusId = $this->onetimepasswordstatusId;
				$onetimepassword->Save($deep);
			}
		}
		return $this->onetimepasswordstatusId;
	}
	
	
	/**
	* Clones the object and saves it to the database
	* @return integer $onetimepasswordstatusId
	*/
	function SaveNew($deep = false)
	{
		$this->onetimepasswordstatusId = '';
		return $this->Save($deep);
	}
	
	
	/**
	* Deletes the object from the database
	* @return boolean
	*/
	function Delete($deep = false, $across = false)
	{
		if ($deep)
		{
			$onetimepasswordList = $this->GetOnetimepasswordList();
			foreach ($onetimepasswordList as $onetimepassword)
			{
				$onetimepassword->Delete($deep, $across);
			}
		}
		$connection = Database::Connect();
		$this->pog_query = "delete from `onetimepasswordstatus` where `onetimepasswordstatusid`='".$this->onetimepasswordstatusId."'";
		return Database::NonQuery($this->pog_query, $connection);
	}
	
	
	/**
	* Deletes a list of objects that match given conditions
	* @param multidimensional array {("field", "comparator", "value"), ("field", "comparator", "value"), ...} 
	* @param bool $deep 
	* @return 
	*/
	function DeleteList($fcv_array, $deep = false, $across = false)
	{
		if (sizeof($fcv_array) > 0)
		{
			if ($deep || $across)
			{
				$objectList = $this->GetList($fcv_array);
				foreach ($objectList as $object)
				{
					$object->Delete($deep, $across);
				}
			}
			else
			{
				$connection = Database::Connect();
				$pog_query = "delete from `onetimepasswordstatus` where ";
				for ($i=0, $c=sizeof($fcv_array); $i<$c; $i++)
				{
					if (sizeof($fcv_array[$i]) == 1)
					{
						$pog_query .= " ".$fcv_array[$i][0]." ";
						continue;
					}
					else
					{
						if ($i > 0 && sizeof($fcv_array[$i-1]) !== 1)
						{
							$pog_query .= " AND ";
						}
						if (isset($this->pog_attribute_type[$fcv_array[$i][0]]['db_attributes']) && $this->pog_attribute_type[$fcv_array[$i][0]]['db_attributes'][0] != 'NUMERIC' && $this->pog_attribute_type[$fcv_array[$i][0]]['db_attributes'][0] != 'SET')
						{
							$pog_query .= "`".$fcv_array[$i][0]."` ".$fcv_array[$i][1]." '".$this->Escape($fcv_array[$i][2])."'";
						}
						else
						{
							$pog_query .= "`".$fcv_array[$i][0]."` ".$fcv_array[$i][1]." '".$fcv_array[$i][2]."'";
						}
					}
				}
				return Database::NonQuery($pog_query, $connection);
			}
		}
	}
	
	
	/**
	* Gets a list of onetimepassword objects associated to this one
	* @param multidimensional array {("field", "comparator", "value"), ("field", "comparator", "value"), ...} 
	* @param string $sortBy 
	* @param boolean $ascending 
	* @param int limit 
	* @return array of onetimepassword objects
	*/
	function GetOnetimepasswordList($fcv_array = array(), $sortBy='', $ascending=true, $limit='')
	{
		$onetimepassword = new onetimepassword();
		$fcv_array[] = array("onetimepasswordstatusId", "=", $this->onetimepasswordstatusId);
		$dbObjects = $onetimepassword->GetList($fcv_array, $sortBy, $ascending, $limit);
		return $dbObjects;
	}
	
	
	/**
	* Makes this the parent of all onetimepassword objects in the onetimepassword List array. Any existing onetimepassword will become orphan(s)
	* @return null
	*/
	function SetOnetimepasswordList(&$list)
	{
		$this->_onetimepasswordList = array();
		$existingOnetimepasswordList = $this->GetOnetimepasswordList();
		foreach ($existingOnetimepasswordList as $onetimepassword)
		{
			$onetimepassword->onetimepasswordstatusId = '';
			$onetimepassword->Save(false);
		}
		$this->_onetimepasswordList = $list;
	}
	
	
	/**
	* Associates the onetimepassword object to this one
	* @return 
	*/
	function AddOnetimepassword(&$onetimepassword)
	{
		$onetimepassword->onetimepasswordstatusId = $this->onetimepasswordstatusId;
		$found = false;
		foreach($this->_onetimepasswordList as $onetimepassword2)
		{
			if ($onetimepassword->onetimepasswordId > 0 && $onetimepassword->onetimepasswordId == $onetimepassword2->onetimepasswordId)
			{
				$found = true;
				break;
			}
		}
		if (!$found)
		{
			$this->_onetimepasswordList[] = $onetimepassword;
		}
	}
}
?>