<?php
/*
	This SQL query will create the table to store your object.

	CREATE TABLE `onetimepassword` (
	`onetimepasswordid` int(11) NOT NULL auto_increment,
	`userid` int(11) NOT NULL,
	`onetimepasswordstatusid` int(11) NOT NULL,
	`reference` VARCHAR(255) NOT NULL,
	`key` VARCHAR(255) NOT NULL,
	`key_checksum` VARCHAR(255) NOT NULL,
	`data` TEXT NOT NULL,
	`version` VARCHAR(255) NOT NULL,
	`creation_date` TIMESTAMP NOT NULL,
	`request_date` TIMESTAMP NOT NULL,
	`usage_date` TIMESTAMP NOT NULL, INDEX(`userid`,`onetimepasswordstatusid`), PRIMARY KEY  (`onetimepasswordid`)) ENGINE=MyISAM;
*/

/**
* <b>onetimepassword</b> class with integrated CRUD methods.
* @author Php Object Generator
* @version POG 3.0d / PHP5.1
* @copyright Free for personal & commercial use. (Offered under the BSD license)
* @link http://www.phpobjectgenerator.com/?language=php5.1&wrapper=pog&objectName=onetimepassword&attributeList=array+%28%0A++0+%3D%3E+%27user%27%2C%0A++1+%3D%3E+%27onetimepasswordstatus%27%2C%0A++2+%3D%3E+%27reference%27%2C%0A++3+%3D%3E+%27key%27%2C%0A++4+%3D%3E+%27key_checksum%27%2C%0A++5+%3D%3E+%27data%27%2C%0A++6+%3D%3E+%27version%27%2C%0A++7+%3D%3E+%27creation_date%27%2C%0A++8+%3D%3E+%27request_date%27%2C%0A++9+%3D%3E+%27usage_date%27%2C%0A%29&typeList=array+%28%0A++0+%3D%3E+%27BELONGSTO%27%2C%0A++1+%3D%3E+%27BELONGSTO%27%2C%0A++2+%3D%3E+%27VARCHAR%28255%29%27%2C%0A++3+%3D%3E+%27VARCHAR%28255%29%27%2C%0A++4+%3D%3E+%27VARCHAR%28255%29%27%2C%0A++5+%3D%3E+%27TEXT%27%2C%0A++6+%3D%3E+%27VARCHAR%28255%29%27%2C%0A++7+%3D%3E+%27TIMESTAMP%27%2C%0A++8+%3D%3E+%27TIMESTAMP%27%2C%0A++9+%3D%3E+%27TIMESTAMP%27%2C%0A%29
*/
include_once('class.pog_base.php');
class onetimepassword extends POG_Base
{
	public $onetimepasswordId = '';

	/**
	 * @var INT(11)
	 */
	public $userId;
	
	/**
	 * @var INT(11)
	 */
	public $onetimepasswordstatusId;
	
	/**
	 * @var VARCHAR(255)
	 */
	public $reference;
	
	/**
	 * @var VARCHAR(255)
	 */
	public $key;
	
	/**
	 * @var VARCHAR(255)
	 */
	public $key_checksum;
	
	/**
	 * @var TEXT
	 */
	public $data;
	
	/**
	 * @var VARCHAR(255)
	 */
	public $version;
	
	/**
	 * @var TIMESTAMP
	 */
	public $creation_date;
	
	/**
	 * @var TIMESTAMP
	 */
	public $request_date;
	
	/**
	 * @var TIMESTAMP
	 */
	public $usage_date;
	
	public $pog_attribute_type = array(
		"onetimepasswordId" => array('db_attributes' => array("NUMERIC", "INT")),
		"user" => array('db_attributes' => array("OBJECT", "BELONGSTO")),
		"onetimepasswordstatus" => array('db_attributes' => array("OBJECT", "BELONGSTO")),
		"reference" => array('db_attributes' => array("TEXT", "VARCHAR", "255")),
		"key" => array('db_attributes' => array("TEXT", "VARCHAR", "255")),
		"key_checksum" => array('db_attributes' => array("TEXT", "VARCHAR", "255")),
		"data" => array('db_attributes' => array("TEXT", "TEXT")),
		"version" => array('db_attributes' => array("TEXT", "VARCHAR", "255")),
		"creation_date" => array('db_attributes' => array("NUMERIC", "TIMESTAMP")),
		"request_date" => array('db_attributes' => array("NUMERIC", "TIMESTAMP")),
		"usage_date" => array('db_attributes' => array("NUMERIC", "TIMESTAMP")),
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
	
	function onetimepassword($reference='', $key='', $key_checksum='', $data='', $version='', $creation_date='', $request_date='', $usage_date='')
	{
		$this->reference = $reference;
		$this->key = $key;
		$this->key_checksum = $key_checksum;
		$this->data = $data;
		$this->version = $version;
		$this->creation_date = $creation_date;
		$this->request_date = $request_date;
		$this->usage_date = $usage_date;
	}
	
	
	/**
	* Gets object from database
	* @param integer $onetimepasswordId 
	* @return object $onetimepassword
	*/
	function Get($onetimepasswordId)
	{
		$connection = Database::Connect();
		$this->pog_query = "select * from `onetimepassword` where `onetimepasswordid`='".intval($onetimepasswordId)."' LIMIT 1";
		$cursor = Database::Reader($this->pog_query, $connection);
		while ($row = Database::Read($cursor))
		{
			$this->onetimepasswordId = $row['onetimepasswordid'];
			$this->userId = $row['userid'];
			$this->onetimepasswordstatusId = $row['onetimepasswordstatusid'];
			$this->reference = $this->Unescape($row['reference']);
			$this->key = $this->Unescape($row['key']);
			$this->key_checksum = $this->Unescape($row['key_checksum']);
			$this->data = $this->Unescape($row['data']);
			$this->version = $this->Unescape($row['version']);
			$this->creation_date = $row['creation_date'];
			$this->request_date = $row['request_date'];
			$this->usage_date = $row['usage_date'];
		}
		return $this;
	}
	
	
	/**
	* Returns a sorted array of objects that match given conditions
	* @param multidimensional array {("field", "comparator", "value"), ("field", "comparator", "value"), ...} 
	* @param string $sortBy 
	* @param boolean $ascending 
	* @param int limit 
	* @return array $onetimepasswordList
	*/
	function GetList($fcv_array = array(), $sortBy='', $ascending=true, $limit='')
	{
		$connection = Database::Connect();
		$sqlLimit = ($limit != '' ? "LIMIT $limit" : '');
		$this->pog_query = "select * from `onetimepassword` ";
		$onetimepasswordList = Array();
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
			$sortBy = "onetimepasswordid";
		}
		$this->pog_query .= " order by ".$sortBy." ".($ascending ? "asc" : "desc")." $sqlLimit";
		$thisObjectName = get_class($this);
		$cursor = Database::Reader($this->pog_query, $connection);
		while ($row = Database::Read($cursor))
		{
			$onetimepassword = new $thisObjectName();
			$onetimepassword->onetimepasswordId = $row['onetimepasswordid'];
			$onetimepassword->userId = $row['userid'];
			$onetimepassword->onetimepasswordstatusId = $row['onetimepasswordstatusid'];
			$onetimepassword->reference = $this->Unescape($row['reference']);
			$onetimepassword->key = $this->Unescape($row['key']);
			$onetimepassword->key_checksum = $this->Unescape($row['key_checksum']);
			$onetimepassword->data = $this->Unescape($row['data']);
			$onetimepassword->version = $this->Unescape($row['version']);
			$onetimepassword->creation_date = $row['creation_date'];
			$onetimepassword->request_date = $row['request_date'];
			$onetimepassword->usage_date = $row['usage_date'];
			$onetimepasswordList[] = $onetimepassword;
		}
		return $onetimepasswordList;
	}
	
	
	/**
	* Saves the object to the database
	* @return integer $onetimepasswordId
	*/
	function Save()
	{
		$connection = Database::Connect();
		$this->pog_query = "select `onetimepasswordid` from `onetimepassword` where `onetimepasswordid`='".$this->onetimepasswordId."' LIMIT 1";
		$rows = Database::Query($this->pog_query, $connection);
		if ($rows > 0)
		{
			$this->pog_query = "update `onetimepassword` set 
			`userid`='".$this->userId."', 
			`onetimepasswordstatusid`='".$this->onetimepasswordstatusId."', 
			`reference`='".$this->Escape($this->reference)."', 
			`key`='".$this->Escape($this->key)."', 
			`key_checksum`='".$this->Escape($this->key_checksum)."', 
			`data`='".$this->Escape($this->data)."', 
			`version`='".$this->Escape($this->version)."', 
			`creation_date`='".$this->creation_date."', 
			`request_date`='".$this->request_date."', 
			`usage_date`='".$this->usage_date."' where `onetimepasswordid`='".$this->onetimepasswordId."'";
		}
		else
		{
			$this->pog_query = "insert into `onetimepassword` (`userid`, `onetimepasswordstatusid`, `reference`, `key`, `key_checksum`, `data`, `version`, `creation_date`, `request_date`, `usage_date` ) values (
			'".$this->userId."', 
			'".$this->onetimepasswordstatusId."', 
			'".$this->Escape($this->reference)."', 
			'".$this->Escape($this->key)."', 
			'".$this->Escape($this->key_checksum)."', 
			'".$this->Escape($this->data)."', 
			'".$this->Escape($this->version)."', 
			'".$this->creation_date."', 
			'".$this->request_date."', 
			'".$this->usage_date."' )";
		}
		$insertId = Database::InsertOrUpdate($this->pog_query, $connection);
		if ($this->onetimepasswordId == "")
		{
			$this->onetimepasswordId = $insertId;
		}
		return $this->onetimepasswordId;
	}
	
	
	/**
	* Clones the object and saves it to the database
	* @return integer $onetimepasswordId
	*/
	function SaveNew()
	{
		$this->onetimepasswordId = '';
		return $this->Save();
	}
	
	
	/**
	* Deletes the object from the database
	* @return boolean
	*/
	function Delete()
	{
		$connection = Database::Connect();
		$this->pog_query = "delete from `onetimepassword` where `onetimepasswordid`='".$this->onetimepasswordId."'";
		return Database::NonQuery($this->pog_query, $connection);
	}
	
	
	/**
	* Deletes a list of objects that match given conditions
	* @param multidimensional array {("field", "comparator", "value"), ("field", "comparator", "value"), ...} 
	* @param bool $deep 
	* @return 
	*/
	function DeleteList($fcv_array)
	{
		if (sizeof($fcv_array) > 0)
		{
			$connection = Database::Connect();
			$pog_query = "delete from `onetimepassword` where ";
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
	
	
	/**
	* Associates the user object to this one
	* @return boolean
	*/
	function GetUser()
	{
		$user = new user();
		return $user->Get($this->userId);
	}
	
	
	/**
	* Associates the user object to this one
	* @return 
	*/
	function SetUser(&$user)
	{
		$this->userId = $user->userId;
	}
	
	
	/**
	* Associates the onetimepasswordstatus object to this one
	* @return boolean
	*/
	function GetOnetimepasswordstatus()
	{
		$onetimepasswordstatus = new onetimepasswordstatus();
		return $onetimepasswordstatus->Get($this->onetimepasswordstatusId);
	}
	
	
	/**
	* Associates the onetimepasswordstatus object to this one
	* @return 
	*/
	function SetOnetimepasswordstatus(&$onetimepasswordstatus)
	{
		$this->onetimepasswordstatusId = $onetimepasswordstatus->onetimepasswordstatusId;
	}
}
?>