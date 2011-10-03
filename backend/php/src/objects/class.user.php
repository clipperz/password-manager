<?php
/*
	This SQL query will create the table to store your object.

	CREATE TABLE `user` (
	`userid` int(11) NOT NULL auto_increment,
	`username` VARCHAR(255) NOT NULL,
	`srp_s` VARCHAR(255) NOT NULL,
	`srp_v` VARCHAR(255) NOT NULL,
	`header` LONGTEXT NOT NULL,
	`statistics` LONGTEXT NOT NULL,
	`auth_version` VARCHAR(255) NOT NULL,
	`version` VARCHAR(255) NOT NULL,
	`lock` VARCHAR(255) NOT NULL, PRIMARY KEY  (`userid`)) ENGINE=MyISAM;
*/

/**
* <b>user</b> class with integrated CRUD methods.
* @author Php Object Generator
* @version POG 3.0e / PHP5.1 MYSQL
* @see http://www.phpobjectgenerator.com/plog/tutorials/45/pdo-mysql
* @copyright Free for personal & commercial use. (Offered under the BSD license)
* @link http://www.phpobjectgenerator.com/?language=php5.1&wrapper=pdo&pdoDriver=mysql&objectName=user&attributeList=array+%28%0A++0+%3D%3E+%27username%27%2C%0A++1+%3D%3E+%27srp_s%27%2C%0A++2+%3D%3E+%27srp_v%27%2C%0A++3+%3D%3E+%27header%27%2C%0A++4+%3D%3E+%27statistics%27%2C%0A++5+%3D%3E+%27auth_version%27%2C%0A++6+%3D%3E+%27version%27%2C%0A++7+%3D%3E+%27lock%27%2C%0A++8+%3D%3E+%27record%27%2C%0A++9+%3D%3E+%27onetimepassword%27%2C%0A%29&typeList=array%2B%2528%250A%2B%2B0%2B%253D%253E%2B%2527VARCHAR%2528255%2529%2527%252C%250A%2B%2B1%2B%253D%253E%2B%2527VARCHAR%2528255%2529%2527%252C%250A%2B%2B2%2B%253D%253E%2B%2527VARCHAR%2528255%2529%2527%252C%250A%2B%2B3%2B%253D%253E%2B%2527LONGTEXT%2527%252C%250A%2B%2B4%2B%253D%253E%2B%2527LONGTEXT%2527%252C%250A%2B%2B5%2B%253D%253E%2B%2527VARCHAR%2528255%2529%2527%252C%250A%2B%2B6%2B%253D%253E%2B%2527VARCHAR%2528255%2529%2527%252C%250A%2B%2B7%2B%253D%253E%2B%2527VARCHAR%2528255%2529%2527%252C%250A%2B%2B8%2B%253D%253E%2B%2527HASMANY%2527%252C%250A%2B%2B9%2B%253D%253E%2B%2527HASMANY%2527%252C%250A%2529
*/
include_once('class.pog_base.php');
class user extends POG_Base
{
	public $userId = '';

	/**
	 * @var VARCHAR(255)
	 */
	public $username;
	
	/**
	 * @var VARCHAR(255)
	 */
	public $srp_s;
	
	/**
	 * @var VARCHAR(255)
	 */
	public $srp_v;
	
	/**
	 * @var LONGTEXT
	 */
	public $header;
	
	/**
	 * @var LONGTEXT
	 */
	public $statistics;
	
	/**
	 * @var VARCHAR(255)
	 */
	public $auth_version;
	
	/**
	 * @var VARCHAR(255)
	 */
	public $version;
	
	/**
	 * @var VARCHAR(255)
	 */
	public $lock;
	
	/**
	 * @var private array of record objects
	 */
	private $_recordList = array();
	
	/**
	 * @var private array of onetimepassword objects
	 */
	private $_onetimepasswordList = array();
	
	public $pog_attribute_type = array(
		"userId" => array('db_attributes' => array("NUMERIC", "INT")),
		"username" => array('db_attributes' => array("TEXT", "VARCHAR", "255")),
		"srp_s" => array('db_attributes' => array("TEXT", "VARCHAR", "255")),
		"srp_v" => array('db_attributes' => array("TEXT", "VARCHAR", "255")),
		"header" => array('db_attributes' => array("TEXT", "LONGTEXT")),
		"statistics" => array('db_attributes' => array("TEXT", "LONGTEXT")),
		"auth_version" => array('db_attributes' => array("TEXT", "VARCHAR", "255")),
		"version" => array('db_attributes' => array("TEXT", "VARCHAR", "255")),
		"lock" => array('db_attributes' => array("TEXT", "VARCHAR", "255")),
		"record" => array('db_attributes' => array("OBJECT", "HASMANY")),
		"onetimepassword" => array('db_attributes' => array("OBJECT", "HASMANY")),
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
	
	function user($username='', $srp_s='', $srp_v='', $header='', $statistics='', $auth_version='', $version='', $lock='')
	{
		$this->username = $username;
		$this->srp_s = $srp_s;
		$this->srp_v = $srp_v;
		$this->header = $header;
		$this->statistics = $statistics;
		$this->auth_version = $auth_version;
		$this->version = $version;
		$this->lock = $lock;
		$this->_recordList = array();
		$this->_onetimepasswordList = array();
	}
	
	
	/**
	* Gets object from database
	* @param integer $userId 
	* @return object $user
	*/
	function Get($userId)
	{
		$connection = Database::Connect();
		$this->pog_query = "select * from `user` where `userid`='".intval($userId)."' LIMIT 1";
		$cursor = Database::Reader($this->pog_query, $connection);
		while ($row = Database::Read($cursor))
		{
			$this->userId = $row['userid'];
			$this->username = $this->Unescape($row['username']);
			$this->srp_s = $this->Unescape($row['srp_s']);
			$this->srp_v = $this->Unescape($row['srp_v']);
			$this->header = $this->Unescape($row['header']);
			$this->statistics = $this->Unescape($row['statistics']);
			$this->auth_version = $this->Unescape($row['auth_version']);
			$this->version = $this->Unescape($row['version']);
			$this->lock = $this->Unescape($row['lock']);
		}
		return $this;
	}
	
	
	/**
	* Returns a sorted array of objects that match given conditions
	* @param multidimensional array {("field", "comparator", "value"), ("field", "comparator", "value"), ...} 
	* @param string $sortBy 
	* @param boolean $ascending 
	* @param int limit 
	* @return array $userList
	*/
	function GetList($fcv_array = array(), $sortBy='', $ascending=true, $limit='')
	{
		$connection = Database::Connect();
		$sqlLimit = ($limit != '' ? "LIMIT $limit" : '');
		$this->pog_query = "select * from `user` ";
		$userList = Array();
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
			$sortBy = "userid";
		}
		$this->pog_query .= " order by ".$sortBy." ".($ascending ? "asc" : "desc")." $sqlLimit";
		$thisObjectName = get_class($this);
		$cursor = Database::Reader($this->pog_query, $connection);
		while ($row = Database::Read($cursor))
		{
			$user = new $thisObjectName();
			$user->userId = $row['userid'];
			$user->username = $this->Unescape($row['username']);
			$user->srp_s = $this->Unescape($row['srp_s']);
			$user->srp_v = $this->Unescape($row['srp_v']);
			$user->header = $this->Unescape($row['header']);
			$user->statistics = $this->Unescape($row['statistics']);
			$user->auth_version = $this->Unescape($row['auth_version']);
			$user->version = $this->Unescape($row['version']);
			$user->lock = $this->Unescape($row['lock']);
			$userList[] = $user;
		}
		return $userList;
	}
	
	
	/**
	* Saves the object to the database
	* @return integer $userId
	*/
	function Save($deep = true)
	{
		$connection = Database::Connect();
		$this->pog_query = "select `userid` from `user` where `userid`='".$this->userId."' LIMIT 1";
		$rows = Database::Query($this->pog_query, $connection);
		if ($rows > 0)
		{
			$this->pog_query = "update `user` set 
			`username`='".$this->Escape($this->username)."', 
			`srp_s`='".$this->Escape($this->srp_s)."', 
			`srp_v`='".$this->Escape($this->srp_v)."', 
			`header`='".$this->Escape($this->header)."', 
			`statistics`='".$this->Escape($this->statistics)."', 
			`auth_version`='".$this->Escape($this->auth_version)."', 
			`version`='".$this->Escape($this->version)."', 
			`lock`='".$this->Escape($this->lock)."'where `userid`='".$this->userId."'";
		}
		else
		{
			$this->pog_query = "insert into `user` (`username`, `srp_s`, `srp_v`, `header`, `statistics`, `auth_version`, `version`, `lock`) values (
			'".$this->Escape($this->username)."', 
			'".$this->Escape($this->srp_s)."', 
			'".$this->Escape($this->srp_v)."', 
			'".$this->Escape($this->header)."', 
			'".$this->Escape($this->statistics)."', 
			'".$this->Escape($this->auth_version)."', 
			'".$this->Escape($this->version)."', 
			'".$this->Escape($this->lock)."')";
		}
		$insertId = Database::InsertOrUpdate($this->pog_query, $connection);
		if ($this->userId == "")
		{
			$this->userId = $insertId;
		}
		if ($deep)
		{
			foreach ($this->_recordList as $record)
			{
				$record->userId = $this->userId;
				$record->Save($deep);
			}
			foreach ($this->_onetimepasswordList as $onetimepassword)
			{
				$onetimepassword->userId = $this->userId;
				$onetimepassword->Save($deep);
			}
		}
		return $this->userId;
	}
	
	
	/**
	* Clones the object and saves it to the database
	* @return integer $userId
	*/
	function SaveNew($deep = false)
	{
		$this->userId = '';
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
			$recordList = $this->GetRecordList();
			foreach ($recordList as $record)
			{
				$record->Delete($deep, $across);
			}
			$onetimepasswordList = $this->GetOnetimepasswordList();
			foreach ($onetimepasswordList as $onetimepassword)
			{
				$onetimepassword->Delete($deep, $across);
			}
		}
		$connection = Database::Connect();
		$this->pog_query = "delete from `user` where `userid`='".$this->userId."'";
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
				$pog_query = "delete from `user` where ";
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
	* Gets a list of record objects associated to this one
	* @param multidimensional array {("field", "comparator", "value"), ("field", "comparator", "value"), ...} 
	* @param string $sortBy 
	* @param boolean $ascending 
	* @param int limit 
	* @return array of record objects
	*/
	function GetRecordList($fcv_array = array(), $sortBy='', $ascending=true, $limit='')
	{
		$record = new record();
		$fcv_array[] = array("userId", "=", $this->userId);
		$dbObjects = $record->GetList($fcv_array, $sortBy, $ascending, $limit);
		return $dbObjects;
	}
	
	
	/**
	* Makes this the parent of all record objects in the record List array. Any existing record will become orphan(s)
	* @return null
	*/
	function SetRecordList(&$list)
	{
		$this->_recordList = array();
		$existingRecordList = $this->GetRecordList();
		foreach ($existingRecordList as $record)
		{
			$record->userId = '';
			$record->Save(false);
		}
		$this->_recordList = $list;
	}
	
	
	/**
	* Associates the record object to this one
	* @return 
	*/
	function AddRecord(&$record)
	{
		$record->userId = $this->userId;
		$found = false;
		foreach($this->_recordList as $record2)
		{
			if ($record->recordId > 0 && $record->recordId == $record2->recordId)
			{
				$found = true;
				break;
			}
		}
		if (!$found)
		{
			$this->_recordList[] = $record;
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
		$fcv_array[] = array("userId", "=", $this->userId);
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
			$onetimepassword->userId = '';
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
		$onetimepassword->userId = $this->userId;
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