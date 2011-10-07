<?php
/*
	This SQL query will create the table to store your object.

	CREATE TABLE `record` (
	`recordid` int(11) NOT NULL auto_increment,
	`userid` int(11) NOT NULL,
	`reference` VARCHAR(255) NOT NULL,
	`data` LONGTEXT NOT NULL,
	`version` VARCHAR(255) NOT NULL,
	`creation_date` TIMESTAMP NOT NULL,
	`update_date` TIMESTAMP NOT NULL,
	`access_date` TIMESTAMP NOT NULL, INDEX(`userid`), PRIMARY KEY  (`recordid`)) ENGINE=MyISAM;
*/

/**
* <b>record</b> class with integrated CRUD methods.
* @author Php Object Generator
* @version POG 3.0e / PHP5.1 MYSQL
* @see http://www.phpobjectgenerator.com/plog/tutorials/45/pdo-mysql
* @copyright Free for personal & commercial use. (Offered under the BSD license)
* @link http://www.phpobjectgenerator.com/?language=php5.1&wrapper=pdo&pdoDriver=mysql&objectName=record&attributeList=array+%28%0A++0+%3D%3E+%27user%27%2C%0A++1+%3D%3E+%27recordversion%27%2C%0A++2+%3D%3E+%27reference%27%2C%0A++3+%3D%3E+%27data%27%2C%0A++4+%3D%3E+%27version%27%2C%0A++5+%3D%3E+%27creation_date%27%2C%0A++6+%3D%3E+%27update_date%27%2C%0A++7+%3D%3E+%27access_date%27%2C%0A%29&typeList=array%2B%2528%250A%2B%2B0%2B%253D%253E%2B%2527BELONGSTO%2527%252C%250A%2B%2B1%2B%253D%253E%2B%2527HASMANY%2527%252C%250A%2B%2B2%2B%253D%253E%2B%2527VARCHAR%2528255%2529%2527%252C%250A%2B%2B3%2B%253D%253E%2B%2527LONGTEXT%2527%252C%250A%2B%2B4%2B%253D%253E%2B%2527VARCHAR%2528255%2529%2527%252C%250A%2B%2B5%2B%253D%253E%2B%2527TIMESTAMP%2527%252C%250A%2B%2B6%2B%253D%253E%2B%2527TIMESTAMP%2527%252C%250A%2B%2B7%2B%253D%253E%2B%2527TIMESTAMP%2527%252C%250A%2529
*/
include_once('class.pog_base.php');
class record extends POG_Base
{
	public $recordId = '';

	/**
	 * @var INT(11)
	 */
	public $userId;
	
	/**
	 * @var private array of recordversion objects
	 */
	private $_recordversionList = array();
	
	/**
	 * @var VARCHAR(255)
	 */
	public $reference;
	
	/**
	 * @var LONGTEXT
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
	public $update_date;
	
	/**
	 * @var TIMESTAMP
	 */
	public $access_date;
	
	public $pog_attribute_type = array(
		"recordId" => array('db_attributes' => array("NUMERIC", "INT")),
		"user" => array('db_attributes' => array("OBJECT", "BELONGSTO")),
		"recordversion" => array('db_attributes' => array("OBJECT", "HASMANY")),
		"reference" => array('db_attributes' => array("TEXT", "VARCHAR", "255")),
		"data" => array('db_attributes' => array("TEXT", "LONGTEXT")),
		"version" => array('db_attributes' => array("TEXT", "VARCHAR", "255")),
		"creation_date" => array('db_attributes' => array("NUMERIC", "TIMESTAMP")),
		"update_date" => array('db_attributes' => array("NUMERIC", "TIMESTAMP")),
		"access_date" => array('db_attributes' => array("NUMERIC", "TIMESTAMP")),
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
	
	function record($reference='', $data='', $version='', $creation_date='', $update_date='', $access_date='')
	{
		$this->_recordversionList = array();
		$this->reference = $reference;
		$this->data = $data;
		$this->version = $version;
		$this->creation_date = $creation_date;
		$this->update_date = $update_date;
		$this->access_date = $access_date;
	}
	
	
	/**
	* Gets object from database
	* @param integer $recordId 
	* @return object $record
	*/
	function Get($recordId)
	{
		$connection = Database::Connect();
		$this->pog_query = "select * from `record` where `recordid`='".intval($recordId)."' LIMIT 1";
		$cursor = Database::Reader($this->pog_query, $connection);
		while ($row = Database::Read($cursor))
		{
			$this->recordId = $row['recordid'];
			$this->userId = $row['userid'];
			$this->reference = $this->Unescape($row['reference']);
			$this->data = $this->Unescape($row['data']);
			$this->version = $this->Unescape($row['version']);
			$this->creation_date = $row['creation_date'];
			$oDate = strtotime($row['update_date']);
			$this->update_date = date('r', $oDate);
			$this->access_date = $row['access_date'];
		}
		return $this;
	}
	
	
	/**
	* Returns a sorted array of objects that match given conditions
	* @param multidimensional array {("field", "comparator", "value"), ("field", "comparator", "value"), ...} 
	* @param string $sortBy 
	* @param boolean $ascending 
	* @param int limit 
	* @return array $recordList
	*/
	function GetList($fcv_array = array(), $sortBy='', $ascending=true, $limit='')
	{
		$connection = Database::Connect();
		$sqlLimit = ($limit != '' ? "LIMIT $limit" : '');
		$this->pog_query = "select * from `record` ";
		$recordList = Array();
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
			$sortBy = "recordid";
		}
		$this->pog_query .= " order by ".$sortBy." ".($ascending ? "asc" : "desc")." $sqlLimit";
		$thisObjectName = get_class($this);
		$cursor = Database::Reader($this->pog_query, $connection);
		while ($row = Database::Read($cursor))
		{
			$record = new $thisObjectName();
			$record->recordId = $row['recordid'];
			$record->userId = $row['userid'];
			$record->reference = $this->Unescape($row['reference']);
			$record->data = $this->Unescape($row['data']);
			$record->version = $this->Unescape($row['version']);
			$record->creation_date = $row['creation_date'];
			$oDate = strtotime($row['update_date']);
			$record->update_date = date('r', $oDate);
			$record->access_date = $row['access_date'];
			$recordList[] = $record;
		}
		return $recordList;
	}
	
	
	/**
	* Saves the object to the database
	* @return integer $recordId
	*/
	function Save($deep = true)
	{
		$connection = Database::Connect();
		$this->update_date = date( 'r');
		$this->access_date = date( 'r');
		$this->pog_query = "select `recordid` from `record` where `recordid`='".$this->recordId."' LIMIT 1";
		$rows = Database::Query($this->pog_query, $connection);
		if ($rows > 0)
		{
			$this->pog_query = "update `record` set 
			`userid`='".$this->userId."', 
			`reference`='".$this->Escape($this->reference)."', 
			`data`='".$this->Escape($this->data)."', 
			`version`='".$this->Escape($this->version)."', 
			`creation_date`='".$this->creation_date."', 
			`update_date`='".$this->update_date."', 
			`access_date`='".$this->access_date."' where `recordid`='".$this->recordId."'";
		}
		else
		{
			$this->pog_query = "insert into `record` (`userid`, `reference`, `data`, `version`, `creation_date`, `update_date`, `access_date` ) values (
			'".$this->userId."', 
			'".$this->Escape($this->reference)."', 
			'".$this->Escape($this->data)."', 
			'".$this->Escape($this->version)."', 
			'".$this->creation_date."', 
			'".$this->update_date."', 
			'".$this->access_date."' )";
		}
		$insertId = Database::InsertOrUpdate($this->pog_query, $connection);
		if ($this->recordId == "")
		{
			$this->recordId = $insertId;
		}
		if ($deep)
		{
			foreach ($this->_recordversionList as $recordversion)
			{
				$recordversion->recordId = $this->recordId;
				$recordversion->Save($deep);
			}
		}
		return $this->recordId;
	}
	
	
	/**
	* Clones the object and saves it to the database
	* @return integer $recordId
	*/
	function SaveNew($deep = false)
	{
		$this->recordId = '';
		$this->creation_date = date( 'Y-m-d H:i:s');
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
			$recordversionList = $this->GetRecordversionList();
			foreach ($recordversionList as $recordversion)
			{
				$recordversion->Delete($deep, $across);
			}
		}
		$connection = Database::Connect();
		$this->pog_query = "delete from `record` where `recordid`='".$this->recordId."'";
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
				$pog_query = "delete from `record` where ";
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
	* Gets a list of recordversion objects associated to this one
	* @param multidimensional array {("field", "comparator", "value"), ("field", "comparator", "value"), ...} 
	* @param string $sortBy 
	* @param boolean $ascending 
	* @param int limit 
	* @return array of recordversion objects
	*/
	function GetRecordversionList($fcv_array = array(), $sortBy='', $ascending=true, $limit='')
	{
		$recordversion = new recordversion();
		$fcv_array[] = array("recordId", "=", $this->recordId);
		$dbObjects = $recordversion->GetList($fcv_array, $sortBy, $ascending, $limit);
		return $dbObjects;
	}
	
	
	/**
	* Makes this the parent of all recordversion objects in the recordversion List array. Any existing recordversion will become orphan(s)
	* @return null
	*/
	function SetRecordversionList(&$list)
	{
		$this->_recordversionList = array();
		$existingRecordversionList = $this->GetRecordversionList();
		foreach ($existingRecordversionList as $recordversion)
		{
			$recordversion->recordId = '';
			$recordversion->Save(false);
		}
		$this->_recordversionList = $list;
	}
	
	
	/**
	* Associates the recordversion object to this one
	* @return 
	*/
	function AddRecordversion(&$recordversion)
	{
		$recordversion->recordId = $this->recordId;
		$found = false;
		foreach($this->_recordversionList as $recordversion2)
		{
			if ($recordversion->recordversionId > 0 && $recordversion->recordversionId == $recordversion2->recordversionId)
			{
				$found = true;
				break;
			}
		}
		if (!$found)
		{
			$this->_recordversionList[] = $recordversion;
		}
	}
}
?>
