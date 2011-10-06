<?php
/*
	This SQL query will create the table to store your object.

	CREATE TABLE `recordversion` (
	`recordversionid` int(11) NOT NULL auto_increment,
	`recordid` int(11) NOT NULL,
	`reference` VARCHAR(255) NOT NULL,
	`header` LONGTEXT NOT NULL,
	`data` LONGTEXT NOT NULL,
	`version` VARCHAR(255) NOT NULL,
	`previous_version_key` VARCHAR(255) NOT NULL,
	`previous_version_id` INT NOT NULL,
	`creation_date` TIMESTAMP NOT NULL,
	`update_date` TIMESTAMP NOT NULL,
	`access_date` TIMESTAMP NOT NULL, INDEX(`recordid`), PRIMARY KEY  (`recordversionid`)) ENGINE=MyISAM;
*/

/**
* <b>recordversion</b> class with integrated CRUD methods.
* @author Php Object Generator
* @version POG 3.0e / PHP5.1 MYSQL
* @see http://www.phpobjectgenerator.com/plog/tutorials/45/pdo-mysql
* @copyright Free for personal & commercial use. (Offered under the BSD license)
* @link http://www.phpobjectgenerator.com/?language=php5.1=pdo&pdoDriver=mysql&objectName=recordversion&attributeList=array+%28%0A++0+%3D%3E+%27record%27%2C%0A++1+%3D%3E+%27reference%27%2C%0A++2+%3D%3E+%27header%27%2C%0A++3+%3D%3E+%27data%27%2C%0A++4+%3D%3E+%27version%27%2C%0A++5+%3D%3E+%27previous_version_key%27%2C%0A++6+%3D%3E+%27previous_version_id%27%2C%0A++7+%3D%3E+%27creation_date%27%2C%0A++8+%3D%3E+%27update_date%27%2C%0A++9+%3D%3E+%27access_date%27%2C%0A%29&typeList=array%2B%2528%250A%2B%2B0%2B%253D%253E%2B%2527BELONGSTO%2527%252C%250A%2B%2B1%2B%253D%253E%2B%2527VARCHAR%2528255%2529%2527%252C%250A%2B%2B2%2B%253D%253E%2B%2527LONGTEXT%2527%252C%250A%2B%2B3%2B%253D%253E%2B%2527LONGTEXT%2527%252C%250A%2B%2B4%2B%253D%253E%2B%2527VARCHAR%2528255%2529%2527%252C%250A%2B%2B5%2B%253D%253E%2B%2527VARCHAR%2528255%2529%2527%252C%250A%2B%2B6%2B%253D%253E%2B%2527INT%2527%252C%250A%2B%2B7%2B%253D%253E%2B%2527TIMESTAMP%2527%252C%250A%2B%2B8%2B%253D%253E%2B%2527TIMESTAMP%2527%252C%250A%2B%2B9%2B%253D%253E%2B%2527TIMESTAMP%2527%252C%250A%2529
*/
include_once('class.pog_base.php');
class recordversion extends POG_Base
{
	public $recordversionId = '';

	/**
	 * @var INT(11)
	 */
	public $recordId;
	
	/**
	 * @var VARCHAR(255)
	 */
	public $reference;
	
	/**
	 * @var LONGTEXT
	 */
	public $header;
	
	/**
	 * @var LONGTEXT
	 */
	public $data;
	
	/**
	 * @var VARCHAR(255)
	 */
	public $version;
	
	/**
	 * @var VARCHAR(255)
	 */
	public $previous_version_key;
	
	/**
	 * @var INT
	 */
	public $previous_version_id;
	
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
		"recordversionId" => array('db_attributes' => array("NUMERIC", "INT")),
		"record" => array('db_attributes' => array("OBJECT", "BELONGSTO")),
		"reference" => array('db_attributes' => array("TEXT", "VARCHAR", "255")),
		"header" => array('db_attributes' => array("TEXT", "LONGTEXT")),
		"data" => array('db_attributes' => array("TEXT", "LONGTEXT")),
		"version" => array('db_attributes' => array("TEXT", "VARCHAR", "255")),
		"previous_version_key" => array('db_attributes' => array("TEXT", "VARCHAR", "255")),
		"previous_version_id" => array('db_attributes' => array("NUMERIC", "INT")),
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
	
	function recordversion($reference='', $header='', $data='', $version='', $previous_version_key='', $previous_version_id='', $creation_date='', $update_date='', $access_date='')
	{
		$this->reference = $reference;
		$this->header = $header;
		$this->data = $data;
		$this->version = $version;
		$this->previous_version_key = $previous_version_key;
		$this->previous_version_id = $previous_version_id;
		$this->creation_date = $creation_date;
		$this->update_date = $update_date;
		$this->access_date = $access_date;
	}
	
	
	/**
	* Gets object from database
	* @param integer $recordversionId 
	* @return object $recordversion
	*/
	function Get($recordversionId)
	{
		$connection = Database::Connect();
		$this->pog_query = "select * from `recordversion` where `recordversionid`='".intval($recordversionId)."' LIMIT 1";
		$cursor = Database::Reader($this->pog_query, $connection);
		while ($row = Database::Read($cursor))
		{
			$this->recordversionId = $row['recordversionid'];
			$this->recordId = $row['recordid'];
			$this->reference = $this->Unescape($row['reference']);
			$this->header = $this->Unescape($row['header']);
			$this->data = $this->Unescape($row['data']);
			$this->version = $this->Unescape($row['version']);
			$this->previous_version_key = $this->Unescape($row['previous_version_key']);
			$this->previous_version_id = $this->Unescape($row['previous_version_id']);
			$this->creation_date = $row['creation_date'];
			$this->update_date = $row['update_date'];
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
	* @return array $recordversionList
	*/
	function GetList($fcv_array = array(), $sortBy='', $ascending=true, $limit='')
	{
		$connection = Database::Connect();
		$sqlLimit = ($limit != '' ? "LIMIT $limit" : '');
		$this->pog_query = "select * from `recordversion` ";
		$recordversionList = Array();
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
			$sortBy = "recordversionid";
		}
		$this->pog_query .= " order by ".$sortBy." ".($ascending ? "asc" : "desc")." $sqlLimit";
		$thisObjectName = get_class($this);
		$cursor = Database::Reader($this->pog_query, $connection);
		while ($row = Database::Read($cursor))
		{
			$recordversion = new $thisObjectName();
			$recordversion->recordversionId = $row['recordversionid'];
			$recordversion->recordId = $row['recordid'];
			$recordversion->reference = $this->Unescape($row['reference']);
			$recordversion->header = $this->Unescape($row['header']);
			$recordversion->data = $this->Unescape($row['data']);
			$recordversion->version = $this->Unescape($row['version']);
			$recordversion->previous_version_key = $this->Unescape($row['previous_version_key']);
			$recordversion->previous_version_id = $this->Unescape($row['previous_version_id']);
			$recordversion->creation_date = $row['creation_date'];
			$recordversion->update_date = $row['update_date'];
			$recordversion->access_date = $row['access_date'];
			$recordversionList[] = $recordversion;
		}
		return $recordversionList;
	}
	
	
	/**
	* Saves the object to the database
	* @return integer $recordversionId
	*/
	function Save()
	{
		$connection = Database::Connect();
		$this->update_date = date( 'Y-m-d H:i:s');
		$this->access_date = date( 'Y-m-d H:i:s');
		$this->pog_query = "select `recordversionid` from `recordversion` where `recordversionid`='".$this->recordversionId."' LIMIT 1";
		$rows = Database::Query($this->pog_query, $connection);
		if ($rows > 0)
		{
			$this->pog_query = "update `recordversion` set 
			`recordid`='".$this->recordId."', 
			`reference`='".$this->Escape($this->reference)."', 
			`header`='".$this->Escape($this->header)."', 
			`data`='".$this->Escape($this->data)."', 
			`version`='".$this->Escape($this->version)."', 
			`previous_version_key`='".$this->Escape($this->previous_version_key)."', 
			`previous_version_id`='".$this->Escape($this->previous_version_id)."', 
			`creation_date`='".$this->creation_date."', 
			`update_date`='".$this->update_date."', 
			`access_date`='".$this->access_date."' where `recordversionid`='".$this->recordversionId."'";
		}
		else
		{
			$this->pog_query = "insert into `recordversion` (`recordid`, `reference`, `header`, `data`, `version`, `previous_version_key`, `previous_version_id`, `creation_date`, `update_date`, `access_date` ) values (
			'".$this->recordId."', 
			'".$this->Escape($this->reference)."', 
			'".$this->Escape($this->header)."', 
			'".$this->Escape($this->data)."', 
			'".$this->Escape($this->version)."', 
			'".$this->Escape($this->previous_version_key)."', 
			'".$this->Escape($this->previous_version_id)."', 
			'".$this->creation_date."', 
			'".$this->update_date."', 
			'".$this->access_date."' )";
		}
		$insertId = Database::InsertOrUpdate($this->pog_query, $connection);
		if ($this->recordversionId == "")
		{
			$this->recordversionId = $insertId;
		}
		return $this->recordversionId;
	}
	
	
	/**
	* Clones the object and saves it to the database
	* @return integer $recordversionId
	*/
	function SaveNew()
	{
		$this->recordversionId = '';
		return $this->Save();
	}
	
	
	/**
	* Deletes the object from the database
	* @return boolean
	*/
	function Delete()
	{
		$connection = Database::Connect();
		$this->pog_query = "delete from `recordversion` where `recordversionid`='".$this->recordversionId."'";
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
			$pog_query = "delete from `recordversion` where ";
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
	* Associates the record object to this one
	* @return boolean
	*/
	function GetRecord()
	{
		$record = new record();
		return $record->Get($this->recordId);
	}
	
	
	/**
	* Associates the record object to this one
	* @return 
	*/
	function SetRecord(&$record)
	{
		$this->recordId = $record->recordId;
	}
}
?>
