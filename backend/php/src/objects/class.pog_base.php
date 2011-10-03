<?php
class POG_Base
{
	/**
	 * Overloading
	 */
	function __call($method, $argv)
	{
		include_once($GLOBALS['configuration']['plugins_path']."/IPlugin.php");
		include_once($GLOBALS['configuration']['plugins_path']."/plugin.".strtolower($method).".php");
		eval('$plugin = new $method($this,$argv);');
		return $plugin->Execute();
	}

	/**
	 * constructor
	 *
	 * @return POG_Base
	 */
	private function POG_Base()
	{
	}


	function SetFieldAttribute($fieldName, $attributeName, $attributeValue)
	{
        if (isset($this->pog_attribute_type[$fieldName]) && isset($this->pog_attribute_type[$fieldName][$attributeName]))
        {
             $this->pog_attribute_type[$fieldName][$attributeName] = $attributeValue;
        }
	}

	function GetFieldAttribute($fieldName, $attributeName)
	{
        if (isset($this->pog_attribute_type[$fieldName]) && isset($this->pog_attribute_type[$fieldName][$attributeName]))
        {
        	return $this->pog_attribute_type[$fieldName][$attributeName];
        }
        return null;
	}

	///////////////////////////
	// Data manipulation
	///////////////////////////

	/**
	* This function will try to encode $text to base64, except when $text is a number. This allows us to Escape all data before they're inserted in the database, regardless of attribute type.
	* @param string $text
	* @return string encoded to base64
	*/
	public function Escape($text)
	{
		if ($GLOBALS['configuration']['db_encoding'] && !is_numeric($text))
		{
			return base64_encode($text);
		}
		return addslashes($text);
	}

	/**
	 * Enter description here...
	 *
	 * @param unknown_type $text
	 * @return unknown
	 */
	public function Unescape($text)
	{
		if ($GLOBALS['configuration']['db_encoding'] && !is_numeric($text))
		{
			return base64_decode($text);
		}
		return stripcslashes($text);
	}


	////////////////////////////////
	// Table -> Object Mapping
	////////////////////////////////

	/**
	 * Executes $query against database and returns the result set as an array of POG objects
	 *
	 * @param string $query. SQL query to execute against database
	 * @param string $objectClass. POG Object type to return
	 * @param bool $lazy. If true, will also load all children/sibling
	 */
	public function FetchObjects($query, $objectClass, $lazy = true)
	{
		$databaseConnection = Database::Connect();
		$result = Database::Query($query, $databaseConnection);
		$objectList = $this->CreateObjects($result, $objectClass, $lazy);
		return $objectList;
	}

	private function CreateObjects($mysql_result, $objectClass, $lazyLoad = true)
	{
		$objectList = array();
		while ($row = mysql_fetch_assoc($mysql_result))
		{
			$pog_object = new $objectClass();
			$this->PopulateObjectAttributes($row, $pog_object);
			$objectList[] = $pog_object;
		}
		return $objectList;
	}

	private function PopulateObjectAttributes($fetched_row, $pog_object)
	{
 		foreach ($this->GetAttributes($pog_object) as $column)
		{
			$pog_object->{$column} = $this->Unescape($fetched_row[strtolower($column)]);
		}
		return $pog_object;
	}

	private function GetAttributes($object)
	{
		$columns = array();
		foreach ($object->pog_attribute_type as $att => $properties)
		{
			if ($properties['db_attributes'][0] != 'OBJECT')
			{
				$columns[] = $att;
			}
		}
		return $columns;
	}

	//misc
	public static function IsColumn($value)
	{
		if (strlen($value) > 2)
		{
			if (substr($value, 0, 1) == '`' && substr($value, strlen($value) - 1, 1) == '`')
			{
				return true;
			}
			return false;
		}
		return false;
	}
}
?>