<?php

/**
 * Class to dynamically create a zip file (archive)
 *
 * @author Rochak Chauhan. Extended by Joel Wan & Mark Slemko
 */

class createZip  {
	var $compressedData = array();
	var $centralDirectory = array(); // central directory
	var $endOfCentralDirectory = "\x50\x4b\x05\x06\x00\x00\x00\x00"; //end of Central directory record
	var $oldOffset = 0;

	/**
	 * Function to create the directory where the file(s) will be unzipped
	 *
	 * @param $directoryName string
	 *
	 */

	function addDirectory($directoryName) {
		$directoryName = str_replace("\\", "/", $directoryName);

		$feedArrayRow = "\x50\x4b\x03\x04";
		$feedArrayRow .= "\x0a\x00";
		$feedArrayRow .= "\x00\x00";
		$feedArrayRow .= "\x00\x00";
		$feedArrayRow .= "\x00\x00\x00\x00";

		$feedArrayRow .= pack("V",0);
		$feedArrayRow .= pack("V",0);
		$feedArrayRow .= pack("V",0);
		$feedArrayRow .= pack("v", strlen($directoryName) );
		$feedArrayRow .= pack("v", 0 );
		$feedArrayRow .= $directoryName;

		$feedArrayRow .= pack("V",0);
		$feedArrayRow .= pack("V",0);
		$feedArrayRow .= pack("V",0);

		$this -> compressedData[] = $feedArrayRow;

		$newOffset = strlen(implode("", $this->compressedData));

		$addCentralRecord = "\x50\x4b\x01\x02";
		$addCentralRecord .="\x00\x00";
		$addCentralRecord .="\x0a\x00";
		$addCentralRecord .="\x00\x00";
		$addCentralRecord .="\x00\x00";
		$addCentralRecord .="\x00\x00\x00\x00";
		$addCentralRecord .= pack("V",0);
		$addCentralRecord .= pack("V",0);
		$addCentralRecord .= pack("V",0);
		$addCentralRecord .= pack("v", strlen($directoryName) );
		$addCentralRecord .= pack("v", 0 );
		$addCentralRecord .= pack("v", 0 );
		$addCentralRecord .= pack("v", 0 );
		$addCentralRecord .= pack("v", 0 );
		$ext = "\x00\x00\x10\x00";
		$ext = "\xff\xff\xff\xff";
		$addCentralRecord .= pack("V", 16 );

		$addCentralRecord .= pack("V", $this -> oldOffset );
		$this -> oldOffset = $newOffset;

		$addCentralRecord .= $directoryName;

		$this -> centralDirectory[] = $addCentralRecord;
	}

	/**
	 * Function to add file(s) to the specified directory in the archive
	 *
	 * @param $directoryName string
	 *
	 */

	function addFile($data, $directoryName)   {

		$directoryName = str_replace("\\", "/", $directoryName);

		$feedArrayRow = "\x50\x4b\x03\x04";
		$feedArrayRow .= "\x14\x00";
		$feedArrayRow .= "\x00\x00";
		$feedArrayRow .= "\x08\x00";
		$feedArrayRow .= "\x00\x00\x00\x00";

		$uncompressedLength = strlen($data);
		$compression = crc32($data);
		$gzCompressedData = gzcompress($data);
		$gzCompressedData = substr( substr($gzCompressedData, 0, strlen($gzCompressedData) - 4), 2);
		$compressedLength = strlen($gzCompressedData);
		$feedArrayRow .= pack("V",$compression);
		$feedArrayRow .= pack("V",$compressedLength);
		$feedArrayRow .= pack("V",$uncompressedLength);
		$feedArrayRow .= pack("v", strlen($directoryName) );
		$feedArrayRow .= pack("v", 0 );
		$feedArrayRow .= $directoryName;

		$feedArrayRow .= $gzCompressedData;

		$feedArrayRow .= pack("V",$compression);
		$feedArrayRow .= pack("V",$compressedLength);
		$feedArrayRow .= pack("V",$uncompressedLength);

		$this -> compressedData[] = $feedArrayRow;

		$newOffset = strlen(implode("", $this->compressedData));

		$addCentralRecord = "\x50\x4b\x01\x02";
		$addCentralRecord .="\x00\x00";
		$addCentralRecord .="\x14\x00";
		$addCentralRecord .="\x00\x00";
		$addCentralRecord .="\x08\x00";
		$addCentralRecord .="\x00\x00\x00\x00";
		$addCentralRecord .= pack("V",$compression);
		$addCentralRecord .= pack("V",$compressedLength);
		$addCentralRecord .= pack("V",$uncompressedLength);
		$addCentralRecord .= pack("v", strlen($directoryName) );
		$addCentralRecord .= pack("v", 0 );
		$addCentralRecord .= pack("v", 0 );
		$addCentralRecord .= pack("v", 0 );
		$addCentralRecord .= pack("v", 0 );
		$addCentralRecord .= pack("V", 32 );

		$addCentralRecord .= pack("V", $this -> oldOffset );
		$this -> oldOffset = $newOffset;

		$addCentralRecord .= $directoryName;

		$this -> centralDirectory[] = $addCentralRecord;
	}

	/**
	 * Fucntion to return the zip file
	 *
	 * @return zipfile (archive)
	 */

	function getZippedfile() {

		$data = implode("", $this -> compressedData);
		$controlDirectory = implode("", $this -> centralDirectory);

		return
			$data.
			$controlDirectory.
			$this -> endOfCentralDirectory.
			pack("v", sizeof($this -> centralDirectory)).
			pack("v", sizeof($this -> centralDirectory)).
			pack("V", strlen($controlDirectory)).
			pack("V", strlen($data)).
			"\x00\x00";
	}

	/**
	 *
	 * Function to force the download of the archive as soon as it is created
	 *
	 * @param archiveName string - name of the created archive file
	 */

	function forceDownload($archiveName) {
		$headerInfo = '';
		header("Pragma: public");
		header("Expires: 0");
		header("Cache-Control: must-revalidate, post-check=0, pre-check=0");
		header("Cache-Control: private",false);
		header("Content-Type: application/zip");
		header("Content-Disposition: attachment; filename=".basename($archiveName).";" );
		header("Content-Transfer-Encoding: binary");
		echo $this->getZippedfile();

	 }

	/**
	 * Generates zip file from POG package.
	 *
	 * @param multi-d array $package
	 * @param array $paths
	 */
	function addPOGPackage($package, $paths=array())
	{
		
		$i = 0;
		foreach ($package as $key=>$value)
		{
			$path = '';
			foreach ($paths as $p)
			{
				$path .= (($path == '') ? $p : "/$p");
			}
			if (strpos($key, ".") == false)
			{
				$paths[] = $key;
				$this->addDirectory((($path == '') ? "$key/" : "$path/$key/"));
				$this->addPOGPackage($package[$key], &$paths);
			}
			else
			{
				$this->addFile(base64_decode($value), (($path == '') ? $key : "$path/$key"));
			}
			if ($i == (sizeof($package)-1))
			{
				array_pop($paths);
			}
			$i++;
		}
	}
}
?>