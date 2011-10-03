<?php
header('Content-Type: text/html');
header('Content-Disposition: attachment; filename=Clipperz_'.date("Ymd").'.html');


	include "./configuration.php";
	include "./objects/class.database.php";
	include "./objects/class.user.php";
	include "./objects/class.record.php";
	include "./objects/class.recordversion.php";
	include "./objects/class.onetimepassword.php";
	include "./objects/class.onetimepasswordstatus.php";

	$htmlContent = file_get_contents("../index.html");

	session_start();

	$user = new user();
	$user = $user->Get($_SESSION["userId"]);
	$records = $user->GetRecordList();
	
	$recordString = "";
	$isFirstRecord = true;
	
	$c = count($records);
	for ($i=0; $i<$c; $i++) {
		$currentRecord = $records[$i];
		$recordVersions = $currentRecord->GetRecordversionList();
		

		if ($isFirstRecord == true) {
			$isFirstRecord = false;
		} else {
			$recordString = $recordString . ",\n";
		}
		
		$versionString = "";
		$isFirstVersion = true;
		
		$cc = count($recordVersions);
		for ($ii=0; $ii<$cc; $ii++) {
			$currentVersion = $recordVersions[$ii];
			
			if ($isFirstVersion == true) {
				$isFirstVersion = false;
			} else {
				$versionString = $versionString . ",\n";
			}

			$versionsString = $versionString .	"\t\t\t\t\t\t'" . $currentVersion->reference . "': {\n" .
													"\t\t\t\t\t\t\theader: '"		. $currentVersion->header . "',\n" .
													"\t\t\t\t\t\t\tdata: '"			. $currentVersion->data . "',\n" .
													"\t\t\t\t\t\t\tversion: '"		. $currentVersion->version . "',\n" .
													"\t\t\t\t\t\t\tcreationDate: '"	. $currentVersion->creation_date . "',\n" .
													"\t\t\t\t\t\t\tupdateDate: '"	. $currentVersion->update_date . "',\n" .
													"\t\t\t\t\t\t\taccessDate: '"	. $currentVersion->access_date . "'\n" .
												"\t\t\t\t\t\t}";
		}
		
		$recordString = $recordString .	"\t\t\t\t'" . $currentRecord->reference . "': {\n" .
											"\t\t\t\t\tdata: '"				. $currentRecord->data . "',\n" .
											"\t\t\t\t\tversion: '"			. $currentRecord->version . "',\n" .
											"\t\t\t\t\tcreationDate: '"		. $currentRecord->creation_date . "',\n" .
											"\t\t\t\t\tupdateDate: '"		. $currentRecord->update_date . "',\n" .
											"\t\t\t\t\taccessDate: '"		. $currentRecord->access_date . "',\n" .
											"\t\t\t\t\tcurrentVersion: '"	. $currentVersion->reference . "',\n" .
											"\t\t\t\t\tversions: {\n" .
												$versionsString . "\n" .
											"\t\t\t\t\t}\n" .
										"\t\t\t\t}";
	}


	$data =		"_clipperz_dump_data_ = {\n" .
						"\tusers:{\n" .
							"\t\t'catchAllUser': {\n" .
								"\t\t\t__masterkey_test_value__: 'masterkey',\n" .
								"\t\t\ts: '112233445566778899aabbccddeeff00112233445566778899aabbccddeeff00',\n" .
								"\t\t\tv: '112233445566778899aabbccddeeff00112233445566778899aabbccddeeff00'\n"  .
							"\t\t},\n" .
							"\t\t'" . $user->username . "': {\n" .
								"\t\t\ts: '" . $user->srp_s . "',\n" .
								"\t\t\tv: '" . $user->srp_v . "',\n" .
								"\t\t\tversion: '" . $user->auth_version . "',\n" .
								"\t\t\tmaxNumberOfRecords: '" . "100" . "',\n" .
								"\t\t\tuserDetails: '" . $user->header . "',\n" .
								"\t\t\tstatistics: '" . $user->statistics . "',\n" .
								"\t\t\tuserDetailsVersion: '" . $user->version . "',\n" .
								"\t\t\trecords: {\n" .
									$recordString . "\n" .
								"\t\t\t}\n" .
							"\t\t}\n" .
						"\t}\n" .
					"}\n" .
					"\n" .
					"Clipperz.PM.Proxy.defaultProxy = new Clipperz.PM.Proxy.Offline();\n" .
					"Clipperz.Crypto.PRNG.defaultRandomGenerator().fastEntropyAccumulationForTestingPurpose();" .
					"\n";

	session_write_close();
	
	echo str_replace("/*offline_data_placeholder*/", $data, $htmlContent);
?>