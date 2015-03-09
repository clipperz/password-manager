/*

Copyright 2008-2015 Clipperz Srl

This file is part of Clipperz, the online password manager.
For further information about its features and functionalities please
refer to http://www.clipperz.com.

* Clipperz is free software: you can redistribute it and/or modify it
  under the terms of the GNU Affero General Public License as published
  by the Free Software Foundation, either version 3 of the License, or 
  (at your option) any later version.

* Clipperz is distributed in the hope that it will be useful, but 
  WITHOUT ANY WARRANTY; without even the implied warranty of 
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU Affero General Public License for more details.

* You should have received a copy of the GNU Affero General Public
  License along with Clipperz. If not, see http://www.gnu.org/licenses/.

*/

var testSHA = function (aValue, anExpectedResult) {
	var byteArrayValue;
	
	byteArrayValue = new Clipperz.ByteArray(aValue);
	hash = Clipperz.Crypto.SHA.sha256(byteArrayValue);
	is(hash.toHexString(), anExpectedResult, "sha256(' " + byteArrayValue.toHexString() + "')");
	
}


var tests = {

	'basic_tests': function (someTestArgs) {
		testSHA('', '0xe3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855');
		testSHA('0xbd', '0x68325720aabd7c82f30f554b313d0570c95accbb7dc4b5aae11204c08ffe732b');
		testSHA('0x5fd4', '0x7c4fbf484498d21b487b9d61de8914b2eadaf2698712936d47c3ada2558f6788');
		testSHA('0xc98c8e55', '0x7abc22c0ae5af26ce93dbb94433a0e0b2e119d014f8e7f65bd56c61ccccd9504');
		testSHA('0x0df1cd526b5a4edd', '0x47f527210d6e8f940b5082fec01b7305908fa2b49ea3ae597c19a3986097153c');
		testSHA('0xfdf4700984ee11b70af1880d0e0fefd4', '0xb01ae16eed3b4a770f127b98469ba26fe3d8e9f59d8a2983214afe6cff0e6b6c');
		testSHA('0x8cf53d90077df9a043bf8d10b470b144784411c93a4d504556834dae3ea4a5bb', '0x56059e8cb3c2978b198208bf5ca1e1ea5659b737a506324b7cec75b5ebaf057d');
		testSHA('0xeebcf5cd6b12c90db64ff71a0e08ccd956e170a50dad769480d6b1fb3eff4934cde90f9e9b930ee637a66285c10f4e8a', '0xc117b9dce689c399ec99008788cd5d24d8396fab7d96315c4f3fe6d56da63bb3');
		testSHA('0x3592ecfd1eac618fd390e7a9c24b656532509367c21a0eac1212ac83c0b20cd896eb72b801c4d212c5452bbbf09317b50c5c9fb1997553d2bbc29bb42f5748ad', '0x105a60865830ac3a371d3843324d4bb5fa8ec0e02ddaa389ad8da4f10215c454');

	},

    //-------------------------------------------------------------------------

	'aldo_cortesi_tests': function (someTestArgs) {
		testSHA('aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa',	'0xa3f01b6939256127582ac8ae9fb47a382a244680806a3f613a118851c1ca1d47');
		testSHA('aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa',	'0xb35439a4ac6f0948b6d6f9e3c6af0f5f590ce20f1bde7090ef7970686ec6738a');
		testSHA('0x79',														'0xa1fce4363854ff888cff4b8e7875d600c2682390412a8cf79b37d0b11148b0fa');
		testSHA('0x80',														'0x76be8b528d0075f7aae98d6fa57a6d3c83ae480a8469e668d7b0af968995ac71');
	},

    //-------------------------------------------------------------------------

	'longTextPerformance_test': function (someTestArgs) {
		var longText;
		var startTime;
		var endTime;
		
		longText = "Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Donec nunc sapien, condimentum vitae, varius vel, pharetra in, augue. Mauris quam magna, pretium sit amet, accumsan id, volutpat lobortis, nibh. Fusce sagittis. Aenean justo. Curabitur euismod pede. Morbi at ante. Proin nisl leo, ultrices sed, facilisis et, nonummy sit amet, lorem. Praesent mauris tellus, pulvinar sed, nonummy vitae, rhoncus non, nunc. Proin placerat malesuada nisl. Nunc id enim. Maecenas commodo enim ac nibh. Sed condimentum, urna sit amet euismod gravida, mi urna varius odio, luctus pretium lectus justo nec felis. Ut in augue et est malesuada rhoncus. Sed vel orci. Mauris suscipit.  Praesent cursus velit non turpis. Donec tristique dolor ac est. Pellentesque habitant morbi tristique senectus et netus et malesuada fames ac turpis egestas. Nulla est sapien, vulputate eget, bibendum id, pharetra nec, mauris. Aliquam faucibus tincidunt dui. Proin iaculis. Maecenas sagittis. Integer et augue. Donec vitae urna in orci aliquet commodo. Vestibulum lorem sem, suscipit ac, placerat nec, mollis in, felis. Donec laoreet odio a mauris. Integer rutrum, sapien id varius molestie, mauris odio egestas orci, non bibendum sem felis in metus.  Phasellus consectetuer lectus adipiscing mauris. Ut magna tellus, euismod ac, suscipit tincidunt, ullamcorper adipiscing, massa. Etiam orci. Phasellus a urna. Cras neque quam, laoreet at, tempus eget, euismod nec, nibh. Etiam hendrerit. Aenean vel lorem. Ut ligula lacus, congue eu, lobortis sit amet, venenatis in, magna. Nullam cursus felis quis est. Sed sem est, condimentum eu, vestibulum a, mattis vel, diam. Curabitur tincidunt pede quis pede. Sed neque diam, convallis vel, luctus at, porta id, nisl. Suspendisse potenti. Sed volutpat lobortis orci. Praesent mi. In interdum. Suspendisse suscipit ipsum eget dolor. Curabitur et tellus sed velit hendrerit varius. Cras sit amet est.  Donec arcu nulla, vehicula et, pretium in, placerat id, felis. Integer mollis auctor lectus. Integer ultrices elementum sapien. Nam et erat. Nam pulvinar porta tortor. Nam at risus. Quisque nulla. Integer vestibulum, lacus id bibendum laoreet, ligula mi pharetra lorem, sit amet pharetra felis mauris quis justo. Aliquam ultricies. Duis a pede eget lorem dapibus rhoncus. Aenean eu elit non libero consectetuer viverra. Maecenas velit mi, eleifend vel, malesuada vel, condimentum quis, odio. Mauris tempus augue sed turpis. Pellentesque condimentum, lacus vitae pellentesque ultricies, risus tellus posuere nisi, et dictum turpis pede nec elit. Sed eu lectus eu justo sagittis euismod. Vestibulum lobortis, urna id mollis rhoncus, orci quam euismod ligula, at malesuada lacus magna vitae massa. Phasellus mattis fermentum velit.  Nulla vulputate consequat enim. Maecenas quis neque. Curabitur sagittis facilisis neque. In elementum, eros non porttitor rhoncus, libero turpis sodales odio, vitae porta tellus purus et ante. Nullam molestie sollicitudin metus. Donec a elit. Morbi ut lacus. Donec at arcu. Quisque velit diam, interdum a, lacinia at, varius et, odio. Cras neque magna, ornare id, sollicitudin id, consequat a, est. Phasellus vestibulum est at leo. Nam facilisis, nulla dapibus condimentum pellentesque, est magna viverra ligula, at sollicitudin urna augue ut sapien. Fusce justo.";
		startTime = new Date();
		testSHA(longText, '0xf6fac13c06784e0fbc61a3d25c41c9984840a8b617a2beb57cf6fa3e5e4a8949');
		endTime = new Date();

		is((endTime - startTime) < 100, true, "Long text hash performance (" + (endTime - startTime) + ")");
	},

    //-------------------------------------------------------------------------
    'syntaxFix': MochiKit.Base.noop
}

//=============================================================================

SimpleTest.runDeferredTests("Clipperz.Crypto.SHA", tests, {trace:false});
