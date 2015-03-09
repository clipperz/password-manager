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

directLoginConfigurations = {
	'Yahoo! Mail':	'{\n  "page": {\n    "title": "Yahoo! Mail"\n  },\n  "form": {\n    "attributes": {\n      "action": "https://login.yahoo.com/config/login?",\n      "method": "post"\n    },\n    "inputs": [\n      {\n        "type": "hidden",\n        "name": ".tries",\n        "value": "1"\n      },\n      {\n        "type": "hidden",\n        "name": ".src",\n        "value": "ym"\n      },\n      {\n        "type": "hidden",\n        "name": ".md5",\n        "value": ""\n      },\n      {\n        "type": "hidden",\n        "name": ".hash",\n        "value": ""\n      },\n      {\n        "type": "hidden",\n        "name": ".js",\n        "value": ""\n      },\n      {\n        "type": "hidden",\n        "name": ".last",\n        "value": ""\n      },\n      {\n        "type": "hidden",\n        "name": "promo",\n        "value": ""\n      },\n      {\n        "type": "hidden",\n        "name": ".intl",\n        "value": "us"\n      },\n      {\n        "type": "hidden",\n        "name": ".bypass",\n        "value": ""\n      },\n      {\n        "type": "hidden",\n        "name": ".partner",\n        "value": ""\n      },\n      {\n        "type": "hidden",\n        "name": ".u",\n        "value": "5gp62cl2vg3ov"\n      },\n      {\n        "type": "hidden",\n        "name": ".v",\n        "value": "0"\n      },\n      {\n        "type": "hidden",\n        "name": ".challenge",\n        "value": "iBEY0IK6k3t9Uals32mrTos8s48p"\n      },\n      {\n        "type": "hidden",\n        "name": ".yplus",\n        "value": ""\n      },\n      {\n        "type": "hidden",\n        "name": ".emailCode",\n        "value": ""\n      },\n      {\n        "type": "hidden",\n        "name": "pkg",\n        "value": ""\n      },\n      {\n        "type": "hidden",\n        "name": "stepid",\n        "value": ""\n      },\n      {\n        "type": "hidden",\n        "name": ".ev",\n        "value": ""\n      },\n      {\n        "type": "hidden",\n        "name": "hasMsgr",\n        "value": "0"\n      },\n      {\n        "type": "hidden",\n        "name": ".chkP",\n        "value": "Y"\n      },\n      {\n        "type": "hidden",\n        "name": ".done",\n        "value": "http://mail.yahoo.com"\n      },\n      {\n        "type": "hidden",\n        "name": ".pd",\n        "value": "ym_ver%3d0%26c="\n      },\n      {\n        "type": "text",\n        "name": "login",\n        "value": ""\n      },\n      {\n        "type": "password",\n        "name": "passwd",\n        "value": ""\n      },\n      {\n        "type": "checkbox",\n        "name": ".persistent",\n        "value": "y"\n      },\n      {\n        "type": "submit",\n        "name": ".save",\n        "value": "Sign In"\n      }\n    ]\n  },\n  "version": "0.2"\n}',
	'Parallels':	'{"page": {"title": "Parallels Account"},\n"form": {"attributes": {"action": "https://www.parallels.com/account/",\n"method": "post"},\n"inputs": [{"type": "text",\n"name": "Email",\n"value": ""},\n{"type": "password",\n"name": "Password",\n"value": ""}]},\n"version": "0.2.3"}',
	__syntaxFix__: "syntax fix"
};