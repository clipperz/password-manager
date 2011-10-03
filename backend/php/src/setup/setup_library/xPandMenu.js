/********************************
* xPandMenu MULTI-LEVEL class
*********************************
* Javascript file
*********************************
* Patrick Brosset
* patrickbrosset@gmail.com
*********************************
* 02/2005
*********************************/


// Show / hide a sub-menu
function xMenuShowHide(obj)
{

	if(obj.style.display == 'none'){
		obj.style.display = 'block';
	}else{
		obj.style.display = 'none';
	}

}


// Toggle expanded / collapsed versions of items' images
function xSwapImg(imgDiv,srcImg,srcAltImg){

	/* Update by Christian Vallee <cv@valtechnologie.com>
	   ==> No need to specify absolute URL for images anymore, this feature will find it on its own */

		// looking for the images' root URL based on the current image
		var str = imgDiv.src;
		var pos = str.search(srcImg);
		// if the URL root wasn't found using the first image, try with the alternative one
		if ( pos == -1 ) { pos = str.search(srcAltImg); }
		// extracting the URL root
		var root = str.substring(0,pos);
		// adding the root the image path supplied
		srcImg = root.concat(srcImg);
		srcAltImg = root.concat(srcAltImg);

	/* End Update */

	if(imgDiv.src == srcImg){
		imgDiv.src = srcAltImg;
	}else{
		imgDiv.src = srcImg;
	}

}


// Restore the menu state when the page loads
function xRestoreState()
{
	//restore list state
	var name = "xMenuState";
	var start = document.cookie.indexOf(name+"=");
	if(start != -1)
	{
		var len = start+name.length+1;
		if ((!start) && (name != document.cookie.substring(0,name.length))) return null;
		if (start == -1) return null;
		var end = document.cookie.indexOf(";",len);
		if (end == -1) end = document.cookie.length;
		var value = unescape(document.cookie.substring(len,end));
		var values = value.split("|");
		for(i=0;i<values.length-1;i++)
		{
			var couple = values[i].split(":");
			document.getElementById(couple[0]).style.display = couple[1];
		}
	}
	//restore img state
	name = "xMenuStateImg";
	start = document.cookie.indexOf(name+"=");
	if(start != -1)
	{
		var len = start+name.length+1;
		if ((!start) && (name != document.cookie.substring(0,name.length))) return null;
		if (start == -1) return null;
		var end = document.cookie.indexOf(";",len);
		if (end == -1) end = document.cookie.length;
		var value = unescape(document.cookie.substring(len,end));
		var values = value.split("[]");
		for(i=0;i<values.length-1;i++)
		{
			var couple = values[i].split(">>");
			var imgs = couple[1].split(",");
			for(var il in imgs)
			{
				document.getElementById(imgs[il]).src = couple[0];
			}
		}
	}
}

//Get the ids of all open nodes
function getOpenNodes()
{
	value = new Array();
	var myLists = document.getElementsByTagName("UL");
	for(i=0;i<myLists.length;i++)
	{
		if(myLists[i].className == "Xtree" && myLists[i].style.display == "block")	value += myLists[i].id + "-";
	}
	return value;
}

// Save the menu state when the page unloads
function xSaveState()
{
	//Save list state
	var value = "";
	var myLists = document.getElementsByTagName("UL");
	for(i=0;i<myLists.length;i++)
	{
		if(myLists[i].className == "Xtree")	value += myLists[i].id + ":" + myLists[i].style.display + "|";
	}
	document.cookie = "xMenuState=" + escape(value) + ";";
	//save img state
	value = new Array();
	myLists = document.getElementsByTagName("IMG");
	for(i=0;i<myLists.length;i++)
	{
		if(myLists[i].id.substring(0,4) == "Ximg")
		{
			if(value[myLists[i].src]){value[myLists[i].src] += "," + myLists[i].id;}
			else{value[myLists[i].src] = myLists[i].id;}
		}
	}
	var str = "";
	for(var imgPath in value)
	{
		str += imgPath + ">>" + value[imgPath] + "[]";
	}
	var cook = str.substring(0,str.length-2);
	document.cookie = "xMenuStateImg=" + escape(cook) + ";";
}

function createRequestObject()
{
	var ro;
    if (window.XMLHttpRequest)
    {
  		ro = new XMLHttpRequest();
	}
	else
	{
        ro = new ActiveXObject('MSXML2.XMLHTTP.3.0');
    }
    return ro;
}

var http = createRequestObject();

function refTree(offset, limit, objectName)
{
	http = createRequestObject();
	var req = './rpc.php?action=Refresh&offset='+offset+'&limit='+limit+'&objectname='+objectName;
	http.open('get', req);
    http.onreadystatechange = handleResponse;
    http.send(null);
}

function sndReq(action, openNodes, objectName, objectId, currentNode, attributes, anchor)
{


	http = createRequestObject();
	var req = './rpc.php?action='+action+'&opennodes='+openNodes+'&objectname='+objectName+'&objectid='+objectId+'&currentnode='+currentNode+'&anchor='+anchor;
	if (action == "Add")
	{
		for (i=0; i<attributes.length; i++)
		{
			thisId = attributes[i];
			var thisInput = document.getElementById(thisId);
			if (thisInput != null)
			{
				if (thisInput.type == "checkbox")
				{
					if (thisInput.checked)
					{
						req += "&" + thisId + "=" + thisInput.value;
					}
				}
				else
				{
					req += "&" + thisId + "=" + thisInput.value;
				}
			}
		}
	}
	else if (action == "Update")
	{
		for (i=0; i<attributes.length; i++)
		{
			thisId = attributes[i];
			var thisInput = document.getElementById(thisId+"_"+objectId);
			if (thisInput.type == "checkbox")
			{
				if (thisInput.checked)
				{
					req += "&" + thisId + "=" + thisInput.value;
				}
			}
			else
			{
				req += "&" + thisId + "=" + thisInput.value;
			}
		}
	}
    http.open('get', req);
    http.onreadystatechange = handleResponse;
    http.send(null);
}

function handleResponse()
{
    if(http.readyState == 4)
    {
        var response = http.responseText;
		document.getElementById('container').innerHTML = response;
    }
}

function expandAll()
{
	var myLists = document.getElementsByTagName("UL");
	for(i=0;i<myLists.length;i++)
	{
		if(myLists[i].className == "Xtree" && myLists[i].style.display == "none")	myLists[i].style.display = "block";
	}
	myLists = document.getElementsByTagName("IMG");
	for(i=0;i<myLists.length;i++)
	{
		if(myLists[i].id.substring(0,4) == "Ximg")
		{
			myLists[i].src = "setup_images/folderopen.gif";
		}
	}
}

function collapseAll()
{
	var myLists = document.getElementsByTagName("UL");
	for(i=0;i<myLists.length;i++)
	{
		if(myLists[i].className == "Xtree" && myLists[i].style.display == "block")	myLists[i].style.display = "none";
	}
	myLists = document.getElementsByTagName("IMG");
	for(i=0;i<myLists.length;i++)
	{
		if(myLists[i].id.substring(0,4) == "Ximg")
		{
			myLists[i].src = "setup_images/folderclose.gif";
		}
	}
}

function ToggleElementVisibility(elementId)
{
	var element = document.getElementById(elementId);
	if (element.style.display != 'none')
	{
		element.style.display = 'none';
	}
	else
	{
		element.style.display = 'inline';
	}
}