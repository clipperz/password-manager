//
//	IE limit: 508 characters!!!!!
//

loadClipperzBookmarklet = function() {
	var	headNode;
	var clipperzScript;
	
	clipperzScript = document.getElementById('clipperzScript');
	headNode = document.getElementsByTagName("head").item(0);

	if (clipperzScript) {
		headNode.removeChild(clipperzScript);
	}

	clipperzScript = document.createElement('script');
	clipperzScript.setAttribute('src', 'http%3a%2f%2fclipperz.com%2ffiles%2fclipperz.com%2fbookmarklet%2fBookmarklet.js');
	clipperzScript.setAttribute('type', 'text/javascript');
	clipperzScript.setAttribute('defer', true);
	headNode.appendChild(clipperzScript);
};

loadClipperzBookmarklet();
