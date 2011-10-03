//
//	IE limit: 508 characters!!!!!
//

loadClipperzBookmarklet = function() {
	var	headNode;
	var clipperzScriptNode;
	
	clipperzScriptNode = document.getElementById('clipperzScript');
	headNode = document.getElementsByTagName('head').item(0);

	if (clipperzScriptNode) {
		headNode.removeChild(clipperzScriptNode);
	}

	clipperzScriptNode = document.createElement('script');
	clipperzScriptNode.setAttribute('src', 'http%3a%2f%2fclipperz.com%2ffiles%2fclipperz.com%2fbookmarklet%2fBookmarklet.js');
	clipperzScriptNode.setAttribute('type', 'text/javascript');
	clipperzScriptNode.setAttribute('defer', true);
	headNode.appendChild(clipperzScriptNode);
};

loadClipperzBookmarklet();
