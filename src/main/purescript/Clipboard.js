"use strict"

export function copyToClipboard(text) {
	// navigator.permissions.query({name: "clipboard-write"}).then(result => {
	// 	if (result.state == "granted" || result.state == "prompt") {
			navigator.clipboard.writeText(text).then(() => {
				console.log("write successfully to clipboard `" + text + "`")
			}, () => {
				console.log("failed on writing to clipboard `" + text + "`")
			});
			return text;
	// 	}
	//   });	  
}