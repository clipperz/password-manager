"use strict"

export function copyToClipboard(text) {
	navigator.clipboard.writeText(text).then(() => {
		console.log("write successfully to clipboard `" + text + "`")
	}, () => {
		console.log("failed on writing to clipboard `" + text + "`")
	});
	return text;
}

export function _getClipboardContent() {
	return  navigator.clipboard.readText()
}