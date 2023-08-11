"use strict"

const blobFromArrayBuffer = function (arrayBuffer) {
	return new Blob(Array(arrayBuffer), { type: "application/octet-stream"})
}

export {
	blobFromArrayBuffer
}