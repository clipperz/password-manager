"use strict"

function arrayBufferToHex(buffer) {
    let result = [...new Uint8Array(buffer)]
		.map(b => b.toString(16).padStart(2, "0"))
		.join("")
	return result.length % 2 == 0 ? result : "0" + result
}

function hexToArrayBuffer(hex) {
	return new Uint8Array(hex.match(/../g).map(h => parseInt(h,16))).buffer
}

function hexEncode(string) {
    return Array.from(
        new TextEncoder().encode(string),
        byte => byte.toString(16).padStart(2, "0")
    ).join("");
}

function hexDecode(hex) {
    const bytes = new Uint8Array(hex.length / 2);
    for (let i = 0; i !== bytes.length; i++) {
        bytes[i] = parseInt(hex.substr(i * 2, 2), 16);
    }
    return (new TextDecoder().decode(bytes))
}

export {
	  arrayBufferToHex
	, hexToArrayBuffer
	, hexEncode
	, hexDecode
}
