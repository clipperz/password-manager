"use strict"

function emptyByteArrayBuffer(size) {
    let result = new ArrayBuffer(size);
    return result;
}

const unit8ArrayXor = function (buf1, buf2) {
    var result = new Uint8Array(emptyByteArrayBuffer(Math.max(buf1.byteLength, buf2.byteLength)));
    buf1.forEach((e, i) => {
        result[i] = result[i] ^ e;
    });
    buf2.forEach((e, i) => {
        result[i] = result[i] ^ e;
    });
    return result;
}

// function fn2xorHighBitAligned(buffer1, buffer2) {
//     return unit8ArrayXor(new Uint8Array(buffer1), new Uint8Array(buffer2));
// }

function fn2xorLowBitAligned(buffer1, buffer2) {
    return unit8ArrayXor(new Uint8Array(buffer1).reverse(), new Uint8Array(buffer2).reverse()).reverse();
}

function toBitString(buf) {
	var result = [] // needs an array as support because a simple map doesn't work: Uint8Array can only contain bytes, not strings
	new Uint8Array(buf).forEach((byte, i) => result[i] = ("0000000" + byte.toString(2)).slice(-8))
	result = result.join("")
	return result;
}

export {
	  emptyByteArrayBuffer
	, fn2xorLowBitAligned
	, toBitString
	  // , fn2xorHighBitAligned
}
