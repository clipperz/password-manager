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

export {
	  emptyByteArrayBuffer
	, fn2xorLowBitAligned
	  // , fn2xorHighBitAligned
}
