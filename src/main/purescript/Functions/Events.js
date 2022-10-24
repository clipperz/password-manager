"use strict"

const _readFile = function (target) { 
    return (onError, onSuccess) => {
        let result = new Promise((resolve, reject) => {
            var file = target.files[0];
            if (!file) {
                reject("File not readable");
            }
            var reader = new FileReader();
            reader.onload = function(e) {
                resolve(e.target.result);
            };
            reader.readAsText(file);
        });

        result.then(onSuccess).catch(onError);
        return (cancelError, cancelerError, cancelerSuccess) => {
        // Handle however you'd cancel the `o` (if the API supports it)
        }
    }; 
};

const _redFile = function (target) {
    return function (onError, onSuccess) {
        var file = target.files[0];
        if (!file) {
            onError("File not readable");
        }
        var reader = new FileReader();
        reader.onload = function(e) {
            var contents = e.target.result;
            console.log("read contents " + contents)
            onSuccess(contents)
        };
            
        // result.then(onSuccess).catch(onError);
        // return (cancelError, cancelerError, cancelerSuccess) => {
            //     console.log(cancelError)
            // // Handle however you'd cancel the `o` (if the API supports it)
            // }
            
        return function (cancelError, cancelerError, cancelerSuccess) {
            cancelerSuccess()
        }
    }
}

export {
    _readFile
}

// function _randomBytes(n) { return (onError, onSuccess) => {
//     let result = new Promise((resolve, reject) => {
//         setTimeout(() => {
//             var i;
//             let result = new Uint8Array(n);  

//             for (i=0; i<n; i++) {
//                 result[i] = _randomByte();
//             }

//             resolve(result);
//         }, gratuitousTimeout);
//     });

//     result.then(onSuccess).catch(onError);
//     return (cancelError, cancelerError, cancelerSuccess) => {
// 		console.log(cancelError)
//       // Handle however you'd cancel the `o` (if the API supports it)
//     }
// }; }
