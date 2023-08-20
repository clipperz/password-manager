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

const _readFileFromDrop = function (event) { 
    return (onError, onSuccess) => {
        let result = new Promise((resolve, reject) => {
            var file = event.dataTransfer.files[0];
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

const renderElement = function(element) {
    return element.innerHTML;
}

const _getXClickCoordinates = function(ev) {
    return ev.clientX;
}

const _getYClickCoordinates = function(ev) {
    return ev.clientY;
}

const printEvent = function(ev) {
    return function() {
        console.log(ev)
    }
}

const cursorToEnd = function(ev) {
	return function() {
		ev.target.focus()
		ev.target.setSelectionRange(-1, -1)
	}
}

export {
    _readFile,
    _readFileFromDrop,
    renderElement,
    _getXClickCoordinates,
    _getYClickCoordinates,
    printEvent,
	cursorToEnd
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
