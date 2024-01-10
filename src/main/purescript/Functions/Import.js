"use strict"

const decodeHTML = function(s) {
    var txt = document.createElement("textarea");
    txt.innerHTML = s;
    return txt.value;
}

const _readFile = function (file) { 
    return (onError, onSuccess) => {
        let result = new Promise((resolve, reject) => {
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

export {
    decodeHTML,
	_readFile
}
