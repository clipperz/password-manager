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

const _getWindowMessage = function() {
	return (onError, onSuccess) => {
		let result = new Promise((resolve, reject) => {
			window.addEventListener('message', (event) => {
				resolve(event.data)
			})
        });

        result.then(onSuccess).catch(onError);
        return (cancelError, cancelerError, cancelerSuccess) => {
        }
	}
}

const _keyboardShortcut = function(shortcut) {
	return (onError, onSuccess) => {
		let result = new Promise((resolve, reject) => {
			Mousetrap.bind(shortcut, function(e) {
				e.preventDefault();
				resolve()
			});
        });

        result.then(onSuccess).catch(onError);
        return (cancelError, cancelerError, cancelerSuccess) => {
        }
	}
}

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

const focus = function(id) {
	return function() {
		document.getElementById(id).focus();
		return;
	}
}

const blur = function(id) {
	return function() {
		document.getElementById(id).blur();
		return;
	}
}

export {
    _readFile,
    _readFileFromDrop,
	_getWindowMessage,
	_keyboardShortcut,
    renderElement,
    _getXClickCoordinates,
    _getYClickCoordinates,
    printEvent,
	cursorToEnd,
	focus,
	blur
}
