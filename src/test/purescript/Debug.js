"use strict"

const formatJsonString = (str) => {
  try {
    return JSON.stringify(JSON.parse(str), null, 2)
  } catch (e) {
    return str
  }
}

const _copyState = function() {
	return (onError, onSuccess) => {
		let result = new Promise((resolve, reject) => {
			Mousetrap.bind('ctrl+alt+c', function(e) {
				resolve()
			});
        });

        result.then(onSuccess).catch(onError);
        return (cancelError, cancelerError, cancelerSuccess) => {
        }
	}
}

export {
	formatJsonString
,	_copyState
}