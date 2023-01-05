"use strict"

const handleDragEvent = function(e) {
	return function (clazzName) {
		let dragElement = e.target
		console.log("Inizio la ricerca del parent")
		while (!dragElement.classList.contains(clazzName)) {
			console.log(dragElement.classList)
			console.log(dragElement.parentElement)
			dragElement = dragElement.parentElement
		}
		console.log("Trovata la classe")
		console.log(dragElement)

		e.dataTransfer.setDragImage(dragElement, 0, 30)
	}
}

export {
	handleDragEvent
}