"use strict"

// const handleDragEvent = function(e) {
// 	return function (clazzName) {
// 		let dragElement = e.target
// 		console.log("Inizio la ricerca del parent")
// 		while (!dragElement.classList.contains(clazzName)) {
// 			console.log(dragElement.classList)
// 			console.log(dragElement.parentElement)
// 			dragElement = dragElement.parentElement
// 		}
// 		console.log("Trovata la classe")
// 		console.log(dragElement)

// 		e.dataTransfer.setDragImage(dragElement, 30, 30)
// 	}
// }

const handleDragStartEvent_ = function(clazzName) {
    return function (x) {
        return function (y) {
            return function (e) {
                let dragElement = e.target
                // console.log("Inizio la ricerca del parent")
                while (!dragElement.classList.contains(clazzName)) {
                    // console.log(dragElement.classList)
                    // console.log(dragElement.parentElement)
                    dragElement = dragElement.parentElement
                }
                // console.log("Trovata la classe")
                // console.log(dragElement)
                e.dataTransfer.setDragImage(dragElement, x, y)
            }
        }
	}
}

export {
	handleDragStartEvent_
}