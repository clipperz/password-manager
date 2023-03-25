"use strict"

//  String -> Int -> Int -> NativeEvent -> Effect Unit
const handleDragStartEvent_ = function(clazzName) { //  String
    return function (x) {                           //  Int
        return function (y) {                       //  Int
            return function (e) {                   //  NativeEvent
                return function () {                //  Effect
                    let dragElement = e.target
                    while (!dragElement.classList.contains(clazzName)) {
                        dragElement = dragElement.parentElement
                    }
                    e.dataTransfer.setDragImage(dragElement, x, y)
                    return e;
                    //  no return statement         //  Unit
                }
            }
        }
	}
}

export {
	handleDragStartEvent_
}