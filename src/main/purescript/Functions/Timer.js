"use strict";

var handle = null;
var timeoutTime = 0;
const resetEvents = ["click", "keydown"]
const listener = _ => resetTimer(timeoutTime)

function resetTimer(time) { clearTimeout(handle); handle = setTimer(time)}

function setTimer(time) {
    return setTimeout(ev => {
        resetEvents.forEach(element => {
            document.removeEventListener(element, listener)
        });
        document.getElementById("lockButton").dispatchEvent(new MouseEvent("click", {
            bubbles: true,
            cancelable: true,
            view: window,
        }))
    }, time * 60000)
}

const activateTimer = function(time) {
    return function() {
        // console.log("activate timer of " + time + " minutes")
        timeoutTime = time
        resetEvents.forEach(element => {
            document.addEventListener(element, listener)
        });
        handle = setTimer(time)
        return;
    }
}

const stopTimer = function() {
    // console.log("stop lock timer")
    resetEvents.forEach(element => {
        document.removeEventListener(element, listener)
    });
    clearTimeout(handle);
    return;
}

export {
    activateTimer,
    stopTimer
}
