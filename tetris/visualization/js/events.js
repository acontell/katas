module.exports = (document, keys, handlerObject) =>
    document.addEventListener('keydown', ev => keys[ev.keyCode] && handlerObject[keys[ev.keyCode]]());