module.exports = (keys, object, handlerObject) => {
    object.addEventListener('keydown', ev => keys[ev.keyCode] && handlerObject[keys[ev.keyCode]]());
};