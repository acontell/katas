const keys = {
    37: 'moveLeft',//Left
    39: 'moveRight',// Right
    40: 'advance',// Down,
    90: 'rotateActivePiece'// z
};

module.exports = (object, handlerObject) => {
    object.addEventListener('keydown', ev => keys[ev.keyCode] && handlerObject[keys[ev.keyCode]]());
};