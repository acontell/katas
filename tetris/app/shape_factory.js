const _ = require('lodash');
const Block = require('./block');
const shapeBuilders = [shapeOne, shapeTwo, shapeThree, shapeFour, shapeFive, shapeSix, shapeSeven];

// Vertical Shape
function shapeOne(initialRow, initialColumn, rotation) {
    return {
        0: [
            new Block(initialRow, initialColumn, true),
            new Block(initialRow - 1, initialColumn),
            new Block(initialRow - 2, initialColumn),
            new Block(initialRow - 3, initialColumn)
        ],
        1: [
            new Block(initialRow, initialColumn - 1),
            new Block(initialRow, initialColumn, true),
            new Block(initialRow, initialColumn + 2),
            new Block(initialRow, initialColumn + 3)
        ]
    }[rotation % 2];
}

// Square Shape
function shapeTwo(initialRow, initialColumn, _) {
    return [
        new Block(initialRow, initialColumn, true),
        new Block(initialRow, initialColumn + 1),
        new Block(initialRow - 1, initialColumn),
        new Block(initialRow - 1, initialColumn + 1)
    ];
}

// half H shape
// const shapeThree = [new Block(0, 0), new Block(-1, 0), new Block(-1, -1), new Block(-2, 0)];
function shapeThree(initialRow, initialColumn, rotation) {
    return {
        0: [
            new Block(initialRow, initialColumn, true),
            new Block(initialRow - 1, initialColumn),
            new Block(initialRow - 1, initialColumn - 1),
            new Block(initialRow - 2, initialColumn)
        ],
        1: [
            new Block(initialRow, initialColumn, true),
            new Block(initialRow, initialColumn - 1),
            new Block(initialRow + 1, initialColumn - 1),
            new Block(initialRow, initialColumn - 2)
        ],
        2: [
            new Block(initialRow, initialColumn, true),
            new Block(initialRow, initialColumn - 1),
            new Block(initialRow + 1, initialColumn + 1),
            new Block(initialRow - 1, initialColumn + 1)
        ],
        3: [
            new Block(initialRow, initialColumn, true),
            new Block(initialRow, initialColumn - 1),
            new Block(initialRow - 1, initialColumn),
            new Block(initialRow, initialColumn + 1)
        ]
    }[rotation % 4];
}

// kind of S
// [new Block(0, 0), new Block(-1, 0), new Block(-1, 1), new Block(-2, 1)];
function shapeFour(initialRow, initialColumn, rotation) {
    return {
        0: [
            new Block(initialRow, initialColumn, true),
            new Block(initialRow - 1, initialColumn),
            new Block(initialRow - 1, initialColumn - 1),
            new Block(initialRow - 2, initialColumn - 1)
        ],
        1: [
            new Block(initialRow, initialColumn, true),
            new Block(initialRow - 1, initialColumn),
            new Block(initialRow - 1, initialColumn),
            new Block(initialRow - 1, initialColumn + 1)
        ]
    }[rotation % 2];
}

// inverted kind of S
// [new Block(0, 0), new Block(-1, 0), new Block(-1, -1), new Block(-2, -1)];
function shapeFive(initialRow, initialColumn, rotation) {
    return {
        0: [
            new Block(initialRow, initialColumn, true),
            new Block(initialRow - 1, initialColumn),
            new Block(initialRow - 1, initialColumn + 1),
            new Block(initialRow - 2, initialColumn + 1)
        ],
        1: [
            new Block(initialRow, initialColumn, true),
            new Block(initialRow, initialColumn + 1),
            new Block(initialRow - 1, initialColumn),
            new Block(initialRow - 1, initialColumn - 1)
        ]
    }[rotation % 2];
}

// L
// [new Block(0, 0), new Block(0, 1), new Block(-1, 0), new Block(-2, 0)];
function shapeSix(initialRow, initialColumn, rotation) {
    return {
        0: [
            new Block(initialRow, initialColumn, true),
            new Block(initialRow, initialColumn + 1),
            new Block(initialRow - 1, initialColumn),
            new Block(initialRow - 2, initialColumn)
        ],
        1: [
            new Block(initialRow, initialColumn, true),
            new Block(initialRow + 1, initialColumn),
            new Block(initialRow, initialColumn + 1),
            new Block(initialRow, initialColumn + 2)
        ],
        2: [
            new Block(initialRow, initialColumn, true),
            new Block(initialRow - 1, initialColumn),
            new Block(initialRow - 2, initialColumn),
            new Block(initialRow - 2, initialColumn - 1)
        ],
        3: [
            new Block(initialRow, initialColumn, true),
            new Block(initialRow, initialColumn - 1),
            new Block(initialRow, initialColumn - 2),
            new Block(initialRow - 1, initialColumn)
        ]
    }[rotation % 4];
}

// inverted L
// [new Block(0, 0), new Block(0, 1), new Block(-1, -1), new Block(-2, -1)];
function shapeSeven(initialRow, initialColumn, rotation) {
    return {
        0: [
            new Block(initialRow, initialColumn, true),
            new Block(initialRow, initialColumn + 1),
            new Block(initialRow - 1, initialColumn + 1),
            new Block(initialRow - 2, initialColumn + 1)
        ],
        1: [
            new Block(initialRow, initialColumn, true),
            new Block(initialRow - 1, initialColumn),
            new Block(initialRow, initialColumn + 1),
            new Block(initialRow, initialColumn + 2)
        ],
        2: [
            new Block(initialRow, initialColumn, true),
            new Block(initialRow + 1, initialColumn),
            new Block(initialRow + 2, initialColumn),
            new Block(initialRow + 2, initialColumn + 1)
        ],
        3: [
            new Block(initialRow, initialColumn, true),
            new Block(initialRow, initialColumn - 1),
            new Block(initialRow, initialColumn - 2),
            new Block(initialRow + 1, initialColumn)
        ]
    }[rotation % 4];
}

module.exports = {
    getBlocksOfShape: (id, initialBlock, rotation) => shapeBuilders[id](initialBlock.getRow(), initialBlock.getColumn(), rotation || 0),
    getNumberOfShapes: () => _.size(shapeBuilders)
};