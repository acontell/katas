const _ = require('lodash');
const Block = require('./block');
const generators = [shapeOne, shapeTwo, shapeThree, shapeFour, shapeFive, shapeSix, shapeSeven];

// Vertical Shape
// [new Block(0, 0), new Block(-1, 0), new Block(-2, 0), new Block(-3, 0)];
function shapeOne(initialBlock) {
    return [
        initialBlock,
        new Block(initialBlock.getRow() - 1, initialBlock.getColumn()),
        new Block(initialBlock.getRow() - 2, initialBlock.getColumn()),
        new Block(initialBlock.getRow() - 3, initialBlock.getColumn())
    ];
}

// L Shape
// [new Block(0, 0), new Block(0, 1), new Block(-1, 0), new Block(-1, -1)];
function shapeTwo(initialBlock) {
    return [
        initialBlock,
        new Block(initialBlock.getRow(), initialBlock.getColumn() + 1),
        new Block(initialBlock.getRow() - 1, initialBlock.getColumn()),
        new Block(initialBlock.getRow() - 1, initialBlock.getColumn() - 1)
    ];
}

// half H shape
// const shapeThree = [new Block(0, 0), new Block(-1, 0), new Block(-1, -1), new Block(-2, 0)];
function shapeThree(initialBlock) {
    return [
        initialBlock,
        new Block(initialBlock.getRow() - 1, initialBlock.getColumn()),
        new Block(initialBlock.getRow() - 1, initialBlock.getColumn() - 1),
        new Block(initialBlock.getRow() - 2, initialBlock.getColumn())
    ];
}

// kid of S
// [new Block(0, 0), new Block(-1, 0), new Block(-1, 1), new Block(-2, 1)];
function shapeFour(initialBlock) {
    return [
        initialBlock,
        new Block(initialBlock.getRow() - 1, initialBlock.getColumn()),
        new Block(initialBlock.getRow() - 1, initialBlock.getColumn() + 1),
        new Block(initialBlock.getRow() - 2, initialBlock.getColumn() + 1)
    ];
}

// inverted kind of S
// [new Block(0, 0), new Block(-1, 0), new Block(-1, -1), new Block(-2, -1)];
function shapeFive(initialBlock) {
    return [
        initialBlock,
        new Block(initialBlock.getRow() - 1, initialBlock.getColumn()),
        new Block(initialBlock.getRow() - 1, initialBlock.getColumn() - 1),
        new Block(initialBlock.getRow() - 2, initialBlock.getColumn() - 1)
    ];
}

// L
// [new Block(0, 0), new Block(0, 1), new Block(-1, 0), new Block(-2, 0)];
function shapeSix(initialBlock) {
    return [
        initialBlock,
        new Block(initialBlock.getRow(), initialBlock.getColumn() + 1),
        new Block(initialBlock.getRow() - 1, initialBlock.getColumn()),
        new Block(initialBlock.getRow() - 2, initialBlock.getColumn())
    ];
}

// inverted L
// [new Block(0, 0), new Block(0, 1), new Block(-1, -1), new Block(-2, -1)];
function shapeSeven(initialBlock) {
    return [
        initialBlock,
        new Block(initialBlock.getRow(), initialBlock.getColumn() + 1),
        new Block(initialBlock.getRow() - 1, initialBlock.getColumn() - 1),
        new Block(initialBlock.getRow() - 2, initialBlock.getColumn() - 1)
    ];
}

module.exports = {
    getGeneratorById: id => generators[id],
    getNumberOfGenerators: () => _.size(generators)
};