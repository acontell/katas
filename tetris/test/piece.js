const _ = require('lodash');
const expect = require('chai').expect;
const pieceFactory = require('../app/piece_factory');
const math = require('../app/math');
const Block = require('../app/block');

// vertical shape
const shapeOne = [new Block(0, 0), new Block(-1, 0), new Block(-2, 0), new Block(-3, 0)];
// square shape
const shapeTwo = [new Block(0, 0), new Block(0, 1), new Block(-1, 0), new Block(-1, -1)];
// half H shape
const shapeThree = [new Block(0, 0), new Block(-1, 0), new Block(-1, -1), new Block(-2, 0)];
// kid of S
const shapeFour = [new Block(0, 0), new Block(-1, 0), new Block(-1, 1), new Block(-2, 1)];
// inverted kind of S
const shapeFive = [new Block(0, 0), new Block(-1, 0), new Block(-1, -1), new Block(-2, -1)];
// L
const shapeSix = [new Block(0, 0), new Block(0, 1), new Block(-1, 0), new Block(-2, 0)];
// inverted L
const shapeSeven = [new Block(0, 0), new Block(0, 1), new Block(-1, -1), new Block(-2, -1)];

const shapes = [shapeOne, shapeTwo, shapeThree, shapeFour, shapeFive, shapeSix, shapeSeven];

describe('As the game', () => {
    describe('In order to add complexity to the Game', () => {
        it('should have pieces of 4 blocks', () => {
            let pieces = pieceFactory.getListOfAvailablePieces();
            expect(_.every(pieces, checkBlocksSize)).to.be.true;
        });
        it('should have pieces of contiguous blocks', () => {
            let pieces = pieceFactory.getListOfAvailablePieces();
            expect(_.every(pieces, checkContiguousBlocks)).to.be.true;
        });
        it('should have 7 specific shapes', () => {
            expect(_.every(_.zip(shapes, getBlocks()), checkBlocks)).to.be.true;
        });
    });
});

function checkBlocksSize(piece) {
    return _.size(piece.getBlocks()) === 4;
}

function checkContiguousBlocks(piece) {
    let blocks = piece.getBlocks();
    return _.every(blocks, block => _.some(blocks, isContiguous(block)));
}

function isContiguous(block) {
    return aBlock => math.isContiguous(block, aBlock);
}

function getBlocks() {
    return pieceFactory.getListOfAvailablePieces().map(piece => piece.getBlocks());
}

function checkBlocks(shapesAndBlocks) {
    let shapes = _.head(shapesAndBlocks);
    let blocks = _.last(shapesAndBlocks);
    return _.every(_.zip(shapes, blocks), shapeAndBlock => _.head(shapeAndBlock).equals(_.last(shapeAndBlock)));
}