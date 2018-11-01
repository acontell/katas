const _ = require('lodash');
const expect = require('chai').expect;
const pieceFactory = require('../app/piece_factory');
const Block = require('../app/block');
const initialBlock = new Block(6, 6);
const piece = pieceFactory.getRandomPiece(initialBlock);

describe('As the game', () => {
    describe('In order to add complexity to the Game', () => {
        it('should have pieces of 4 blocks', () => {
            expect(checkBlocksSize(piece)).to.be.true;
        });
        it('should have pieces of contiguous blocks', () => {
            expect(checkContiguousBlocks(piece)).to.be.true;
        });
        it('should have 7 specific shapes', () => {
            testAllPiecesRotation(0);
        });
        it('should rotate 90 degrees clockwise', () => {
            //testPieceRotation(0, 1);
            //testPieceRotation(1, 1);
            //testPieceRotation(2, 1);
            //testPieceRotation(3, 1);
            //testPieceRotation(4, 1);
            //testPieceRotation(5, 1);
            //testPieceRotation(6, 1);
        });
        it('should rotate 180 degrees rotated twice', () => {
            //testPieceRotation(0, 2);
            //testPieceRotation(1, 2);
            //testPieceRotation(2, 2);
            //testPieceRotation(3, 2);
            //testPieceRotation(4, 2);
            //testPieceRotation(5, 2);
            //testPieceRotation(6, 2);
        });
        it('should rotate 180 degrees rotated thrice', () => {
            //testPieceRotation(0, 3);
            //testPieceRotation(1, 3);
            //testPieceRotation(2, 3);
            //testPieceRotation(3, 3);
            //testPieceRotation(4, 3);
            //testPieceRotation(5, 3);
            //testPieceRotation(6, 3);
        });
        it('should recover initial state when rotated four times', () => {
            //testPieceRotation(0, 4);
            //testPieceRotation(1, 4);
            //testPieceRotation(2, 4);
            //testPieceRotation(3, 4);
            //testPieceRotation(4, 4);
            //testPieceRotation(5, 4);
            //testPieceRotation(6, 4);
        });
        it('should go back to rotation one when rotated five times', () => {
            //testPieceRotation(0, 5);
            //testPieceRotation(1, 4);
            //testPieceRotation(2, 4);
            //testPieceRotation(3, 4);
            //testPieceRotation(4, 4);
            //testPieceRotation(5, 4);
            //testPieceRotation(6, 4);
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
    return aBlock => {
        let row = block.getRow();
        let column = block.getColumn();
        let row1 = aBlock.getRow();
        let column1 = aBlock.getColumn();
        return column1 + 1 === column
            || column1 - 1 === column
            || row1 + 1 === row
            || row1 - 1 === row;
    };
}

function testAllPiecesRotation(nTimes) {
    _.range(pieceFactory.getNumberAvailablePieces()).forEach(idx => testPieceRotation(idx, nTimes));
}

function testPieceRotation(pieceId, nRotations) {
    let piece = pieceFactory.getPiece(pieceId, initialBlock);
    rotatePiece(piece, nRotations);
    assertPieceHasRotatedWell(piece, nRotations);
}

function rotatePiece(piece, nRotations) {
    _.range(nRotations).forEach(_ => piece.rotate());
}

function assertPieceHasRotatedWell(piece, nRotations) {
    //console.log("EXPECTED:", expectedRotations[piece.getId()][nRotations].map(block => block.row + "," + block.column));
    //console.log("ACTUAL:", piece.getBlocks().map(block => block.getRow() + "," + block.getColumn()));
    piece.getBlocks().forEach((block, idx) => {
        expect(block.getRow()).to.equal(expectedRotations[piece.getId()][nRotations][idx].row);
        expect(block.getColumn()).to.equal(expectedRotations[piece.getId()][nRotations][idx].column);
    });
}

const expectedRotations = {
    // Vertical shape
    0: {
        0: [{row: 6, column: 6}, {row: 5, column: 6}, {row: 4, column: 6}, {row: 3, column: 6}],
        1: [{row: 6, column: 6}, {row: 6, column: 5}, {row: 6, column: 4}, {row: 6, column: 3}],
        2: [{row: 6, column: 6}, {row: 5, column: 6}, {row: 4, column: 6}, {row: 3, column: 6}],
        3: [{row: 6, column: 6}, {row: 6, column: 5}, {row: 6, column: 4}, {row: 6, column: 3}]
    },
    // Square Shape
    1: {
        0: [{row: 6, column: 6}, {row: 6, column: 7}, {row: 5, column: 6}, {row: 5, column: 7}],
        // TODO : FROM HERE ON
        1: [{row: 6, column: 6}, {row: 6, column: 5}, {row: 6, column: 4}, {row: 6, column: 3}],
        2: [{row: 6, column: 6}, {row: 5, column: 6}, {row: 4, column: 6}, {row: 3, column: 6}],
        3: [{row: 6, column: 6}, {row: 6, column: 5}, {row: 6, column: 4}, {row: 6, column: 3}]
    },
    // half H shape
    2: {
        0: [{row: 6, column: 6}, {row: 5, column: 6}, {row: 5, column: 5}, {row: 4, column: 6}],
        // TODO : FROM HERE ON
        1: [{row: 6, column: 6}, {row: 6, column: 5}, {row: 6, column: 4}, {row: 6, column: 3}],
        2: [{row: 6, column: 6}, {row: 5, column: 6}, {row: 4, column: 6}, {row: 3, column: 6}],
        3: [{row: 6, column: 6}, {row: 6, column: 5}, {row: 6, column: 4}, {row: 6, column: 3}]
    },
    // kind of S
    3: {
        0: [{row: 6, column: 6}, {row: 5, column: 6}, {row: 5, column: 5}, {row: 4, column: 5}],
        // TODO : FROM HERE ON
        1: [{row: 6, column: 6}, {row: 6, column: 5}, {row: 6, column: 4}, {row: 6, column: 3}],
        2: [{row: 6, column: 6}, {row: 5, column: 6}, {row: 4, column: 6}, {row: 3, column: 6}],
        3: [{row: 6, column: 6}, {row: 6, column: 5}, {row: 6, column: 4}, {row: 6, column: 3}]
    },
    // inverted kind of S
    4: {
        0: [{row: 6, column: 6}, {row: 5, column: 6}, {row: 5, column: 7}, {row: 4, column: 7}],
        // TODO : FROM HERE ON
        1: [{row: 6, column: 6}, {row: 6, column: 5}, {row: 6, column: 4}, {row: 6, column: 3}],
        2: [{row: 6, column: 6}, {row: 5, column: 6}, {row: 4, column: 6}, {row: 3, column: 6}],
        3: [{row: 6, column: 6}, {row: 6, column: 5}, {row: 6, column: 4}, {row: 6, column: 3}]
    },
    // L
    5: {
        0: [{row: 6, column: 6}, {row: 6, column: 7}, {row: 5, column: 6}, {row: 4, column: 6}],
        // TODO : FROM HERE ON
        1: [{row: 6, column: 6}, {row: 6, column: 5}, {row: 6, column: 4}, {row: 6, column: 3}],
        2: [{row: 6, column: 6}, {row: 5, column: 6}, {row: 4, column: 6}, {row: 3, column: 6}],
        3: [{row: 6, column: 6}, {row: 6, column: 5}, {row: 6, column: 4}, {row: 6, column: 3}]
    },
    // inverted L
    6: {
        0: [{row: 6, column: 6}, {row: 6, column: 7}, {row: 5, column: 7}, {row: 4, column: 7}],
        1: [{row: 6, column: 6}, {row: 6, column: 5}, {row: 6, column: 4}, {row: 6, column: 3}],
        2: [{row: 6, column: 6}, {row: 5, column: 6}, {row: 4, column: 6}, {row: 3, column: 6}],
        3: [{row: 6, column: 6}, {row: 6, column: 5}, {row: 6, column: 4}, {row: 6, column: 3}]
    }
};