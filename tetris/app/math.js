let _ = require('lodash');
let Block = require('../app/block');

function isContiguous(block, block1) {
    let row = block.getRow();
    let column = block.getColumn();
    let row1 = block1.getRow();
    let column1 = block1.getColumn();
    return column1 + 1 === column
        || column1 - 1 === column
        || row1 + 1 === row
        || row1 - 1 === row;
}

function canMoveDown(piece, pieces, numberOfRows) {
    return checkHasNoReachedBottom(piece, numberOfRows) && checkBlocksNoBottomCollision(piece.getBlocks(), toBlocks(removePiece(piece, pieces)));
}

function removePiece(piece, pieces) {
    return _.filter(pieces, aPiece => aPiece !== piece);
}

function toBlocks(pieces) {
    return _.flatMap(pieces, piece => piece.getBlocks());
}

function checkHasNoReachedBottom(piece, bottom) {
    return getNextRow(piece.getLowestBlock()) <= bottom;
}
function getNextRow(block) {
    return block.getRow() + 1;
}

function checkBlocksNoBottomCollision(blocks, allBlocks) {
    return _.every(blocks, block => !isPositionTaken(new Block(getNextRow(block), block.getColumn()), allBlocks));
}

function isPositionTaken(block, blocks) {
    return _.some(blocks, aBlock => block.equals(aBlock));
}

module.exports = {
    isContiguous: isContiguous,
    canMoveDown: canMoveDown
};