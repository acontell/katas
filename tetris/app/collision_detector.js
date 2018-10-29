let _ = require('lodash');
let Block = require('../app/block');

function checkHasNotReachedBottom(piece, bottom) {
    return getNextRow(piece.getLowestBlock()) <= bottom;
}

function getNextRow(block) {
    return block.getRow() + 1;
}

function checkNoBottomCollision(blocks, allBlocks) {
    return _.every(blocks, block => !isPositionTaken(new Block(getNextRow(block), block.getColumn()), allBlocks));
}

function isPositionTaken(block, blocks) {
    return _.some(blocks, aBlock => block.equals(aBlock));
}

module.exports = {
    checkHasNotReachedBottom: checkHasNotReachedBottom,
    checkNoBottomCollision: checkNoBottomCollision
};