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

/*
- GroupBy all blocks by row => if size === numberOfColumns, mark them for deletion.
- Iterate over pieces => if all its blocks are mark for deletion, mark it for deletion.
- filter pieces and return only those that are not marked for deletion.
 */
function clearLines(pieces, numberOfColumns) {
    markBlocksForDeletion(toBlocks(pieces), numberOfColumns);
    updatePieces(pieces);
    return _.filter(pieces, piece => !piece.isEmpty());
}

function markBlocksForDeletion(allBlocks, numberOfColumns) {
    let blocksToDelete = _.filter(_.groupBy(allBlocks, block => block.getRow()), blocks => _.size(blocks) === numberOfColumns);
    return _.flatten(blocksToDelete).map(block => block.markForDeletion());
}

function updatePieces(pieces) {
    _.forEach(pieces, piece => piece.removeDeletedBlocks());
}

module.exports = {
    isContiguous: isContiguous,
    canMoveDown: canMoveDown,
    clearLines: clearLines
};