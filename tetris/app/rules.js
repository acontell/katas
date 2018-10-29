let _ = require('lodash');
let collisionDetector = require('./collision_detector');

function canMoveDown(piece, pieces, numberOfRows) {
    return collisionDetector.checkHasNotReachedBottom(piece, numberOfRows)
        && collisionDetector.checkNoBottomCollision(piece.getBlocks(), toBlocks(removePiece(piece, pieces)));
}

function toBlocks(pieces) {
    return _.flatMap(pieces, piece => piece.getBlocks());
}

function removePiece(piece, pieces) {
    return _.filter(pieces, aPiece => aPiece !== piece);
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
    canMoveDown: canMoveDown,
    clearLines: clearLines
};