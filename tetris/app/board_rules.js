let _ = require('lodash');
let collisionDetector = require('./collision_detector');

function canMoveDown(piece, pieces, numberOfRows) {
    return collisionDetector.checkHasNotReachedBottom(piece, numberOfRows)
        && collisionDetector.checkNoBottomCollision(piece.getBlocks(), toBlocks(pieces));
}

function toBlocks(pieces) {
    return _.flatMap(pieces, piece => piece.getBlocks());
}

function canAddNewPiece(newPiece, pieces) {
    return collisionDetector.checkNoBottomCollision(newPiece.getBlocks(), toBlocks(pieces));
}

function getCompletedLines(pieces, numberOfColumns) {
    return _.map(_.groupBy(toBlocks(pieces), block => block.getRow()), (value, key) => [key, _.size(value)])
        .filter(arr => arr[1] === numberOfColumns)
        .map(arr => +arr[0]);
}

module.exports = {
    canMoveDown: canMoveDown,
    canAddNewPiece: canAddNewPiece,
    getCompletedLines: getCompletedLines
};