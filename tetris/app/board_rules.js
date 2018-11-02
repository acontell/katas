let _ = require('lodash');
let Block = require('../app/block');
let collisionDetector = require('./collision_detector');
let pieceFactory = require('./piece_factory');

function canMoveDown(piece, pieces, numberOfRows) {
    return collisionDetector.checkHasNotReachedBottom(getNextRow(piece.getLowestBlock()), numberOfRows)
        && collisionDetector.checkNoCollision(blocksNextRow(piece.getBlocks()), toBlocks(pieces));
}

function getNextRow(block) {
    return block.getRow() + 1;
}

function blocksNextRow(blocks) {
    return blocks.map(block => new Block(getNextRow(block), block.getColumn()));
}

function toBlocks(pieces) {
    return _.flatMap(pieces, piece => piece.getBlocks());
}

function canAddNewPiece(newPiece, pieces) {
    return collisionDetector.checkNoCollision(newPiece.getBlocks(), toBlocks(pieces));
}

function getCompletedLines(pieces, numberOfColumns) {
    return _.map(_.groupBy(toBlocks(pieces), block => block.getRow()), (value, key) => [key, _.size(value)])
        .filter(arr => arr[1] === numberOfColumns)
        .map(arr => +arr[0]);
}

function canRotate(piece, pieces, numberOfRows, numberOfColumns) {
    let rotatedPiece = pieceFactory.getRotatedPiece(piece);
    return collisionDetector.checkHasNotReachedBottom(rotatedPiece.getLowestBlock().getRow(), numberOfRows)
        && collisionDetector.checkHasNotReachedRightSide(rotatedPiece.getFarRightBlock().getColumn(), numberOfColumns)
        && collisionDetector.checkHasNotReachedLeftSide(rotatedPiece.getFarLeftBlock().getColumn())
        && collisionDetector.checkNoCollision(rotatedPiece.getBlocks(), toBlocks(pieces));
}

module.exports = {
    canMoveDown: canMoveDown,
    canAddNewPiece: canAddNewPiece,
    getCompletedLines: getCompletedLines,
    canRotate: canRotate
};