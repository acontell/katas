let _ = require('lodash');
let Block = require('../app/block');
let collisionDetector = require('./collision_detector');
let pieceFactory = require('./piece_factory');

function canMoveDown(piece, pieces, numberOfRows) {
    return collisionDetector.checkHasNotReachedBottom(getNextRow(piece.getLowestBlock()), numberOfRows)
        && collisionDetector.checkNoCollision(blocksToNextRow(piece.getBlocks()), toBlocks(pieces));
}

function getNextRow(block) {
    return block.getRow() + 1;
}

function blocksToNextRow(blocks) {
    return blocks.map(block => new Block(getNextRow(block), block.getColumn()));
}

function toBlocks(pieces) {
    return _.flatMap(pieces, piece => piece.getBlocks());
}

function canMoveRight(piece, pieces, numberOfColumns) {
    return collisionDetector.checkHasNotReachedRightSide(getNextColumn(piece.getFarRightBlock()), numberOfColumns)
        && collisionDetector.checkNoCollision(blocksToNextColumn(piece.getBlocks()), toBlocks(pieces));
}

function getNextColumn(block) {
    return block.getColumn() + 1;
}

function blocksToNextColumn(blocks) {
    return blocks.map(block => new Block(block.getRow(), getNextColumn(block)));
}

function canMoveLeft(piece, pieces) {
    return collisionDetector.checkHasNotReachedLeftSide(getPreviousColumn(piece.getLowestBlock()))
        && collisionDetector.checkNoCollision(blocksToPreviousColumn(piece.getBlocks()), toBlocks(pieces));
}

function getPreviousColumn(block) {
    return block.getColumn() - 1;
}

function blocksToPreviousColumn(blocks) {
    return blocks.map(block => new Block(block.getRow(), getPreviousColumn(block)));
}

function getCompletedLines(pieces, numberOfColumns) {
    return getBlocksPerRow(pieces)
        .filter(rowAndNumberOfBlocks => _.last(rowAndNumberOfBlocks) === numberOfColumns)
        .map(rowAndNumberOfBlocks => Number(_.head(rowAndNumberOfBlocks)));
}

function getBlocksPerRow(pieces) {
    return _.map(_.groupBy(toBlocks(pieces), block => block.getRow()), (blocks, row) => [row, _.size(blocks)])
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
    canMoveRight: canMoveRight,
    canMoveLeft: canMoveLeft,
    canRotate: canRotate,
    canAddPiece: (newPiece, pieces) => collisionDetector.checkNoCollision(newPiece.getBlocks(), toBlocks(pieces)),
    getCompletedLines: getCompletedLines
};