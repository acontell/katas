const _ = require('lodash');
const Block = require('./block');

function Board(numberOfRows, numberOfColumns, pieceFactory, boardRules) {
    const top = 0;
    const center = Math.ceil(numberOfColumns / 2);
    let stuckPieces = [];
    let activePiece;

    this.getNumberOfRows = () => numberOfRows;
    this.getNumberOfColumns = () => numberOfColumns;
    this.addStuckPieces = morePieces => stuckPieces = stuckPieces.concat(morePieces);
    this.getStuckPieces = () => stuckPieces;
    this.newActivePiece = () => activePiece = this.buildNewPiece();
    this.blockActivePiece = () => stuckPieces = stuckPieces.concat(activePiece);
    this.buildNewPiece = () => pieceFactory.getRandomPiece(this.getTopCenterBlock());
    this.getActivePiece = () => activePiece;
    this.getTopCenterBlock = () => new Block(top, center);
    this.moveActivePiece = () => activePiece.moveDown();
    this.moveRight = () => activePiece.moveRight();
    this.moveLeft = () => activePiece.moveLeft();
    this.canMoveActivePiece = () => boardRules.canMoveDown(activePiece, stuckPieces, numberOfRows);
    this.canMoveRight = () => boardRules.canMoveRight(activePiece, stuckPieces, numberOfColumns);
    this.canMoveLeft = () => boardRules.canMoveLeft(activePiece, stuckPieces);
    this.isBoardFull = () => !boardRules.canAddPiece(this.buildNewPiece(), stuckPieces);
    this.isBoardEmpty = () => _.isEmpty(stuckPieces);
    this.getCompletedLines = () => boardRules.getCompletedLines(stuckPieces, numberOfColumns);
    this.canRotatePiece = () => boardRules.canRotate(activePiece, stuckPieces, numberOfRows, numberOfColumns);
    this.rotateActivePiece = () => activePiece = pieceFactory.getRotatedPiece(activePiece);
    this.clearLines = completedRows => stuckPieces.forEach(piece => piece.clearBlocks(completedRows));
    this.removeEmptyPieces = () => stuckPieces = stuckPieces.filter(piece => !piece.isEmpty());
    this.collapsePieces = completedRows => stuckPieces.forEach(piece => piece.collapse(completedRows));
}

module.exports = Board;