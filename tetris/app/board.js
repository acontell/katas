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
    this.canMoveDownActivePiece = () => boardRules.canMoveDown(activePiece, stuckPieces, numberOfRows);
    this.moveDownActivePiece = () => activePiece.moveDown();
    this.canMoveRightActivePiece = () => boardRules.canMoveRight(activePiece, stuckPieces, numberOfColumns);
    this.moveRightActivePiece = () => activePiece.moveRight();
    this.canMoveLeftActivePiece = () => boardRules.canMoveLeft(activePiece, stuckPieces);
    this.moveLeftActivePiece = () => activePiece.moveLeft();
    this.canRotateActivePiece = () => boardRules.canRotate(activePiece, stuckPieces, numberOfRows, numberOfColumns);
    this.rotateActivePiece = () => activePiece = pieceFactory.getRotatedPiece(activePiece);
    this.isFull = () => !boardRules.canAddPiece(this.buildNewPiece(), stuckPieces);
    this.isEmpty = () => _.isEmpty(stuckPieces);
    this.getCompletedLines = () => boardRules.getCompletedLines(stuckPieces, numberOfColumns);
    this.clearLines = completedRows => stuckPieces.forEach(piece => piece.clearBlocks(completedRows));
    this.removeEmptyPieces = () => stuckPieces = stuckPieces.filter(piece => !piece.isEmpty());
    this.collapsePieces = completedRows => stuckPieces.forEach(piece => piece.collapseBlocks(completedRows));
}

module.exports = Board;