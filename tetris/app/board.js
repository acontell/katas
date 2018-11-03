const _ = require('lodash');
const Block = require('./block');

function Board(numberOfRows, numberOfColumns, pieceFactory, boardRules) {
    const top = 0;
    const center = Math.ceil(numberOfColumns / 2);
    let pieces = [];
    let activePiece;

    this.getNumberOfRows = () => numberOfRows;
    this.getNumberOfColumns = () => numberOfColumns;
    this.getNumberOfPieces = () => _.size(pieces);
    this.addNewPiece = () => activePiece = this.buildNewPiece();
    this.addPieces = morePieces => pieces = pieces.concat(morePieces);
    this.getPieces = () => pieces;
    this.blockActivePiece = () => pieces = pieces.concat(activePiece);
    this.buildNewPiece = () => pieceFactory.getRandomPiece(this.getTopCenterBlock());
    this.getActivePiece = () => activePiece;
    this.getTopCenterBlock = () => new Block(top, center);
    this.moveActivePiece = () => activePiece.moveDown();
    this.moveRight = () => activePiece.moveRight();
    this.moveLeft = () => activePiece.moveLeft();
    this.canMoveActivePiece = () => boardRules.canMoveDown(activePiece, pieces, numberOfRows);
    this.canMoveRight = () => boardRules.canMoveRight(activePiece, pieces, numberOfColumns);
    this.canMoveLeft = () => boardRules.canMoveLeft(activePiece, pieces);
    this.isBoardFull = () => !boardRules.canAddPiece(this.buildNewPiece(), pieces);
    this.isBoardEmpty = () => _.isEmpty(pieces);
    this.getCompletedLines = () => boardRules.getCompletedLines(pieces, numberOfColumns);
    this.canRotatePiece = () => boardRules.canRotate(activePiece, pieces, numberOfRows, numberOfColumns);
    this.rotateActivePiece = () => activePiece = pieceFactory.getRotatedPiece(activePiece);
    this.clearLines = completedRows => pieces.forEach(piece => piece.clearBlocks(completedRows));
    this.removeEmptyPieces = () => pieces = pieces.filter(piece => !piece.isEmpty());
    this.collapsePieces = completedRows => pieces.forEach(piece => piece.collapse(completedRows));
}

module.exports = Board;