const _ = require('lodash');
const Block = require('./block');

function Board(numberOfRows, numberOfColumns, pieceFactory, boardRules) {
    const top = Math.ceil(numberOfRows / 2);
    const center = Math.ceil(numberOfColumns / 2);
    let pieces = [];
    let activePiece;

    this.getNumberOfRows = () => numberOfRows;
    this.getNumberOfColumns = () => numberOfColumns;
    this.getNumberOfPieces = () => _.size(pieces);
    this.addNewPiece = () => activePiece = this.buildNewPiece();
    this.activePieceToBoard = () => pieces = pieces.concat(activePiece);
    this.buildNewPiece = () => pieceFactory.getRandomPiece(this.getTopCenterBlock());
    this.getActivePiece = () => activePiece;
    this.getTopCenterBlock = () => new Block(top, center);
    this.moveActivePiece = () => activePiece.moveDown();
    this.canMoveActivePiece = () => boardRules.canMoveDown(activePiece, pieces, numberOfRows);
    this.isBoardFull = () => boardRules.canAddNewPiece(this.buildNewPiece(), pieces);
    this.clearLines = () => pieces = boardRules.clearLines(pieces, numberOfColumns);
}

module.exports = Board;