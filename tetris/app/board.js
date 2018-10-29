const _ = require('lodash');
const Block = require('./block');

function Board(numberOfRows, numberOfColumns, pieceFactory, rules) {
    const top = Math.ceil(numberOfRows / 2);
    const center = Math.ceil(numberOfColumns / 2);
    let pieces = [];
    let activePiece;

    this.getNumberOfRows = () => numberOfRows;
    this.getNumberOfColumns = () => numberOfColumns;
    this.getNumberOfPieces = () => _.size(pieces);
    this.addNewPiece = () => {
        activePiece = this.buildNewPiece();
        pieces = pieces.concat(activePiece);
    };
    this.buildNewPiece = () => pieceFactory.getRandomPiece(this.getTopCenterBlock());
    this.getActivePiece = () => activePiece;
    this.getTopCenterBlock = () => new Block(top, center);
    this.moveActivePiece = () => activePiece.moveDown();
    this.canMoveActivePiece = () => rules.canMoveDown(activePiece, pieces, numberOfRows);
    this.isBoardFull = () => !rules.canMoveDown(this.buildNewPiece(), pieces, numberOfRows);
    this.clearLines = () => pieces = rules.clearLines(pieces, numberOfColumns);
}

module.exports = Board;