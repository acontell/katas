const _ = require('lodash');
const Block = require('./block');

function Board(numberOfRows, numberOfColumns, pieceFactory, math) {
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
    this.moveDown = () => activePiece.moveDown();
    this.canMoveDown = () => math.canMoveDown(activePiece, pieces, numberOfRows);
    this.isBoardFull = () => !math.canMoveDown(this.buildNewPiece(), pieces, numberOfRows);
    this.clearLines = () => pieces = math.clearLines(pieces, numberOfColumns);
}

module.exports = Board;