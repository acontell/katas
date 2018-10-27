const _ = require('lodash');
const Block = require('./block');
const pieceFactory = require('./piece_factory');

function Board(numberOfRows, numberOfColumns) {
    const top = Math.ceil(numberOfRows / 2);
    const center = Math.ceil(numberOfColumns / 2);
    let pieces = [];
    let activePiece;

    this.getNumberOfRows = () => numberOfRows;
    this.getNumberOfColumns = () => numberOfColumns;
    this.getNumberOfPieces = () => _.size(pieces);
    this.addNewPiece = () => {
        activePiece = pieceFactory.getRandomPiece(new Block(top, center));
        pieces = pieces.concat(activePiece);
    };
    this.getActivePiece = () => activePiece;
    this.getTopCenterBlock = () => new Block(top, center);
    this.moveDown = () => activePiece.moveDown();
}

module.exports = Board;