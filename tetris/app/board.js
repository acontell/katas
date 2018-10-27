const _ = require('lodash');
const Block = require('./block');
const pieceFactory = require('./piece_factory');

function Board(numberOfRows, numberOfColumns) {
    const topCenterBlock = new Block(Math.ceil(numberOfRows / 2), Math.ceil(numberOfColumns / 2));
    let pieces = [];
    let activePiece;

    this.getNumberOfRows = () => numberOfRows;
    this.getNumberOfColumns = () => numberOfColumns;
    this.getNumberOfPieces = () => _.size(pieces);
    this.addNewPiece = () => {
        let newPiece = pieceFactory.getRandomPiece(topCenterBlock);
        pieces = pieces.concat(newPiece);
        activePiece = newPiece;
    };
    this.getActivePiece = () => activePiece;
    this.getTopCenterBlock = () => topCenterBlock;
}

module.exports = Board;