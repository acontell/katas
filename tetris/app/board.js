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
    this.addPieces = morePieces => pieces = pieces.concat(morePieces);
    this.blockActivePiece = () => pieces = pieces.concat(activePiece);
    this.buildNewPiece = () => pieceFactory.getRandomPiece(this.getTopCenterBlock());
    this.getActivePiece = () => activePiece;
    this.getTopCenterBlock = () => new Block(top, center);
    this.moveActivePiece = () => activePiece.moveDown();
    this.canMoveActivePiece = () => boardRules.canMoveDown(activePiece, pieces, numberOfRows);
    this.isBoardFull = () => boardRules.canAddNewPiece(this.buildNewPiece(), pieces);
    this.getCompletedLines = () => boardRules.getCompletedLines(pieces, numberOfColumns);
    this.updateBoard = completedLines => pieces = clearAndCollapse(completedLines);

    function clearAndCollapse(lines) {
        return pieces
            .map(piece => piece.clearBlocks(lines))
            .filter(piece => !piece.isEmpty())
            .map(piece => piece.collapse(lines));
    }
}

module.exports = Board;