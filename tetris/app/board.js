function Board(numberOfColumns, numberOfRows) {
    const topCenter = 0;
    let pieces = [];
    let activePiece;

    this.getNumberOfColumns = () => numberOfColumns;
    this.getNumberOfRows = () => numberOfRows;
    this.getNumberOfPieces = () => pieces.length;
    this.addNewPiece = () => {
        let newPiece = new Piece;
        pieces = pieces.concat(newPiece);
        activePiece = newPiece;
    };
}

module.exports = Board;