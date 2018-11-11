function MatrixConverter(defaultValue) {
    let matrix;

    this.convert = board => {
        matrix = getFilledMatrix(board.getNumberOfRows(), board.getNumberOfColumns());
        addPiecesToMatrix(board.getAllPieces());
        return matrix;
    };

    function getFilledMatrix(numberOfRows, numberOfColumns) {
        return new Array(numberOfRows).fill(defaultValue).map(_ => new Array(numberOfColumns).fill(defaultValue));
    }

    function addPiecesToMatrix(pieces) {
        pieces.forEach(addPieceToMatrix);
    }

    function addPieceToMatrix(piece) {
        getBlocksInRange(piece.getBlocks(), matrix).forEach(block => matrix[block.getRow()][block.getColumn()] = piece.getId());
    }

    function getBlocksInRange(blocks) {
        return blocks.filter(block => matrix[block.getRow()] && matrix[block.getRow()][block.getColumn()]);
    }
}

module.exports = MatrixConverter;