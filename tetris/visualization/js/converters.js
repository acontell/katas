function MatrixConverter(defaultValue) {
    let matrix;

    function filledMatrix(numberOfRows, numberOfColumns) {
        return new Array(numberOfRows).fill(defaultValue).map(_ => new Array(numberOfColumns).fill(defaultValue));
    }

    function addToMatrix(piece) {
        getBlocksInRange(piece.getBlocks(), matrix).forEach(block => matrix[block.getRow()][block.getColumn()] = piece.getId());
    }

    function getBlocksInRange(blocks) {
        return blocks.filter(block => matrix[block.getRow()] && matrix[block.getRow()][block.getColumn()]);
    }

    this.convert = board => {
        matrix = filledMatrix(board.getNumberOfRows(), board.getNumberOfColumns());
        board.getStuckPieces().concat(board.getActivePiece()).forEach(piece => addToMatrix(piece));
        return matrix;
    }
}

module.exports = MatrixConverter;