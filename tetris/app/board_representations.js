function filledMatrix(numberOfRows, numberOfColumns, defaultValue) {
    return new Array(numberOfRows).fill(defaultValue).map(_ => new Array(numberOfColumns).fill(defaultValue));
}

function addToMatrix(piece, matrix) {
    getBlocksInRange(piece.getBlocks(), matrix).forEach(block => matrix[block.getRow()][block.getColumn()] = piece.getId());
}

function getBlocksInRange(blocks, matrix) {
    return blocks.filter(block => matrix[block.getRow()] && matrix[block.getRow()][block.getColumn()]);
}

module.exports = {
    toMatrix: (pieces, numberOfRows, numberOfColumns, defaultValue) => {
        let matrix = filledMatrix(numberOfRows, numberOfColumns, defaultValue);
        pieces.forEach(piece => addToMatrix(piece, matrix));
        return matrix;
    }
};