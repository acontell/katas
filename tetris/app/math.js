function isContiguous(block, block1) {
    let row = block.getRow();
    let column = block.getColumn();
    let row1 = block1.getRow();
    let column1 = block1.getColumn();
    return column1 + 1 === column
        || column1 - 1 === column
        || row1 + 1 === row
        || row1 - 1 === row;
}

// TODO
function canMoveDown(piece, pieces, numberOfRows) {
    // lowerRow
    //let lowerRow = piece.getInitialBlock().getRow();
    //let columnsWithLowerRow = _.filter(piece.getBlocks(), block => block.getRow() === lowerRow)
      //  .map(block => block.getColumn());
    //return _.filter(pieces, blockedPiece => piece !== blockedPiece)
    //    .filter(blockedPiece => )
    return true;
}

module.exports = {
    isContiguous: isContiguous,
    canMoveDown: canMoveDown
};