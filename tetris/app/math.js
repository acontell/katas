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

module.exports = {
    isContiguous: isContiguous
};