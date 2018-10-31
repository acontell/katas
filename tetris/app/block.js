function Block(row, column) {
    this.getRow = () => row;
    this.getColumn = () => column;
    this.moveDown = () => ++row;
    this.equals = aBlock => row === aBlock.getRow() && column === aBlock.getColumn();
}

module.exports = Block;